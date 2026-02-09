#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <map>
#include <sstream>
#include <set>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <climits>
#include <queue>
#include <cmath>
using namespace std;

using Qty = long long;
static const Qty INF = (Qty)4e18;

// ==================== ADATSTRUKTÚRÁK ====================

struct Edge {
    string a, b;
    int dur;      // utazási idő
    Qty cap;      // kapacitás
    int period() const { return (dur > 0) ? dur : 1; }
};

struct RecipeItem {
    string prod;
    Qty qty;
};

struct Recipe {
    string city;        // Hol gyártják
    string makes;       // Mit gyártanak
    int dur;            // Gyártási idő
    vector<RecipeItem> need; // Alapanyagok
};

struct Task {
    int day;
    string type;   // "SHIP" vagy "MAKE"
    string from, to;
    string city;
    string product;
    Qty qty;
};

struct Event {
    int day;
    string city;
    string product;
    Qty qty;
    bool operator>(Event const& o) const { return day > o.day; }
};

// ==================== SEGÉD PARSER ====================

class ParseUtil {
public:
    static pair<Qty,string> parseToken(const string& tok) {
        Qty q = 0;
        int i = 0;
        while(i < (int)tok.size() && isdigit((unsigned char)tok[i])) {
            q = q * 10 + (tok[i]-'0');
            i++;
        }
        if(q == 0) q = 1;
        return {q, tok.substr(i)};
    }

    static vector<RecipeItem> parseNeeds(const string& s) {
        vector<RecipeItem> res;
        if(s == "-" || s.empty()) return res;
        string cur;
        for(char c: s) {
            if(c == ',') {
                if(!cur.empty()) {
                    auto [q,p] = parseToken(cur);
                    if(!p.empty()) res.push_back({p,q});
                    cur.clear();
                }
            } else if(!isspace((unsigned char)c)) {
                cur.push_back(c);
            }
        }
        if(!cur.empty()) {
            auto [q,p] = parseToken(cur);
            if(!p.empty()) res.push_back({p,q});
        }
        return res;
    }
};

// ==================== BEMENETI MODELL ====================

struct InputData {
    vector<Edge> edges;
    unordered_map<string, vector<Recipe>> recipeByProduct;
    unordered_map<string, unordered_map<string, Qty>> initialStock;
    unordered_map<string, unordered_map<string, Qty>> goal;
    unordered_set<string> cities;
};

// ==================== PARSER OSZTÁLY ====================

class Parser {
public:
    InputData load(const string& menetrend, const string& gyartas, const string& cel) {
        InputData in;
        readMenetrend(menetrend, in);
        readGyartas(gyartas, in);
        readCel(cel, in);
        return in;
    }

private:
    static void readMenetrend(const string& file, InputData& in) {
        ifstream f(file);
        if(!f) throw runtime_error("Cannot open " + file);

        string h;
        f >> h >> h >> h >> h >> h; // Header

        while(true) {
            string id;
            f >> id;
            if(!f) break;
            if(id == "END") break;

            Edge e;
            f >> e.a >> e.b >> e.dur >> e.cap;
            // Ha a sor "-" jeleket tartalmaz, a >> beolvassa őket, a gráfban izoláltak lesznek.
            in.edges.push_back(e);
            in.cities.insert(e.a);
            in.cities.insert(e.b);
        }
    }

    static void readGyartas(const string& file, InputData& in) {
        ifstream f(file);
        if(!f) throw runtime_error("Cannot open " + file);

        string line; getline(f, line); // Header skip

        while(true) {
            string city;
            f >> city;
            if(!f) break;
            if(city == "END") break;

            string termelStr, gyartStr, kellStr, durStr;
            f >> termelStr >> gyartStr >> kellStr >> durStr;

            in.cities.insert(city);

            if (termelStr != "-") {
                auto items = ParseUtil::parseNeeds(termelStr);
                for (const auto& item : items) {
                    in.initialStock[city][item.prod] += item.qty;
                }
            }

            if (gyartStr != "-") {
                Recipe r;
                r.city = city;
                r.makes = gyartStr;
                if (durStr != "-") {
                    try { r.dur = stoi(durStr); } catch(...) { r.dur = 0; }
                } else {
                    r.dur = 0;
                }
                r.need = ParseUtil::parseNeeds(kellStr);
                in.recipeByProduct[r.makes].push_back(r);
            }
        }
    }

    static void readCel(const string& file, InputData& in) {
        ifstream f(file);
        if(!f) throw runtime_error("Cannot open " + file);

        string h1, h2; f >> h1 >> h2;

        while(true) {
            string city;
            f >> city;
            if(!f) break;
            if(city == "END") break;

            string needStr;
            f >> needStr;
            auto items = ParseUtil::parseNeeds(needStr);
            for(auto& item : items) {
                in.goal[city][item.prod] += item.qty;
            }
            in.cities.insert(city);
        }
    }
};

// ==================== HÁLÓZAT ÉS ÚTVONALKERESÉS ====================

class Network {
public:
    explicit Network(const InputData& in) {
        build(in);
    }

    const unordered_map<string, unordered_map<string, Edge>>& edges() const { return edgeMap; }

    // ÚTVONALKERESŐ
    // figyeli a "torlódás-büntetést" (penalty) is tie-breakerként.
    pair<int, vector<string>> findPathWithLoad(
        const string& src,
        const string& dst,
        int startDay,
        const unordered_map<string, unordered_map<string, Qty>>& currentLoad
        ) const {
        // Távolság helyett most párt tárolunk: {érkezési_idő, büntetőpont}
        // A büntetőpont a várakozási ciklusok száma.
        unordered_map<string, pair<int, int>> dist;
        unordered_map<string, string> parent;

        for (const auto& c : cities) dist[c] = {INT_MAX, INT_MAX};
        dist[src] = {startDay, 0};

        // A priority queue az érkezési idő szerint rendez (kisebb elől)
        struct Node { int t; int penalty; string v; };
        struct Cmp {
            bool operator()(const Node& a, const Node& b) const {
                if (a.t != b.t) return a.t > b.t;
                return a.penalty > b.penalty; // Ha idő egyenlő, a kisebb büntetésű legyen elől
            }
        };
        priority_queue<Node, vector<Node>, Cmp> pq;

        pq.push({startDay, 0, src});

        while (!pq.empty()) {
            auto [t, currentPenalty, v] = pq.top(); pq.pop();

            // Ha rosszabb állapotban vagyunk, mint a már megtalált legjobb, skip
            if (t > dist[v].first || (t == dist[v].first && currentPenalty > dist[v].second)) continue;
            if (v == dst) break;

            auto it = edgeMap.find(v);
            if (it == edgeMap.end()) continue;

            for (const auto& kv : it->second) {
                const string& to = kv.first;
                const Edge& e = kv.second;

                // Terhelés lekérdezése
                Qty load = 0;
                auto itU = currentLoad.find(v);
                if (itU != currentLoad.end()) {
                    auto itV = itU->second.find(to);
                    if (itV != itU->second.end()) load = itV->second;
                }

                // Számítások
                int waitCycles = (int)(load / e.cap);
                int congestionPenalty = waitCycles * e.period(); // Időbeli csúszás

                int period = e.period();
                int scheduleWait = (period - (t % period)) % period; // Menetrendi várakozás

                int arrive = t + scheduleWait + e.dur + congestionPenalty;

                // ÚJ: Büntetőpont növelése a várakozási ciklusokkal
                // Ez segít dönteni, ha az érkezési idő egyenlő: a kerülőúton waitCycles=0
                int newPenalty = currentPenalty + waitCycles;

                // FRISSÍTÉSI FELTÉTEL:
                // 1. Ha hamarabb érünk oda -> UPDATE
                // 2. Ha ugyanakkor érünk oda, DE kisebb büntetéssel (kevesebb dugó) -> UPDATE
                bool better = false;
                if (arrive < dist[to].first) {
                    better = true;
                } else if (arrive == dist[to].first && newPenalty < dist[to].second) {
                    better = true;
                }

                if (better) {
                    dist[to] = {arrive, newPenalty};
                    parent[to] = v;
                    pq.push({arrive, newPenalty, to});
                }
            }
        }

        if (dist[dst].first == INT_MAX) return {INT_MAX, {}};

        vector<string> path;
        for (string cur = dst;; cur = parent[cur]) {
            path.push_back(cur);
            if (cur == src) break;
        }
        reverse(path.begin(), path.end());
        return {dist[dst].first, path};
    }

private:
    unordered_set<string> cities;
    unordered_map<string, unordered_map<string, Edge>> edgeMap;

    void build(const InputData& in) {
        cities = in.cities;
        for(const auto& e : in.edges) {
            edgeMap[e.a][e.b] = e;
            edgeMap[e.b][e.a] = e;
        }
    }
};

// ==================== KRITIKUSSÁG ====================

class Criticality {
public:
    unordered_map<string, long long> score;

    Criticality(const InputData& in, const Network& net) : input(in) {
        buildReverseDeps();
        computeScores();
    }

    long long get(const string& p) const {
        auto it = score.find(p);
        return (it == score.end()) ? 0LL : it->second;
    }

private:
    const InputData& input;
    unordered_map<string, vector<string>> usedBy;

    void buildReverseDeps() {
        for (const auto& kv : input.recipeByProduct) {
            for (const auto& r : kv.second) {
                for (const auto& ing : r.need) {
                    usedBy[ing.prod].push_back(r.makes);
                }
            }
        }
    }

    long long dfsScore(const string& p, unordered_map<string, int>& visited) {
        if (visited[p] == 2) return score[p];
        if (visited[p] == 1) return 0;

        visited[p] = 1;

        long long best = 0;
        for(const auto& g : input.goal) {
            if(g.second.count(p)) best = max(best, 10LL);
        }

        auto it = usedBy.find(p);
        if(it != usedBy.end()) {
            for(const string& parentProd : it->second) {
                long long s = dfsScore(parentProd, visited);
                best = max(best, s + 10);
            }
        }

        visited[p] = 2;
        score[p] = best;
        return best;
    }

    void computeScores() {
        unordered_set<string> allProds;

        // 1. Kezdeti készletek hozzáadása
        for(const auto& kv : input.initialStock) {
            for(const auto& p : kv.second) {
                allProds.insert(p.first);
            }
        }

        // 2. Receptek feldolgozása
        for(const auto& kv : input.recipeByProduct) {
            allProds.insert(kv.first); // A termék amit gyártunk

            for (const auto& r : kv.second) {
                for (const auto& n : r.need) {
                    allProds.insert(n.prod); // Az alapanyagok
                }
            }
        }

        // 3. Célok feldolgozása
        for(const auto& kv : input.goal) {
            for(const auto& p : kv.second) {
                allProds.insert(p.first);
            }
        }

        // 4. Pontszámítás
        unordered_map<string, int> visited;
        for(const auto& p : allProds) {
            if(visited[p] == 0) dfsScore(p, visited);
        }
    }
};

// ==================== TERVEZŐ  ====================

class Planner {
public:
    struct Plan {
        unordered_map<string, unordered_map<string, Qty>> makeNeed;
        unordered_map<string, unordered_map<string, unordered_map<string, Qty>>> edgeDemand;
    };

    Planner(const InputData& in, const Network& net)
        : input(in), network(net) {
        currentStock = input.initialStock;
    }

    Plan buildPlan() {
        Plan plan;
        set<pair<string,string>> recursionStack;

        for(const auto& cityGoal : input.goal) {
            const string& targetCity = cityGoal.first;
            for(const auto& prodGoal : cityGoal.second) {
                requireAtCity(plan, prodGoal.first, prodGoal.second, targetCity, recursionStack);
            }
        }
        return plan;
    }

private:
    const InputData& input;
    const Network& network;
    unordered_map<string, unordered_map<string, Qty>> currentStock;
    unordered_map<string, unordered_map<string, Qty>> tempEdgeLoad;

    void addShipment(Plan& plan, const string& from, const string& to, const string& prod, Qty qty) {
        if (from == to || qty <= 0) return;

        for(int i = 0; i < qty; ++i) {
            auto [arrive, path] = network.findPathWithLoad(from, to, 0, tempEdgeLoad);
            if(path.empty()) throw runtime_error("Nincs ut a(z) " + from + " es " + to + " kozott!");

            for(size_t k = 0; k < path.size() - 1; ++k) {
                string u = path[k];
                string v = path[k+1];
                plan.edgeDemand[u][v][prod]++;
                tempEdgeLoad[u][v]++;
            }
        }
    }

    void requireAtCity(Plan& plan, const string& product, Qty qty, const string& targetCity, set<pair<string,string>>& recursionStack) {
        if (qty <= 0) return;

        if(recursionStack.count({targetCity, product})) {
            throw runtime_error("KOROKOS FUGGOSEG (Vagy hianyzo alapanyag): " + product + " itt: " + targetCity);
        }
        recursionStack.insert({targetCity, product});

        // Jelöljük, ha találtunk forrást, csak nem értük el
        bool foundButUnreachable = false;

        // 1. Készlet keresés
        struct StockSource {
            string city;
            int arrivalTime;
            Qty available;
        };
        vector<StockSource> sources;

        for (auto& cs : currentStock) {
            const string& sCity = cs.first;
            if (cs.second.count(product) && cs.second[product] > 0) {
                auto [arr, path] = network.findPathWithLoad(sCity, targetCity, 0, tempEdgeLoad);

                // Ha van út, VAGY már ott vagyunk
                if (!path.empty() || sCity == targetCity) {
                    int time = (sCity == targetCity) ? 0 : arr;
                    sources.push_back({sCity, time, cs.second[product]});
                } else {
                    // Ha van készlet, de nincs út
                    foundButUnreachable = true;
                }
            }
        }

        sort(sources.begin(), sources.end(), [](const auto& a, const auto& b){
            return a.arrivalTime < b.arrivalTime;
        });

        Qty needed = qty;
        for (const auto& src : sources) {
            if (needed <= 0) break;
            Qty take = min(needed, src.available);
            currentStock[src.city][product] -= take;

            if (src.city != targetCity) {
                addShipment(plan, src.city, targetCity, product, take);
            }
            needed -= take;
        }

        if (needed <= 0) {
            recursionStack.erase({targetCity, product});
            return;
        }

        // 2. Gyártás (Párhuzamosítás támogatása)
        auto itR = input.recipeByProduct.find(product);
        if (itR != input.recipeByProduct.end()) {
            const vector<Recipe>& recipes = itR->second;

            // Ha csak egy helyen lehet gyártani, vagy kevés kell, használjuk az elsőt
            if (recipes.empty()) { /* Hiba kezelése lentebb */ }
            else {
                // --- C opció: költség-alapú elosztás ---

                struct Cand {
                    int idx;
                    long long cost;
                    double w;
                };

                vector<Cand> cands;
                cands.reserve(recipes.size());

                // 1) receptenként költség becslése: gyártási idő + szállítási (érkezési) idő
                for (int i = 0; i < (int)recipes.size(); ++i) {
                    const Recipe& r = recipes[i];

                    int shipTime = 0;
                    if (r.city != targetCity) {
                        auto [arr, path] = network.findPathWithLoad(r.city, targetCity, 0, tempEdgeLoad);
                        if (path.empty()) continue; // nincs út -> ezt a gyárat kihagyjuk
                        shipTime = arr; // startDay=0 esetén ez kvázi idő
                    }

                    long long cost = (long long)r.dur + (long long)shipTime;
                    double w = 1.0 / (double)(cost + 1); // kisebb cost => nagyobb súly
                    cands.push_back({i, cost, w});
                }

                // Ha van recept, de egyik gyárból sincs út a célhoz -> lehetetlen
                if (cands.empty()) {
                    recursionStack.erase({targetCity, product});
                    throw runtime_error(
                        "Lehetetlen: van recept a(z) " + product +
                        " termekhez, de egyik gyartohely sem erheto el a celhoz: " + targetCity
                        );
                }

                // 2) arányos kiosztás a súlyok alapján
                double sumW = 0.0;
                for (const auto& c : cands) sumW += c.w;

                vector<Qty> assign(recipes.size(), 0);
                Qty given = 0;

                for (const auto& c : cands) {
                    Qty share = (Qty)((c.w / sumW) * (double)needed); // floor implicit
                    assign[c.idx] = share;
                    given += share;
                }

                // 3) maradékot a legolcsóbb recept(ek) kapják
                sort(cands.begin(), cands.end(), [](const Cand& a, const Cand& b) {
                    if (a.cost != b.cost) return a.cost < b.cost;
                    return a.idx < b.idx;
                });

                Qty rem = needed - given;
                for (Qty k = 0; k < rem; ++k) {
                    assign[cands[k % cands.size()].idx]++; // körkörösen a legjobbak között
                }

                // 4) végrehajtás
                for (int i = 0; i < (int)recipes.size(); ++i) {
                    Qty myShare = assign[i];
                    if (myShare <= 0) continue;

                    const Recipe& r = recipes[i];
                    plan.makeNeed[r.city][product] += myShare;

                    if (r.city != targetCity) {
                        addShipment(plan, r.city, targetCity, product, myShare);
                    }

                    for (const auto& ing : r.need) {
                        requireAtCity(plan, ing.prod, ing.qty * myShare, r.city, recursionStack);
                    }
                }

                recursionStack.erase({targetCity, product});
                return;
            }
        }

        //hibaüzenet
        recursionStack.erase({targetCity, product});

        if (foundButUnreachable) {
            throw runtime_error("Lehetetlen: Van forras a(z) " + product + " termekbol, de a celallomas nem elerheto (nincs ut).");
        } else {
            throw runtime_error("Lehetetlen: hianyzo forras es recept a(z) " + product + " termekhez.");
        }
    }
};

// ==================== SZIMULÁTOR ====================

class Simulator {
public:
    Simulator(const InputData& in, const Network& net, Planner::Plan plan, const Criticality& crit)
        : input(in), network(net), criticality(crit),
        makeRem(std::move(plan.makeNeed)), edgeRem(std::move(plan.edgeDemand))
    {
        initInventory();
    }

    vector<Task> run(int safetyLimit = 5000) {
        int day = 0;
        while(day <= safetyLimit) {
            applyEvents(day);
            startProductions(day);
            startShipments(day);
            saveStatus(day);
            if (allDone()) break;
            day++;
        }
        if (day > safetyLimit) throw runtime_error("Szimulacios idotullepes");
        sortTasks();
        return tasks;
    }

    const auto& getStatusMap() const { return statusByDay; }

private:
    const InputData& input;
    const Network& network;
    const Criticality& criticality;

    unordered_map<string, unordered_map<string, Qty>> inv;
    unordered_map<string, unordered_map<string, Qty>> makeRem;
    unordered_map<string, unordered_map<string, unordered_map<string, Qty>>> edgeRem;
    priority_queue<Event, vector<Event>, greater<Event>> events;
    vector<Task> tasks;
    unordered_map<int, unordered_map<string, unordered_map<string, Qty>>> statusByDay;

    void initInventory() {
        for(const auto& kv : input.initialStock) {
            for(const auto& p : kv.second) {
                inv[kv.first][p.first] = p.second;
            }
        }
    }

    void applyEvents(int day) {
        while(!events.empty() && events.top().day == day) {
            auto e = events.top(); events.pop();
            inv[e.city][e.product] += e.qty;
        }
    }

    void startProductions(int day) {
        // Végigmegyünk azokon a városokon, ahol a terv szerint gyártani kell valamit
        for(auto& [city, prodMap] : makeRem) {

            // 1. Összeszedjük, mit kell itt gyártani
            vector<string> prods;
            for(auto& [p, q] : prodMap) {
                if(q > 0) prods.push_back(p);
            }

            // 2. Sorba rendezzük fontosság (Criticality) szerint
            sort(prods.begin(), prods.end(), [&](const string& a, const string& b){
                return criticality.get(a) > criticality.get(b);
            });

            // 3. Megpróbáljuk elindítani a gyártást
            for(const string& prod : prods) {
                Qty& needed = prodMap[prod];
                if(needed <= 0) continue;

                // Mivel egy termékhez (pl. "XX") több recept is tartozhat (A-ban és C-ben is gyártják),
                // meg kell keresnünk azt a receptet, ami az AKTUÁLIS városhoz (city) tartozik.

                const vector<Recipe>& recipes = input.recipeByProduct.at(prod);
                const Recipe* rPtr = nullptr;

                for (const auto& cand : recipes) {
                    if (cand.city == city) {
                        rPtr = &cand;
                        break;
                    }
                }

                // Biztonsági ellenőrzés: ha nincs recept ehhez a városhoz, átugorjuk
                if (!rPtr) continue;
                const Recipe& r = *rPtr;
                // --- JAVÍTOTT RÉSZ VÉGE ---

                // Megnézzük, mennyi alapanyagunk van a raktárban (inv)
                Qty canMake = INF;
                for(const auto& ing : r.need) {
                    Qty have = inv[city][ing.prod];
                    if(ing.qty > 0) {
                        canMake = min(canMake, have / ing.qty);
                    }
                }

                // Ha tudunk gyártani legalább 1-et
                if(canMake > 0) {
                    // Annyit indítunk, amennyi kell, vagy amennyire van anyag (amelyik kevesebb)
                    Qty makeNow = min(needed, canMake);

                    // Levonjuk az alapanyagokat
                    for(const auto& ing : r.need) {
                        inv[city][ing.prod] -= ing.qty * makeNow;
                    }

                    // Esemény bejegyzése (mikor lesz kész?)
                    events.push({day + r.dur, city, prod, makeNow});

                    // Task naplózása a kimenethez
                    tasks.push_back({day, "MAKE", "", "", city, prod, makeNow});

                    // Csökkentjük a hátralévő gyártási igényt
                    needed -= makeNow;
                }
            }
        }
    }

    void startShipments(int day) {
        for(const auto& u_entry : network.edges()) {
            string u = u_entry.first;
            for(const auto& v_entry : u_entry.second) {
                string v = v_entry.first;
                const Edge& e = v_entry.second;
                if(day % e.period() != 0) continue;
                if(edgeRem[u].find(v) == edgeRem[u].end()) continue;

                auto& demands = edgeRem[u][v];
                Qty cap = e.cap;
                vector<string> toShip;
                for(auto& [p, q] : demands) if(q > 0) toShip.push_back(p);

                sort(toShip.begin(), toShip.end(), [&](const string& a, const string& b){
                    long long ca = criticality.get(a);
                    long long cb = criticality.get(b);
                    if(ca != cb) return ca > cb;
                    return a < b;
                });

                for(const string& p : toShip) {
                    if(cap <= 0) break;
                    Qty need = demands[p];
                    Qty have = inv[u][p];
                    Qty sending = min({need, have, cap});
                    if(sending > 0) {
                        inv[u][p] -= sending;
                        demands[p] -= sending;
                        cap -= sending;
                        events.push({day + e.dur, v, p, sending});
                        tasks.push_back({day, "SHIP", u, v, "", p, sending});
                    }
                }
            }
        }
    }

    bool allDone() const {
        for(auto& c : makeRem) for(auto& p : c.second) if(p.second > 0) return false;
        for(auto& u : edgeRem) for(auto& v : u.second) for(auto& p : v.second) if(p.second > 0) return false;

        for(const auto& gCity : input.goal) {
            string city = gCity.first;
            if(inv.find(city) == inv.end()) return false;
            const auto& invCity = inv.at(city);
            for(const auto& gProd : gCity.second) {
                if(invCity.find(gProd.first) == invCity.end() || invCity.at(gProd.first) < gProd.second) return false;
            }
        }
        return true;
    }

    void saveStatus(int day) { statusByDay[day] = inv; }
    void sortTasks() {
        sort(tasks.begin(), tasks.end(), [](const Task& a, const Task& b){
            if(a.day != b.day) return a.day < b.day;
            if(a.type != b.type) return a.type < b.type;
            if(a.type == "SHIP") {
                if(a.from != b.from) return a.from < b.from;
                if(a.to != b.to) return a.to < b.to;
            } else {
                if(a.city != b.city) return a.city < b.city;
            }
            return a.product < b.product;
        });
    }
};

// ==================== MAIN ====================

static void printDayBlocks(const vector<Task>& tasks, const unordered_map<int, unordered_map<string, unordered_map<string, Qty>>>& statusByDay, ostream& os) {
    map<int, vector<Task>> byDay;
    int lastDay = -1;
    for (const auto& t : tasks) { byDay[t.day].push_back(t); lastDay = max(lastDay, t.day); }
    for (const auto& kv : statusByDay) lastDay = max(lastDay, kv.first);

    auto printStatus = [&](int day) {
        os << "STATUS:";
        auto itDay = statusByDay.find(day);
        if (itDay == statusByDay.end()) { os << " (nincs)\n"; return; }
        vector<string> cities;
        for (const auto& kv : itDay->second) cities.push_back(kv.first);
        sort(cities.begin(), cities.end());
        bool any = false;
        for (const auto& city : cities) {
            const auto& mp = itDay->second.at(city);
            vector<pair<string, Qty>> prods;
            for (const auto& kv : mp) if (kv.second > 0 && kv.second < INF/2) prods.push_back(kv);
            if (prods.empty()) continue;
            sort(prods.begin(), prods.end());
            os << " " << city << ":";
            bool first = true;
            for (auto& [p,q] : prods) { if (!first) os << ","; os << " " << q << p; first = false; }
            any = true;
        }
        if (!any) os << " (empty)";
        os << "\n";
    };

    for (int day = 0; day <= lastDay; ++day) {
        os << "DAY " << day << "\n";
        if (byDay.count(day)) {
            auto& vec = byDay[day];
            sort(vec.begin(), vec.end(), [](const Task& a, const Task& b){ if(a.type != b.type) return a.type < b.type; return a.product < b.product; });
            for (const auto& t : vec) {
                if (t.type == "SHIP") os << "SHIP " << t.from << "->" << t.to << " " << t.qty << t.product << "\n";
                else os << "MAKE " << t.city << ": " << t.qty << t.product << "\n";
            }
        }
        printStatus(day);
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    try {
        Parser parser;
        // fájlnevek a teszteléshez
        //         InputData input = parser.load("Hello_Word_m.txt", "Hello_World_g.txt", "Hello_World_c.txt");
        //          InputData input = parser.load("atszallas_m.txt", "atszallas_g.txt", "atszallas_c.txt");
        //          InputData input = parser.load("hello_gyartas_m.txt", "hello_gyartas_g.txt", "hello_gyartas_c.txt");
        //          InputData input = parser.load("hello_utemezes_m.txt", "hello_utemezes_g.txt", "hello_utemezes_c.txt");
        //          InputData input = parser.load("ketfele_m.txt", "ketfele_g.txt", "ketfele_c.txt");
        //          InputData input = parser.load("ketfelol_m.txt", "ketfelol_g.txt", "ketfelol_c.txt");
        //          InputData input = parser.load("utemezes2_m.txt", "utemezes2_g.txt", "utemezes2_c.txt");
        //          InputData input = parser.load("cserebere_m.txt", "cserebere_g.txt", "cserebere_c.txt");
        //          InputData input = parser.load("parallel_m.txt", "parallel_g.txt", "parallel_c.txt");
                  InputData input = parser.load("kombinalas_m.txt", "kombinalas_g.txt", "kombinalas_c.txt");
        //          InputData input = parser.load("korbe_m.txt", "korbe_g.txt", "korbe_c.txt");
        //          InputData input = parser.load("gyartasi_lanc_m.txt", "gyartasi_lanc_g.txt", "gyartasi_lanc_c.txt");
        //          InputData input = parser.load("van_kesz_is_m.txt", "van_kesz_is_g.txt", "van_kesz_is_c.txt");
        //          InputData input = parser.load("konvertibilis_a_m.txt", "konvertibilis_a_g.txt", "konvertibilis_a_c.txt");
        //          InputData input = parser.load("konvertibilis_b_m.txt", "konvertibilis_b_g.txt", "konvertibilis_b_c.txt");
        //          InputData input = parser.load("csapda_m.txt", "csapda_g.txt", "csapda_c.txt");
        //          InputData input = parser.load("hello_lehetetlen_m.txt", "hello_lehetetlen_g.txt", "hello_lehetetlen_c.txt");
        //          InputData input = parser.load("diszjunkt_m.txt", "diszjunkt_g.txt", "diszjunkt_c.txt");
        //          InputData input = parser.load("lehetetlen_3_m.txt", "lehetetlen_3_g.txt", "lehetetlen_3_c.txt");
        //          InputData input = parser.load("extra_1_m.txt", "extra_1_g.txt", "extra_1_c.txt");
        //          InputData input = parser.load("extra_2_m.txt", "extra_2_g.txt", "extra_2_c.txt");
        //          InputData input = parser.load("extra_3_m.txt", "extra_3_g.txt", "extra_3_c.txt");

        Network net(input);
        Criticality crit(input, net);
        Planner planner(input, net);
        auto plan = planner.buildPlan();
        Simulator sim(input, net, std::move(plan), crit);
        auto tasks = sim.run();
        printDayBlocks(tasks, sim.getStatusMap(), cout);

    } catch(const exception& e) {
        cerr << "HIBA: " << e.what() << "\n";
        return 1;
    }
    return 0;
}

