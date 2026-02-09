# Production & Logistics Simulator

C++ program that plans and simulates production and transportation in a network of cities.
The system considers capacity limits, production recipes, timing constraints, and resource dependencies.

## Overview
The simulator determines whether a given production goal can be achieved and, if so, produces
a feasible schedule of manufacturing and transportation tasks over time.

It combines:
- graph-based routing,
- multi-level production recipes,
- capacity-aware scheduling,
- and day-by-day simulation.

## Key Features
- Transportation network with travel time and capacity constraints
- Manufacturing recipes with required input materials and production time
- Automatic detection of infeasible scenarios (missing resources, cycles, unreachable cities)
- Load-aware pathfinding with congestion penalties
- Parallel production and shipment planning
- Priority-based scheduling using product criticality

## Input
The program reads three text files:
- **Transport network** (cities, edges, travel time, capacity)
- **Production rules** (recipes, required inputs, manufacturing time)
- **Goal definition** (required products per city)

Several predefined scenarios are included (parallel flows, bottlenecks, cycles, impossible cases).

## Output
The simulator prints:
- daily production and shipping tasks
- inventory status per city
- clear error messages if the goal cannot be achieved

Example:
DAY 2
MAKE A: 2X
SHIP A->B 1X
STATUS: A: 1X B: 1Y

## Technical Notes
- Implemented in modern C++ (STL containers, priority queues, recursion)
- Custom parsers for structured input
- Separation of planning and simulation phases
- Event-driven execution model

## Build & Run
Compile with a C++17-compatible compiler:

```bash
g++ -std=c++17 src/main.cpp -O2 -o simulator
./simulator
