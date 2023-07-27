# 3clspd

Three-level lot sizing and replenishment problem

for run one instances with default configuration

julia threelspd.jl

for run one instance with formulation solver

julia thresslspd.jl --inst XXXX --form XXXX --solver XXXX

There a .sh to run set of instances ./executar.sh

for run one instance with formulation, solver and capacity

julia thresslspd.jl --inst XXXX --form XXXX --solver XXXX --capacity XXXX

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

Instances group

Uncapacitated instances
P = { 1 }

N = {50, 100, 200}

T = {15, 30}

W = {5, 10, 15, 20}

V = {SD_SF, SD_DF, DD_SF, DD_DF}

instances = {1,2, \ldots, 5}

style = {bal, unbal}

Number instances uncapacitated = 3 * 2 * 4 * 4 * 5 * 2 = 960 instances

Capacitated instances
factor cap = {1.50, 1.75, 2.00}

Variations of capacity = 3^3 variations = 27 variations

Number instances capacitated = 960 * 27 = 25920 instances

%%%%%%%%%%%%%%%%%%%

P = { 1 }

N = {50, 100, 200}

T = {15, 30}

W = {5, 10, 20}

V = {DD_SF}

instances = {1,2, \ldots, 3}

style = {bal, unbal}

%%%%%%%%%%%%%%%%%%%%%%%%%%

Julia => 1.6.2

DataStructures => v"0.18.9"

Gurobi => v"0.8.1"

MathOptInterface => v"0.9.22"

JuMP => v"0.21.8"

MathOptInterface => v"0.9.22" // instalado pelo jump

DataStructures => v"0.18.13" // instalado pelo jump 

Pkg.add(Pkg.PackageSpec(name="XXX",version="YYY"))

Pkg.add(Pkg.PackageSpec(name="JuMP",version="0.21.8"))

Pkg.add(Pkg.PackageSpec(name="Gurobi",version="0.8.1"))
