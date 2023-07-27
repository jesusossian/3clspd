push!(LOAD_PATH, "modules/")
# push!(DEPOT_PATH, JULIA_DEPOT_PATH)
using Pkg
#Pkg.activate(".")
# Pkg.instantiate()
# Pkg.build()

using JuMP
using Gurobi
#using CPLEX

import Data
import Parameters
import Formulations
#import Heuristics
import DPHeuristicsCC
#import LagrangianRelaxation
import RelaxAndFix

# Read the parameters from command line
params = Parameters.readInputParameters(ARGS)

# Read instance data
inst = Data.readData(params.instName,params)

if params.form == "std"
	Formulations.standardFormulation(inst, params)
elseif params.form == "mc"
	Formulations.multicommodityFormulation(inst, params)
elseif params.form == "echelon"
	Formulations.echelonStockFormulation(inst, params)
elseif params.form == "esn"
	Formulations.echelonStockFormulationESN(inst, params)
elseif params.form == "estp"
	Formulations.echelonStockFormulationESTP(inst, params)
elseif params.form == "esls"
	Formulations.echelonStockFormulationESLS(inst, params)
elseif params.form == "rounding"
	Heuristics.partialmulticommodityRounding(inst, params)
elseif (params.form == "randdpheurCC" || params.form == "randdpheurCCfo" ) && params.capacity != 0
	DPHeuristicsCC.RandomizedDPHeuristicBottomUpCC(inst,params)
elseif (params.form == "randdpheurCCA" || params.form == "randdpheurCCAfo" ) && params.capacity != 0
	DPHeuristicsCC.RandomizedDPHeuristicBottomUpCCAdaptative(inst,params)
elseif params.form == "lr" && params.capacity != 0
	LagrangianRelaxation.lagrangianRelaxation(inst,params)
elseif (params.form == "rf" || params.form == "rffo" ) && params.capacity != 0
	RelaxAndFix.RelaxAndFixEchelonStockFormulation3(inst,params)
elseif (params.form == "rfesls" || params.form == "rffoesls" ) && params.capacity != 0
	RelaxAndFix.RelaxAndFixEchelonStockESLS(inst,params)
#elseif params.form == "cuttingplane"
#	BranchAndCut.cuttingplanestandardFormulation(inst, params)
end
