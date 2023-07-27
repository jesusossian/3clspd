module RelaxAndFix

using JuMP
using Gurobi
#using CPLEX
using Data
using Parameters

using FixAndOptimize

export FixAndOptimizeStandardFormulation

function RelaxAndFixStandardFormulation(inst::InstanceData, params,paramsrf)
	println("Running RelaxAndFix.RelaxAndFixStandardFormulation")

	nbsubprob = ceil(inst.NT/paramsrf.fixsizerf)
	maxtimesubprob = paramsrf.maxtimerf/nbsubprob


	if params.solver == "Gurobi"
		env = Gurobi.Env()
		model = Model(with_optimizer(Gurobi.Optimizer,TimeLimit=maxtimesubprob,MIPGap=paramsrf.tolgaprf))
	#elseif params.solver == "Cplex"
	#	model = Model(solver=CplexSolver(CPX_PARAM_TILIM=maxtimesubprob,CPX_PARAM_EPGAP=paramsrf.tolgaprf))
	else
		println("No solver selected")
		return 0
	end

	### Defining variables ###
	@variable(model,0 <= x[i=1:inst.NI,p=1:inst.NP,t=1:inst.NT] <= sum(inst.d[i,p1,k] for p1=1:inst.NP, k=t:inst.NT))
	@variable(model, y[i=1:inst.NI,p=1:inst.NP,t=1:inst.NT], Bin)
	@variable(model,0 <= s[i=1:inst.NI,p=1:inst.NP,t=1:inst.NT] <= sum(inst.d[i,p1,k] for p1=1:inst.NP, k=t:inst.NT))
	@variable(model,0 <= r[i=1:inst.NI,p1=1:inst.NP, p2=1:inst.NP, t=1:inst.NT; p1!= p2] <= sum(inst.d[i,p1,k] for p1=1:inst.NP, k=t:inst.NT))

	### Objective function ###
	@objective(model, Min,
		sum(x[i,p,t]*inst.pc[i,p]+ y[i,p,t]*inst.sc[i,p] + s[i,p,t]*inst.hc[i,p] for i=1:inst.NI, p=1:inst.NP, t=1:inst.NT)
		+ sum(r[i,p1,p2,t]*inst.tc[p1,p2] for i=1:inst.NI, p1=1:inst.NP, p2=1:inst.NP, t=1:inst.NT if p1!=p2)
	)
	#@objective(model, Min, sum(x[i,p,t]*pc[i,p]+y[i,p,t]*sc[i,p] for i=1:NI,p=1:NP,t=1:NT)  )

	### Balance constraints ###
	@constraint(model,
		balance1[i=1:inst.NI,p=1:inst.NP],
		x[i,p,1] + sum(r[i,p1,p,1] - r[i,p,p1,1]  for p1 in 1:inst.NP if p1 != p) == inst.d[i,p,1] + s[i,p,1]
	)
	@constraint(model,
		balance[i=1:inst.NI,p=1:inst.NP,t=2:inst.NT],
		s[i,p,t-1] + x[i,p,t] + sum(r[i,p1,p,t] - r[i,p,p1,t] for p1 in 1:inst.NP if p1 != p) == inst.d[i,p,t] + s[i,p,t]
	)

	### Setup constraints ###
	#@constraint(model, setup[i=1:inst.NI,p=1:inst.NP,t=1:inst.NT], x[i,p,t] <= sum(inst.d[i,p1,k] for p1 in 1:inst.NP, k in t:inst.NT)*y[i,p,t])
	@constraint(model,
		setup[i=1:inst.NI,p=1:inst.NP,t=1:inst.NT],
		x[i,p,t] <= min(floor((inst.C[p]-inst.st[i,p])/inst.pt[i,p]) ,sum(inst.d[i,p1,k] for p1 in 1:inst.NP, k in t:inst.NT))*y[i,p,t]
	)

	### Capacity constraints ###
	@constraint(model,
		capacity[p=1:inst.NP,t=1:inst.NT],
		sum(inst.st[i,p]*y[i,p,t]+inst.pt[i,p]*x[i,p,t] for i in 1:inst.NI) <= inst.C[p]
	)

	## set k and kprime according to the initial parameters
	k = paramsrf.horsizerf
	kprime = paramsrf.fixsizerf

	elapsedtime = 0
	alpha = 1
	beta = min(alpha + k - 1, inst.NT)
	while(beta <= inst.NT)
		println("RELAX AND FIX [$(alpha), $(beta)]")
		t1 = time_ns()
		if(alpha > 1)
			#println("\n\n Fixed variables")
			### Define fixed variables ###
			for t in 1:alpha-1, i in 1:inst.NI, p in 1:inst.NP
				if value(y[i,p,t]) >= 0.9
					set_lower_bound(y[i,p,t],1.0)
				else
					set_upper_bound(y[i,p,t],0.0)
				end
			end
		end

		#println("\n\n Integer variables")
		### Define integer variables ###
		for t in alpha:beta, i in 1:inst.NI, p in 1:inst.NP
			set_binary(y[i,p,t])
		end

		for t in beta+1:inst.NT, i in 1:inst.NI, p in 1:inst.NP
			unset_binary(y[i,p,t])
		end

		status = optimize!(model)

		alpha = alpha + kprime
		if beta == inst.NT
			beta = inst.NT+1
		else
			beta = min(alpha + k -1,inst.NT)
		end
		t2 = time_ns()
		elapsedtime += (t2-t1)/1.0e9
		println("Elapsed ",elapsedtime)

	end

	bestsol = objective_value(model)
	ysol = ones(Int,inst.NI,inst.NP,inst.NT)
	for i in 1:inst.NI, p in 1:inst.NP, t in 1:inst.NT
		if value(y[i,p,t]) >= 0.99
			ysol[i,p,t] = 1
		else
			ysol[i,p,t] = 0
		end
	end


	### Reset integrality requirements and bounds to default
	for i in 1:inst.NI, p in 1:inst.NP, t in 1:inst.NT
		set_binary(y[i,p,t])
		set_lower_bound(y[i,p,t],0.0)
	end

	return ysol, bestsol

end


function RelaxAndFixEchelonStockFormulation3(inst::InstanceData,params)

	if params.maxtimerf < 3600
		maxtimerf = params.maxtimerf
	else
		maxtimerf = 300
	end

	## set k and kprime according to the initial parameters
	kprime = params.fixsizerf
	k = kprime + params.freeintervalrf

	#Defining number of subproblems and maximum time for executing them
	nbsubprob = ceil(inst.NT/kprime)
	#maxtimesubprob = floor(maxtimefo/(2*nbsubprob)) #try to guarantee at least two passes
	maxtimesubprob = floor(maxtimerf/nbsubprob) #try to guarantee at least two passes

	println("Running RelaxAndFix.RelaxAndFixEchelonStockFormulation3")
	if params.solver == "Gurobi"
		if !(@isdefined env)
		    env = Gurobi.Env()
		end
		#println("defined env")
		#model = Model(with_optimizer(Gurobi.Optimizer,TimeLimit=maxtimesubprob,MIPGap=tolgapfo,OutputFlag=0))
		model = Model(with_optimizer(Gurobi.Optimizer,TimeLimit=maxtimesubprob,MIPGap=params.tolgaprf))
	else
		println("No solver selected")
		return 0
	end

	### Defining variables ###
	@variable(model,0 <= xp[i=1:inst.NP,t=1:inst.NT] <= Inf)
	@variable(model,0 <= xr[i=1:inst.NR,t=1:inst.NT] <= Inf)
	@variable(model,0 <= xw[i=1:inst.NW,t=1:inst.NT] <= Inf)
	@variable(model, yp[i=1:inst.NP,t=1:inst.NT], Bin)
	@variable(model, yr[i=1:inst.NR,t=1:inst.NT], Bin)
	@variable(model, yw[i=1:inst.NW,t=1:inst.NT], Bin)
	@variable(model,0 <= isp[i=1:inst.NP,t=0:inst.NT] <= Inf)
	@variable(model,0 <= isr[i=1:inst.NR,t=0:inst.NT] <= Inf)
	@variable(model,0 <= isw[i=1:inst.NW,t=0:inst.NT] <= Inf)

	#println("Defined variables")

	### Objective function ###
	@objective(model, Min,
		sum(inst.SCP[i,t]*yp[i,t] for i=1:inst.NP, t=1:inst.NT)
		+ sum(inst.SCR[i,t]*yr[i,t] for i=1:inst.NR, t=1:inst.NT)
		+ sum(inst.SCW[i,t]*yw[i,t] for i=1:inst.NW, t=1:inst.NT)
		+ sum(inst.HCP[i]*isp[i,t] for i=1:inst.NP, t=1:inst.NT)
		+ sum((inst.HCW[i]- inst.HCP[1])*isw[i,t] for i=1:inst.NW, t=1:inst.NT)
		+ sum((inst.HCR[i]- inst.HCW[inst.DeltamR[i]])*isr[i,t] for i=1:inst.NR, t=1:inst.NT)
	)

	#println("Finished objective function")

	### Setup constraints ###
	@constraint(model,
		setupP[t=1:inst.NT], xp[1,t] <= sum(inst.DP[1,k] for k in t:inst.NT)*yp[1,t]
	)
	@constraint(model,
		setupW[i=1:inst.NW, t=1:inst.NT], xw[i,t] <= sum(inst.DW[i,k] for k in t:inst.NT)*yw[i,t]
	)
	@constraint(model,
		setupR[i=1:inst.NR, t=1:inst.NT], xr[i,t] <= sum(inst.D[i,k] for k in t:inst.NT)*yr[i,t]
	)

	if params.capacity != 0.0
		@constraint(model,
			capP[t=1:inst.NT], xp[1,t] <= min(inst.C[t],sum(inst.DP[1,k] for k in t:inst.NT))*yp[1,t]
		)
	end


	if params.capacityw != 0.0
		@constraint(model,
			capW[w=1:inst.NW,t=1:inst.NT], isw[w,t] - sum(isr[inst.DeltaW[w][k],t] for k in 1:length(inst.DeltaW[w])) <= inst.CW[w,t]
		)
	end

	if params.capacityr != 0.0
		@constraint(model,
			capR[r=1:inst.NR,t=1:inst.NT], isr[r,t] <= inst.CR[r,t]
		)
	end

	#println("Finished setup constraints")

	### no initial inventory
	@constraint(model, zeroinvP[p=1:inst.NP], isp[p,0] == 0)
	@constraint(model, zeroinvW[w=1:inst.NW], isw[w,0] == 0)
	@constraint(model, zeroinvR[r=1:inst.NR], isr[r,0] == 0)

	#println("Finished zero inventory constraints")

	### Balance constraints ###
	@constraint(model,
		balanceP[i=1:inst.NP, t=1:inst.NT],
		isp[i,t-1] + xp[i,t] == inst.DP[i,t] + isp[i,t]
	)
	@constraint(model,
		balanceW[w=1:inst.NW,t=1:inst.NT],
		isw[w,t-1] + xw[w,t] == inst.DW[w,t] + isw[w,t]
	)
	@constraint(model,
		balanceR[i=1:inst.NR,t=1:inst.NT],
		isr[i,t-1] + xr[i,t] == inst.D[i,t] + isr[i,t]
	)

	# echelon stock
	@constraint(model,
		echelonP[i=1:inst.NP, t=1:inst.NT],
		isp[i,t] >= sum(isw[w,t] for w in 1:inst.NW)
	)

	@constraint(model,
		echeloW[w=1:inst.NW,t=1:inst.NT],
		isw[w,t] >= sum(isr[inst.DeltaW[w][k],t]   for k in 1:length(inst.DeltaW[w]))
	)

	### Fix and optimize ###
	println("\n #####################################")
	println("\n RELAX AND FIX")
	println("\n #####################################")

	elapsedtime = 0
	alpha = 1
	beta = min(alpha + k - 1, inst.NT)

	for t in 1:inst.NT
		unset_binary(yp[1,t])
		for w in 1:inst.NW
			unset_binary(yw[w,t])
		end
		for r in 1:inst.NR
			unset_binary(yr[r,t])
		end
	end

	while(beta <= inst.NT)
		println("RELAX AND FIX [$(alpha), $(beta)]")
		t1 = time_ns()
		if(alpha > 1)
			#println("\n\n Fixed variables")
			### Define fixed variables ###

			for t in 1:alpha-1
				if value(yp[1,t]) >= 0.9
					set_lower_bound(yp[1,t],1.0)
				else
					if params.fixonlyonesrf == 0
						set_upper_bound(yp[1,t],0.0)
					end
				end
				for w in 1:inst.NW
					if value(yw[w,t]) >= 0.9
						set_lower_bound(yw[w,t],1.0)
					else
						if params.fixonlyonesrf == 0
							set_upper_bound(yw[w,t],0.0)
						end
					end
				end
				for r in 1:inst.NR
					if value(yr[r,t]) >= 0.9
						set_lower_bound(yr[r,t],1.0)
					else
						if params.fixonlyonesrf == 0
							set_upper_bound(yr[r,t],1.0)
						end
					end
				end
			end
		end

		#println("\n\n Integer variables")
		### Define integer variables ###
		for t in alpha:beta
			set_binary(yp[1,t])
			for w in 1:inst.NW
				set_binary(yw[w,t])
			end
			for r in 1:inst.NR
				set_binary(yr[r,t])
			end
		end



		status = optimize!(model)

		alpha = alpha + kprime

		if beta == inst.NT
			beta = inst.NT+1
		else
			beta = min(alpha + k -1,inst.NT)
		end

		t2 = time_ns()
		elapsedtime += (t2-t1)/1.0e9

		println("Elapsed ",elapsedtime)

	end

	bestsol = getobjectivevalue(model)
	ypsol = ones(Int,inst.NT)
	ywsol = ones(Int,inst.NW,inst.NT)
	yrsol = ones(Int,inst.NR,inst.NT)

	SETR = Vector{Vector{Int}}(undef,inst.NR)
	for r in 1:inst.NR
		SETR[r] = []
	end

	SETW = Vector{Vector{Int}}(undef,inst.NW)
	for w in 1:inst.NW
		SETW[w] = []
	end

	SETP = Vector{Int}
	SETP = []

	for t in 1:inst.NT
		if value(yp[1,t]) >= 0.9
			ypsol[t] = 1
			push!(SETP,t)
		else
			ypsol[t] = 0
		end
		for w in 1:inst.NW
			if value(yw[w,t]) >= 0.9
				ywsol[w,t] = 1
				push!(SETW[w],t)
			else
				ywsol[w,t] = 0
			end
		end
		for r in 1:inst.NR
			if value(yr[r,t]) >= 0.9
				yrsol[r,t] = 1
				push!(SETR[r],t)
			else
				yrsol[r,t] = 0
			end
		end
	end

	if params.form == "rf"
		open("saida.txt","a") do f
			write(f,";$(params.form);$(inst.NR);$(inst.NT);$(inst.NW);$(params.capacity);$(params.balanced);$(params.fixsizerf);$(params.freeintervalrf);$(params.maxtimerf);$(bestsol);$(elapsedtime)\n")
		end
	elseif params.form == "rffo"
		open("saida.txt","a") do f
			write(f,";$(params.form);$(inst.NR);$(inst.NT);$(inst.NW);$(params.capacity);$(params.balanced);$(params.fixsizerf);$(params.freeintervalrf);$(params.maxtimerf);$(bestsol);$(elapsedtime)")
		end
	end

	if params.form == "rffo"
		bestcost = FixAndOptimize.FixAndOptimizeEchelonStockFormulation3(inst,params,SETR,SETW,SETP,bestsol,elapsedtime)
	end

	### Reset integrality requirements and bounds to default
	for t in 1:inst.NT
		set_binary(yp[1,t])
		set_lower_bound(yp[1,t],0.0)
		for w in 1:inst.NW
			set_binary(yw[w,t])
			set_lower_bound(yw[w,t],0.0)
		end
		for r in 1:inst.NR
			set_binary(yr[r,t])
			set_lower_bound(yr[r,t],0.0)
		end
	end

	return SETP,SETR,SETW,bestsol

end







function RelaxAndFixEchelonStockESLS(inst::InstanceData,params)

	if params.maxtimerf < 3600
		maxtimerf = params.maxtimerf
	else
		maxtimerf = 300
	end

	## set k and kprime according to the initial parameters
	kprime = params.fixsizerf
	k = kprime + params.freeintervalrf

	#Defining number of subproblems and maximum time for executing them
	nbsubprob = ceil(inst.NT/kprime)
	#maxtimesubprob = floor(maxtimefo/(2*nbsubprob)) #try to guarantee at least two passes
	maxtimesubprob = floor(maxtimerf/nbsubprob) #try to guarantee at least two passes

	println("Running RelaxAndFix.RelaxAndFixEchelonStockFormulation3")
	if params.solver == "Gurobi"
		if !(@isdefined env)
		    env = Gurobi.Env()
		end
		#println("defined env")
		#model = Model(with_optimizer(Gurobi.Optimizer,TimeLimit=maxtimesubprob,MIPGap=tolgapfo,OutputFlag=0))
		model = Model(with_optimizer(Gurobi.Optimizer,TimeLimit=maxtimesubprob,MIPGap=params.tolgaprf))
	else
		println("No solver selected")
		return 0
	end

	### Defining variables ###
	@variable(model,0 <= xp[i=1:inst.NP,t=1:inst.NT] <= Inf)
	@variable(model,0 <= xr[i=1:inst.NR,t=1:inst.NT] <= Inf)
	@variable(model,0 <= xw[i=1:inst.NW,t=1:inst.NT] <= Inf)
	@variable(model, yp[i=1:inst.NP,t=1:inst.NT], Bin)
	@variable(model, yr[i=1:inst.NR,t=1:inst.NT], Bin)
	@variable(model, yw[i=1:inst.NW,t=1:inst.NT], Bin)
	@variable(model,0 <= isp[i=1:inst.NP,t=0:inst.NT] <= Inf)
	@variable(model,0 <= isr[i=1:inst.NR,t=0:inst.NT] <= Inf)
	@variable(model,0 <= isw[i=1:inst.NW,t=0:inst.NT] <= Inf)

	### Objective function ###
	@objective(model, Min,
		sum(inst.SCP[i,t]*yp[i,t] for i=1:inst.NP, t=1:inst.NT)
		+ sum(inst.SCR[i,t]*yr[i,t] for i=1:inst.NR, t=1:inst.NT)
		+ sum(inst.SCW[i,t]*yw[i,t] for i=1:inst.NW, t=1:inst.NT)
		+ sum(inst.HCP[i]*isp[i,t] for i=1:inst.NP, t=1:inst.NT)
		+ sum((inst.HCW[i]- inst.HCP[1])*isw[i,t] for i=1:inst.NW, t=1:inst.NT)
		+ sum((inst.HCR[i]- inst.HCW[inst.DeltamR[i]])*isr[i,t] for i=1:inst.NR, t=1:inst.NT)
	)

	### Setup constraints ###
	@constraint(model,
		setupP[t=1:inst.NT], xp[1,t] <= sum(inst.DP[1,k] for k in t:inst.NT)*yp[1,t]
	)
	@constraint(model,
		setupW[i=1:inst.NW, t=1:inst.NT], xw[i,t] <= sum(inst.DW[i,k] for k in t:inst.NT)*yw[i,t]
	)
	@constraint(model,
		setupR[i=1:inst.NR, t=1:inst.NT], xr[i,t] <= sum(inst.D[i,k] for k in t:inst.NT)*yr[i,t]
	)

	### Capacity constraints ###
	if params.capacity != 0.0
		@constraint(model,
			capP[t=1:inst.NT], xp[1,t] <= min(inst.C[t],sum(inst.DP[1,k] for k in t:inst.NT))*yp[1,t]
		)
	end

	if params.capacityw != 0.0
		@constraint(model,
			capW[w=1:inst.NW,t=1:inst.NT], isw[w,t] - sum(isr[inst.DeltaW[w][k],t]   for k in 1:length(inst.DeltaW[w])) <= inst.CW[w,t]
		)
	end

	if params.capacityr != 0.0
		@constraint(model,
			capR[r=1:inst.NR,t=1:inst.NT], isr[r,t] <= inst.CR[r,t]
		)
	end

	### no initial inventory
	@constraint(model, zeroinvP[p=1:inst.NP], isp[p,0] == 0)
	@constraint(model, zeroinvW[w=1:inst.NW], isw[w,0] == 0)
	@constraint(model, zeroinvR[r=1:inst.NR], isr[r,0] == 0)

	### Balance constraints ###
	@constraint(model,
		balanceP[i=1:inst.NP, t=1:inst.NT],
		isp[i,t-1] + xp[i,t] == inst.DP[i,t] + isp[i,t]
	)
	@constraint(model,
		balanceW[w=1:inst.NW,t=1:inst.NT],
		isw[w,t-1] + xw[w,t] == inst.DW[w,t] + isw[w,t]
	)
	@constraint(model,
		balanceR[i=1:inst.NR,t=1:inst.NT],
		isr[i,t-1] + xr[i,t] == inst.D[i,t] + isr[i,t]
	)

	#echelon stock
	@constraint(model,
		echelonP[i=1:inst.NP, t=1:inst.NT],
		isp[i,t] >= sum(isw[w,t] for w in 1:inst.NW)
	)

	@constraint(model,
		echeloW[w=1:inst.NW,t=1:inst.NT],
		isw[w,t] >= sum(isr[inst.DeltaW[w][k],t] for k in 1:length(inst.DeltaW[w]))
	)

	#echelon stock ls inequalities
	@constraint(model,
		echelon1P[i=1:inst.NP, l=1:inst.NT, k=1:l],
		isp[i,k-1] >= sum(inst.DP[i,j] for j in k:l) - sum(inst.DP[i,j]*yp[i,u] for j=k:l, u=k:j)
	)

	@constraint(model,
		echelon1W[i=1:inst.NW, l=1:inst.NT, k=1:l],
		isw[i,k-1] >= sum(inst.DW[i,j] for j in k:l) - sum(inst.DW[i,j]*yw[i,u] for j=k:l, u=k:j)
	)

	@constraint(model,
		echelon1R[i=1:inst.NR, l=1:inst.NT, k=1:l],
		isr[i,k-1] >= sum(inst.D[i,j] for j in k:l) - sum(inst.D[i,j]*yr[i,u] for j=k:l, u=k:j)
	)

	### Fix and optimize ###
	println("\n #####################################")
	println("\n RELAX AND FIX")
	println("\n #####################################")

	elapsedtime = 0
	alpha = 1
	beta = min(alpha + k - 1, inst.NT)

	for t in 1:inst.NT
		unset_binary(yp[1,t])
		for w in 1:inst.NW
			unset_binary(yw[w,t])
		end
		for r in 1:inst.NR
			unset_binary(yr[r,t])
		end
	end

	while(beta <= inst.NT)
		println("RELAX AND FIX [$(alpha), $(beta)]")
		t1 = time_ns()
		if(alpha > 1)
			#println("\n\n Fixed variables")
			### Define fixed variables ###

			for t in 1:alpha-1
				if value(yp[1,t]) >= 0.9
					set_lower_bound(yp[1,t],1.0)
				else
					if params.fixonlyonesrf == 0
						set_upper_bound(yp[1,t],0.0)
					end
				end
				for w in 1:inst.NW
					if value(yw[w,t]) >= 0.9
						set_lower_bound(yw[w,t],1.0)
					else
						if params.fixonlyonesrf == 0
							set_upper_bound(yw[w,t],0.0)
						end
					end
				end
				for r in 1:inst.NR
					if value(yr[r,t]) >= 0.9
						set_lower_bound(yr[r,t],1.0)
					else
						if params.fixonlyonesrf == 0
							set_upper_bound(yr[r,t],1.0)
						end
					end
				end
			end
		end

		#println("\n\n Integer variables")
		### Define integer variables ###
		for t in alpha:beta
			set_binary(yp[1,t])
			for w in 1:inst.NW
				set_binary(yw[w,t])
			end
			for r in 1:inst.NR
				set_binary(yr[r,t])
			end
		end



		status = optimize!(model)

		alpha = alpha + kprime

		if beta == inst.NT
			beta = inst.NT+1
		else
			beta = min(alpha + k -1,inst.NT)
		end

		t2 = time_ns()
		elapsedtime += (t2-t1)/1.0e9

		println("Elapsed ",elapsedtime)

	end

	bestsol = getobjectivevalue(model)
	ypsol = ones(Int,inst.NT)
	ywsol = ones(Int,inst.NW,inst.NT)
	yrsol = ones(Int,inst.NR,inst.NT)

	SETR = Vector{Vector{Int}}(undef,inst.NR)
	for r in 1:inst.NR
		SETR[r] = []
	end

	SETW = Vector{Vector{Int}}(undef,inst.NW)
	for w in 1:inst.NW
		SETW[w] = []
	end

	SETP = Vector{Int}
	SETP = []

	for t in 1:inst.NT
		if value(yp[1,t]) >= 0.9
			ypsol[t] = 1
			push!(SETP,t)
		else
			ypsol[t] = 0
		end
		for w in 1:inst.NW
			if value(yw[w,t]) >= 0.9
				ywsol[w,t] = 1
				push!(SETW[w],t)
			else
				ywsol[w,t] = 0
			end
		end
		for r in 1:inst.NR
			if value(yr[r,t]) >= 0.9
				yrsol[r,t] = 1
				push!(SETR[r],t)
			else
				yrsol[r,t] = 0
			end
		end
	end

	if params.form == "rfesls"
		open("saida.txt","a") do f
			write(f,";$(params.form);$(inst.NR);$(inst.NT);$(inst.NW);$(params.capacity);$(params.balanced);$(params.fixsizerf);$(params.freeintervalrf);$(params.maxtimerf);$(bestsol);$(elapsedtime)\n")
		end
	elseif params.form == "rffoesls"
		open("saida.txt","a") do f
			write(f,";$(params.form);$(inst.NR);$(inst.NT);$(inst.NW);$(params.capacity);$(params.balanced);$(params.fixsizerf);$(params.freeintervalrf);$(params.maxtimerf);$(bestsol);$(elapsedtime)")
		end
	end

	if params.form == "rffoesls"
		bestcost = FixAndOptimize.FixAndOptimizeEchelonStockESLS(inst,params,SETR,SETW,SETP,bestsol,elapsedtime)
	end

	### Reset integrality requirements and bounds to default
	for t in 1:inst.NT
		set_binary(yp[1,t])
		set_lower_bound(yp[1,t],0.0)
		for w in 1:inst.NW
			set_binary(yw[w,t])
			set_lower_bound(yw[w,t],0.0)
		end
		for r in 1:inst.NR
			set_binary(yr[r,t])
			set_lower_bound(yr[r,t],0.0)
		end
	end

	return SETP,SETR,SETW,bestsol

end









function FixAndOptimizeInterval(inst,model,yp,yw,yr,ypsol,ywsol,yrsol,bestsol,alpha,beta)

	for t in 1:inst.NT
		if ypsol[1,t] == 1
			set_lower_bound(yp[1,t],1)
		else
			set_lower_bound(yp[1,t],0)
		end

		for w in 1:inst.NW
			if ywsol[w,t] == 1
				set_lower_bound(yw[w,t],1)
			else
				set_lower_bound(yw[w,t],0)
			end
		end

		for r in 1:inst.NR
			if yrsol[r,t] == 1
				set_lower_bound(yr[r,t],1)
			else
				set_lower_bound(yr[r,t],0)
			end
		end

	end

	for t in alpha:beta
		set_lower_bound(yp[1,t],0)
		for w in 1:inst.NW
			set_lower_bound(yw[w,t],0)
		end
		for r in 1:inst.NR
			set_lower_bound(yr[r,t],0)
		end
	end

	optimize!(model)

	if getobjectivevalue(model) < bestsol - 0.01
		if getobjectivevalue(model) < bestsol - 0.5
			proceed = true
		end
		bestsol = getobjectivevalue(model)
		for t in 1:inst.NT
			if value(yp[1,t]) >= 0.99
				ypsol[1,t] = 1
			else
				ypsol[1,t] = 0
			end
			for w in 1:inst.NW
				if value(yw[w,t]) >= 0.99
					ywsol[w,t] = 1
				else
					ywsol[w,t] = 0
				end
			end
			for r in 1:inst.NR
				if value(yr[r,t]) >= 0.99
					yrsol[r,t] = 1
				else
					yrsol[r,t] = 0
				end
			end
		end

	end

	return bestsol

end


function FixAndOptimizeWarehouse(inst,model,yp,yw,yr,ypsol,ywsol,yrsol,bestsol,ware)

	for t in 1:inst.NT
		if ypsol[1,t] == 1
			set_lower_bound(yp[1,t],1)
		else
			set_lower_bound(yp[1,t],0)
		end

		for w in 1:inst.NW
			if ywsol[w,t] == 1
				set_lower_bound(yw[w,t],1)
			else
				set_lower_bound(yw[w,t],0)
			end
		end

		for r in 1:inst.NR
			if yrsol[r,t] == 1
				set_lower_bound(yr[r,t],1)
			else
				set_lower_bound(yr[r,t],0)
			end
		end

	end

	for t in 1:inst.NT
		set_lower_bound(yp[1,t],0)
		set_lower_bound(yw[ware,t],0)
		for k in 1:length(inst.DeltaW[ware])
			r = inst.DeltaW[ware][k]
			set_lower_bound(yr[r,t],0)
		end
	end

	optimize!(model)
	if getobjectivevalue(model) < bestsol - 0.01
		if getobjectivevalue(model) < bestsol - 0.5
			proceed = true
		end
		bestsol = getobjectivevalue(model)
		for t in 1:inst.NT
			if value(yp[1,t]) >= 0.99
				ypsol[1,t] = 1
			else
				ypsol[1,t] = 0
			end
			for w in 1:inst.NW
				if value(yw[w,t]) >= 0.99
					ywsol[w,t] = 1
				else
					ywsol[w,t] = 0
				end
			end
			for r in 1:inst.NR
				if value(yr[r,t]) >= 0.99
					yrsol[r,t] = 1
				else
					yrsol[r,t] = 0
				end
			end
		end

	end

end

end
