#!/bin/bash
maxt=600
P=1
for bal in 0 #0 1
do
    for fixsize in 2 3 4
    do
	for freeint in 2 3 4
	do
	    for model in rf
	    do
		for cap in 1.5 #2.00 1.75 1.50
		do
		    for capw in 0.0
		    do
			for capr in 0.0
			do
			    for N in 50 #50 100 200
			    do
				for T in 15 #15 30
				do
				    for W in 5 10 15 20
				    do
					for V in SD_SF SD_DF DD_SF DD_DF
					do
					    echo "instance;T;W;R;cap;bal;fixsize;freeint;maxtime;bestsol;time" >> saida.txt
					    for id in $(seq 5)
					    do
						julia threeplsp.jl --inst instances/N${N}T${T}/N${N}T${T}P${P}W${W}${V}${id}.dat --form ${model} --balanced ${bal} --capacity ${cap} --maxtimerf ${maxt} --fixsizerf ${fixsize} --freeintervalrf ${freeint} >> report/bal${bal}_cap${cap}_fixsize${fixsize}_freeint${freeint}_${model}_N${N}T${T}P${P}W${W}${V}${id}.txt
					    done
					    mv saida.txt result/bal${bal}_${model}_fixsize${fixsize}_freeint${freeint}_cap${cap}_capw${capw}_capr${capr}_N${N}_T${T}_W${W}_V${V}.csv    
					done
				    done
				done
			    done
			done
		    done
		done
	    done
	done
    done
done
