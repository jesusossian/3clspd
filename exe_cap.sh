#!/bin/bash
#bal=0 # 0 unbal 1 bal
#cap=1.50 # 2.00 1.75 1.50
#capw=1.50 # 2.00 1.75 1.50
#capr=1.50 # 2.00 1.75 1.50
for bal in 0 # 0 1
do
    for form in esn estp esls #std mc 3level partialmc echelon esn estp esls
    do
	for cap in 1.75 # 1.50 1.75 2.00
	do
	    for capw in 1.50 1.75 2.00
	    do
		for capr in 1.50 1.75 2.00 # 1.50 1.75 2.00
		do
		    for N in 50 #50 100 200
		    do
			for T in 15 #15 30
			do
			    for P in 1
			    do
				for W in 5 10 15 20 #5 10 15 20
				do
				    for V in DD_SF #SD_SF SD_DF DD_SF DD_DF
				    do
					echo "instance;form;cap;capw;capr;T;W;R;bestbound;bestsol;gap;time;nodes;opt" >> saida.txt
		    			for id in $(seq 5)
		    			do
		    			    julia threeplsp.jl --inst instances/N${N}T${T}/N${N}T${T}P${P}W${W}${V}${id}.dat --form ${form} --balanced ${bal} --capacity ${cap} --capacityw ${capw} --capacityr ${capr} >> report/bal${bal}_form${form}_cap${cap}_capw${capw}_capr${capr}_N${N}T${T}P${P}W${W}${V}${id}.txt
		    			done
					mv saida.txt result/bal${bal}_form${form}_cap${cap}_capw${capw}_capr${capr}_N${N}_T${T}.csv
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
