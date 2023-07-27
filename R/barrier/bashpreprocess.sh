for i in *.txt
do
	sed -i '/instance;bestboud/d' $i
	mv -- "$i" "${i%.txt}.csv"
done

for i in *_std_*
do
	sed -i '1s/^/instance;form;bestbound;bestsol;gap;time;numnodes;disablesolver\n/' $i

done

for i in *_3level_*
do
	sed -i '1s/^/instance;form;bestbound;bestsol;gap;time;numnodes\n/' $i

done

for i in *_bcstd_*
do
	sed -i '1s/^/instance;form;bestbound;bestsol;gap;time;numnodes;disablesolver\n/' $i

done

for i in *_bc3level_*
do
	sed -i '1s/^/instance;form;bestbound;bestsol;gap;time;numnodes;disablesolver\n/' $i

done

for i in *_mc_*
do
	sed -i '1s/^/instance;form;bestbound;bestsol;gap;time;numnodes;disablesolver\n/' $i

done

for i in *_randdpheurmc_*
do
	sed -i '1s/^/instance;form;heursol;bestbound;bestsol;gap;time;numnodes;percfixed\n/' $i
	sed -i -e 's/datrand/dat;rand/g' $i

done

for i in *_randdpheur_*
do
	sed -i '1s/^/instance;bestcost; averagecost; elapsedtime; dptype; dpalpha\n/' $i

done
