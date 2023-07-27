for i in *_randdpheurmc_*
do
	sed -i -e 's/dat.rand/dat;rand/g' $i

done
