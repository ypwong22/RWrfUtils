clean=1
submit=0

if [ $clean -eq 0 ]
then

    for month in {1..12}
    do
	sed s/MONTHHERE/$month/ <template.wrf-cpc.r >wrf-cpc.${month}.r
    done

    for month in {1..12}
    do
	cat <<EOF >> wrf-cpc.${month}.sh
#!bin/bash
#PBS -N wrf-cpc-${month}
#PBS -q batch
#PBS -l nodes=1:ppn=1
#PBS -l mem=4gb
#PBS -l walltime=6:00:00
#PBS -r n
#PBS -j oe
  module load R
  cd /nfs/gpfs/PAS0661/RWrfUtil
  Rscript wrf-cpc.${month}.r
EOF
    done

    if [ $submit -eq 1 ]
    then
	for month in {1..12}
	do
	    qsub wrf-cpc.${month}.sh
	done
    fi
    
else

    for month in {1..12}
    do
	rm wrf-cpc.${month}.r
	rm wrf-cpc.${month}.sh
    done

fi