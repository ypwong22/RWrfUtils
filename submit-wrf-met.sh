clean=1
submit=0

if [ $clean -eq 0 ]
then

    for month in {1..12}
    do
	sed s/MONTHHERE/$month/ <template.wrf-met.r >wrf-met.${month}.temp.r
	for var in hgt RH T2 RH2 # hgt U V U10 V10 TT T2 RH RH2
	do
	    sed s/VARHERE/$var/ <wrf-met.${month}.temp.r >wrf-met.${month}.${var}.r
	done
	rm wrf-met.${month}.temp.r
    done

    for month in {1..12}
    do
	for var in hgt RH T2 RH2 # hgt U V U10 V10 TT T2 RH RH2 
	do
cat <<EOF >> wrf-met.${month}.${var}.sh
#!bin/bash
#PBS -N wrf-met-${month}-${var}
#PBS -q batch
#PBS -l nodes=1:ppn=1
#PBS -l mem=4gb
#PBS -l walltime=6:00:00
#PBS -r n
#PBS -j oe
  module load R
  cd /nfs/gpfs/PAS0661/RWrfUtil
  Rscript wrf-met.${month}.${var}.r
EOF
	done
    done

    if [ $submit -eq 1 ]
    then
	for month in {1..12}
	do
	    for var in hgt RH T2 RH2 #hgt U V U10 V10 TT T2 RH RH2 
	    do
		qsub wrf-met.${month}.${var}.sh
	    done
	done
    fi

else

    for month in {1..12}
    do
	for var in hgt U V U10 V10 TT T2 RH RH2 
	do
	    rm wrf-met.${month}.${var}.r
	    rm wrf-met.${month}.${var}.sh
	done
    done

fi