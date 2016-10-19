clean=1
submit=0

if [ $clean -eq 0 ]
then

    for month in {1..12}
    do
	sed s/MONTHHERE/$month/ <template.wrf-narr.r >wrf-narr.${month}.temp.r
	for var in RH2 RAINNC #hgt SH SH2 T2 RH RH2 RAINNC # hgt U V U10 V10 TT T2 RH RH2 SH SH2 RAINNC
	do
	    sed s/VARHERE/$var/ <wrf-narr.${month}.temp.r >wrf-narr.${month}.${var}.r
	done
	rm wrf-narr.${month}.temp.r
    done

    for month in {1..12}
    do
	for var in RH2 RAINNC #hgt SH SH2 T2 RH RH2 RAINNC # hgt U V U10 V10 TT T2 RH RH2 SH SH2 RAINNC
	do
cat <<EOF >> wrf-narr.${month}.${var}.sh
#!bin/bash
#PBS -N wrf-narr-${month}-${var}
#PBS -q batch
#PBS -l nodes=1:ppn=1
#PBS -l mem=4gb
#PBS -l walltime=6:00:00
#PBS -r n
#PBS -j oe
  module load R
  cd /nfs/gpfs/PAS0661/RWrfUtil
  Rscript wrf-narr.${month}.${var}.r
EOF
	done
    done

    if [ $submit -eq 1 ]
    then
	for month in {1..12}
	do
	    for var in RH2 RAINNC #hgt SH SH2 T2 RH RH2 RAINNC #hgt U V U10 V10 TT T2 RH RH2 SH SH2 RAINNC
	    do
		qsub wrf-narr.${month}.${var}.sh
	    done
	done
    fi

else

    for month in {1..12}
    do
	for var in hgt U V U10 V10 TT T2 RH RH2 SH SH2 RAINNC
	do
	    rm wrf-narr.${month}.${var}.r
	    rm wrf-narr.${month}.${var}.sh
	done
    done

fi