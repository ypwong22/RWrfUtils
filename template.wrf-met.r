# Compare the mean difference and rmse between daily hgt of WRF and MET_EM on each month

wrfdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/wrfout"
metdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/met"
outdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/analysis/output"

source("compare.met_em.wrf.r")
source("get.wrf.tcon.r")
source("plot2d.r")

daysofmonth = c(31,28,31,30,31,30,31,31,30,31,30,31)

i = MONTHHERE # 1:12

varname = VARHERE # "hgt"
vardim = DIMHERE # 4
subset = c(paste("1981-",sprintf("%02d",i),"-01 00:00:00",sep=""), paste("1981-",sprintf("%02d",i),"-",sprintf("%02d",daysofmonth[i])," 23:00:00",sep=""))
path = wrfdir
pattern = paste("wrfout_d01_1981-",sprintf("%02d",i),"*",sep="")
res = "daily"
calc = "ASIS"
pct = FALSE
metpath = metdir
metpattern = paste("met_em.d01.1981-",sprintf("%02d",i),"*",sep="")
prlevs = c(500,700,850)

# Get lat & lon
lat = get.wrf.tcon("XLAT", path, pattern)
lon = get.wrf.tcon("XLONG", path, pattern)

# Get the difference
diff = compare.met_em.wrf(varname, vardim, subset, path, pattern, res, calc, pct, metpath, metpattern, prlevs)

#
mean = condense(diff$diff, diff$timestamp, calc = "MEAN", toscale = "monthly")
mean$var = mean$var[,,,1] # remove the time dimension

#
rmse = condense(diff$diff, diff$timestamp, calc = "SD", toscale = "monthly")
rmse$var = rmse$var[,,,1] # remove the time dimension

#
pdf(paste("./output/wrf-met_",varname,"_",i,".pdf",sep=""))
par(mfrow = c(length(prlevs), 2))
for (j in 1:length(prlevs)){
    plot2d(mean$var[,,j], lat, lon, xlab = "Lon", ylab = "Lat")
    title(main = "Mean Difference")
    plot2d(rmse$var[,,j], lat, lon, xlab = "Lon", ylab = "Lat")
    title(main = "RMSE")
}
dev.off()

# Write the average hgt of WRF, met_em, and the difference to ".RData" file for record
save.image(paste("./output/wrf-met_",varname,"_",i,".RData",sep=""))
