# Compare the difference in mean and extreme quantiles between daily precipitation of CPC and WRF in each month

wrfdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-09-03/wrfout"
cpcdir = "/nfs/gpfs/PAS0661/DATA/CPC"
outdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-09-03/analysis/output"

source("compare.cpc.wrf.r")
source("plot2d.r")

daysofmonth = c(31,28,31,30,31,30,31,31,30,31,30,31)

i = MONTHHERE # 1:12

subset = c(paste("2001-",sprintf("%02d",i),"-01 00:00:00",sep=""), paste("2001-",sprintf("%02d",i),"-",sprintf("%02d",daysofmonth[i])," 23:00:00",sep=""))
path = wrfdir
pattern = paste("wrfout_d01_200*",sep="")
res = "daily"
calc = "ASIS"
pct = FALSE
cpcpath = cpcdir
cpcfile = "precip.V1.0.yyyy.nc" # use default
wrfstepsize = 1 # hour
bucket_mm = 100

# Get the difference
diff = compare.cpc.wrf(subset, path, pattern, wrfstepsize, bucket_mm, res, calc, pct, cpcpath, cpcfile, return_tstamp = TRUE)

# Get the interpolated lat & lon
lat = diff$lat
lon = diff$lon

#
mean = condense(diff$diff, diff$timestamp, calc = "MEAN", toscale = "monthly", return_tstamp = TRUE)
if (length(dim(mean$var)) == 3){
    mean$var = mean$var[,,1] # remove the time dimension
}

#
rmse = condense(diff$diff, diff$timestamp, calc = "SD", toscale = "monthly", return_tstamp = TRUE)
if (length(dim(rmse$var)) == 3){
    rmse$var = rmse$var[,,1] # remove the time dimension
}

#
pdf(paste(outdir,"/wrf-cpc_",i,".pdf",sep=""))
par(mfrow = c(2, 1))
plot2d(mean$var, lat, lon, xlab = "Lon", ylab = "Lat", nx = length(lon), ny = length(lat))
title(main = paste("Mean Difference"))
plot2d(rmse$var, lat, lon, xlab = "Lon", ylab = "Lat", nx = length(lon), ny = length(lat))
title(main = paste("RMSE"))
dev.off()

# Write the difference to ".RData" file for record
save.image(paste(outdir,"/wrf-cpc_",i,".RData",sep=""))
