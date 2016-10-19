# Compare the mean difference and rmse between daily variables of WRF and NARR on each month

wrfdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-09-03/wrfout"
narrdir = "/nfs/gpfs/PAS0661/DATA/NARR"
outdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-09-03/analysis/output"

source("compare.narr.wrf.r")
source("plot2d.r")

daysofmonth = c(31,28,31,30,31,30,31,31,30,31,30,31)

varlookup = c("hgt","U","V","U10","V10","PR","PSFC","TT","T2","RH","RH2","SH","SH2","RAINNC")
dimlookup = c(4, 4, 4, 3, 3, 4, 3, 4, 3, 4, 3, 4, 3, 3)

i = MONTHHERE # 1:12

varname = "VARHERE" # "hgt"
vardim = dimlookup[ varlookup == varname ]

if (length(vardim) < 1){
    stop(paste("Need to manually enter vardim for ",varname,".",sep=""))
}

subset = c(paste("2001-",sprintf("%02d",i),"-01 00:00:00",sep=""), paste("2001-",sprintf("%02d",i),"-",sprintf("%02d",daysofmonth[i])," 23:00:00",sep=""))
path = wrfdir
pattern = paste("wrfout_d01_2001-",sprintf("%02d",i),"*",sep="")
res = "daily"
calc = "ASIS"
pct = FALSE
narrpath = narrdir
narrfile = NULL # use default
prlevs = c(500,700,850)

# Get the difference
diff = compare.narr.wrf(varname, vardim, subset, path, pattern, res, calc, pct, narrpath, narrfile, prlevs, return_tstamp = TRUE)

# Get the interpolated lat & lon
lat = diff$lat
lon = diff$lon

#
mean = condense(diff$diff, diff$timestamp, calc = "MEAN", toscale = "monthly", return_tstamp = TRUE)
if (length(dim(mean$var)) == 4){
    mean$var = mean$var[,,,1] # remove the time dimension
} else if (length(dim(mean$var)) == 3 & vardim == 3){
    mean$var = mean$var[,,1] # remove the time dimension
}

#
rmse = condense(diff$diff, diff$timestamp, calc = "SD", toscale = "monthly", return_tstamp = TRUE)
if (length(dim(rmse$var)) == 4){
    rmse$var = rmse$var[,,,1] # remove the time dimension
} else if (length(dim(rmse$var)) == 3 & vardim == 3){
    rmse$var = rmse$var[,,1] # remove the time dimension
}

#
if (vardim == 4){
    pdf(paste(outdir,"/wrf-narr_",varname,"_",i,".pdf",sep=""))
    par(mfrow = c(length(prlevs), 2))
    for (j in 1:length(prlevs)){
        plot2d(mean$var[,,j], lat, lon, xlab = "Lon", ylab = "Lat")
        title(main = paste("Mean Difference at",prlevs[j],"[hPa]"))
        plot2d(rmse$var[,,j], lat, lon, xlab = "Lon", ylab = "Lat")
        title(main = paste("RMSE at",prlevs[j],"[hPa]"))
    }
    dev.off()
} else if (vardim == 3){
    pdf(paste(outdir,"/wrf-narr_",varname,"_",i,".pdf",sep=""))
    par(mfrow = c(2, 1))
    plot2d(mean$var, lat, lon, xlab = "Lon", ylab = "Lat")
    title(main = paste("Mean Difference"))
    plot2d(rmse$var, lat, lon, xlab = "Lon", ylab = "Lat")
    title(main = paste("RMSE"))
    dev.off()
}

# Write the difference to ".RData" file for record
save.image(paste(outdir,"/wrf-narr_",varname,"_",i,".RData",sep=""))
