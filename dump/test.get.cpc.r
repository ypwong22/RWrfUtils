RWrfUtil = "/nfs/gpfs/PAS0661/RWrfUtil"
setwd(RWrfUtil)

source("plot2d.r")
source("get.cpc.rain.r")
source("get.cpc.dim.r")

sublat = c(25,50)
sublon = c(-125,-75)

# read 1 file
var = get.cpc.rain(cpcpath = "/nfs/gpfs/PAS0661/DATA/CPC", cpcfile = "precip.V1.0.yyyy.nc", subset = c("1981-01-10 00:00:00","1981-01-11 00:00:00"), sublat = sublat, sublon = sublon, calc = "ASIS", return_tstamp = TRUE)

# read several files
var = get.cpc.rain(cpcpath = "/nfs/gpfs/PAS0661/DATA/CPC", cpcfile = "precip.V1.0.yyyy.nc", subset = c("1981-12-31 00:00:00","1982-01-31 00:00:00"), sublat = sublat, sublon = sublon, calc = "SUM", return_tstamp = TRUE)

# plot
lat = get.cpc.dim("lat",path = "/nfs/gpfs/PAS0661/DATA/CPC", pattern = "precip.V1.0.1990", subset = sublat)
lon = get.cpc.dim("lon",path = "/nfs/gpfs/PAS0661/DATA/CPC", pattern = "precip.V1.0.1990", subset = sublon)

plot2d(var$var, lat, lon, xlim=sublon, ylim=sublat, nx = length(lon), ny = length(lat))
