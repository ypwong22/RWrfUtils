RWrfUtil = "/nfs/gpfs/PAS0661/RWrfUtil"
setwd(RWrfUtil)

source("plot2d.r")
source("get.narr.tcon.r")
source("get.narr.tvar.r")

sublat = c(30,50)
sublon = c(-100,-90)

# read 1 file
var = get.narr.tvar("hgt", subset = c("1981-01-10 00:00:00","1981-01-11 00:00:00"), sublat = sublat, sublon = sublon, narrpath = "/nfs/gpfs/PAS0661/DATA/NARR", narrfile = "hgt.yyyymm.nc", calc = "ASIS", prlevs = c(500, 700), offset = 0)

# read several files
var = get.narr.tvar("hgt", subset = c("1986-12-31 00:00:00","1987-01-11 00:00:00"), sublat = sublat, sublon = sublon, narrpath = "/nfs/gpfs/PAS0661/DATA/NARR", narrfile = "hgt.yyyymm.nc", calc = "MEAN", prlevs = c(500, 700), offset = 0)

# plot
lat = get.narr.tcon("lat", narrpath = "/nfs/gpfs/PAS0661/DATA/NARR/", narrpattern = "hgt.198001.nc", sublat = sublat, sublon = sublon)
lon = get.narr.tcon("lon", narrpath = "/nfs/gpfs/PAS0661/DATA/NARR/", narrpattern = "hgt.198001.nc", sublat = sublat, sublon = sublon)

#plot2d(var[,,1,1],lat,lon, xlim=sublon, ylim=sublat)
plot2d(var[,,1],lat,lon, xlim=sublon, ylim=sublat)
