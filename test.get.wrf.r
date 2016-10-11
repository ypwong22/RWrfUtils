RWrfUtil = "/nfs/gpfs/PAS0661/RWrfUtil"

setwd(RWrfUtil) # need to set this

source("plot2d.r")
source("get.wrf.tvar.r")
source("get.wrf.tcon.r")

# test 1 date, "ASIS"
vvv = get.wrf.tvar("hgt", 4, subset = c("1981-01-01 00:00:00", "1981-01-01 23:00:00"), path = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/wrfout/", pattern = "wrfout_d01_1981-01-01 *", calc = "ASIS", prlevs = 500, return_tstamp = TRUE)

# test multiple dates, "ASIS"
vvv = get.wrf.tvar("hgt", 4, subset = c("1981-02-28 00:00:00", "1981-03-01 23:00:00"), path = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/wrfout/", pattern = "wrfout_d01_1981-0[23]-0*", calc = "ASIS", prlevs = 500, return_tstamp = TRUE)

# test multiple dates, "MEAN"
vvv = get.wrf.tvar("T2", subset = c("1981-02-28 00:00:00", "1981-03-01 23:00:00"), path = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/wrfout/", pattern = "wrfout_d01_1981-0[23]-0*", calc = "MEAN", offset = 273.15, return_tstamp = TRUE)

lat = get.wrf.tcon("XLAT", path = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/wrfout/", pattern = "wrfout_d01_1981-01-01 *")
lon = get.wrf.tcon("XLONG", path = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/wrfout/", pattern = "wrfout_d01_1981-01-01 *")

plot2d(vvv$var, lat, lon)
