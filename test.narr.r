# Compare the rmse between daily shum of WRF and NARR

wrfdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/wrfout"
narrdir = "/nfs/gpfs/PAS0661/DATA/NARR"
outdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/analysis/output"

source("compare.narr.wrf.r")
source("plot2d.r")

varname = "SH"
vardim = 4
subset = c("1981-07-01 00:00:00", "1981-07-31 23:00:00")
path = wrfdir
pattern = "wrfout_d01_1981-07*"
res = "daily"
calc = "ASIS"
pct = FALSE
narrpath = narrdir
narrfile = "shum.yyyy.nc"
prlevs = c(500,700,850)

diff = compare.narr.wrf(varname, vardim, subset, path, pattern, res, calc, pct, narrpath, narrfile, prlevs)

#
plot2d(diff$diff, diff$lat, diff$lon)


# Write the ASIS to a netcdf file
