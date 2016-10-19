# Compare the rmse between daily, monthly, and annual average precipitation of WRF and CPC

wrfdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/wrfout"
cpcdir = "/nfs/gpfs/PAS0661/DATA/CPC"
outdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-08-26/analysis/output"

source("compare.cpc.wrf.r")
source("plot2d.r")

subset = c("1981-12-01 00:00:00", "1981-12-31 23:00:00")
path = wrfdir
pattern = "wrfout_d01_1981-12*"
res = "daily"
calc = "ASIS"
pct = FALSE
cpcpath = cpcdir
cpcfile = "precip.V1.0.yyyy.nc"

diff = compare.cpc.wrf(subset, path, pattern, res, calc, pct, cpcpath, cpcfile)

#
plot2d(diff$diff, diff$lat, diff$lon)

# Write the ASIS to a netcdf file
