# Plot the WRF elevation and NARR elevation for comparison

metdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-09-03/met"
narrdir = "/nfs/gpfs/PAS0661/DATA/NARR"
outdir = "/nfs/gpfs/PAS0661/downscaling2/wrf/2016-09-03/analysis/output"

source("get.met_em.tcon.r")
source("get.narr.tcon.r")
source("plot2d.r")

pattern = "met_em.d01.2001-01-01_00*"

wrflat = get.met_em.tcon("XLAT_M", path = metdir, pattern = pattern)
wrflon = get.met_em.tcon("XLONG_M", path = metdir, pattern = pattern)

wrfelev = get.met_em.tcon("ELEV", path = metdir, pattern = pattern)

narrlat = get.narr.tcon("lat", narrpath = narrdir, narrpattern = "hgt.sfc.nc")
narrlon = get.narr.tcon("lon", narrpath = narrdir, narrpattern = "hgt.sfc.nc")

narrelev = get.narr.tcon("hgt", narrpath = narrdir, narrpattern = "hgt.sfc.nc")

pdf(paste(outdir,"/elevation-wrf-narr.pdf",sep=""))
par(mfrow=c(2,1))

brks = seq(0, 500, length.out = 65)

plot2d(wrfelev, wrflat, wrflon, xlim = c(min(wrflon), max(wrflon)), ylim = c(min(wrflat), max(wrflat)), breaks = brks)
plot2d(narrelev, narrlat, narrlon, xlim = c(min(wrflon), max(wrflon)), ylim = c(min(wrflat), max(wrflat)), breaks = brks)
dev.off()
