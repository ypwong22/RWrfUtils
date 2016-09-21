compare.narr.wrf <- function(path, pattern, varname, subset, calc = c("ASIS","MEAN","SD"), narrpath){
    # Calculate the difference between NARR and WRF output on common 0.25x0.25 grids (the same as CPC)
    # calc = "ASIS" - return time series
    #        "MEAN" - return mean(WRF-NARR)
    #        "SD"   - return rmse

    require("RNetCDF")
    require("akima")
    source("get.wrf.tcon.r")
    source("get.wrf.tvar.r")
    source("get.narr.tcon.r")
    source("get.narr.tvar.r")

    # NARR path
    if (is.null(narrpath)){
        narrpath = "/nfs/gpfs/PAS0661/DATA/NARR"
    }

    # lookup table between WRF and NARR variables
    WRFVAR = c("hgt","u","v","T2","U10","V10","RAINNC") # SST
    NARFILE = c("hgt","uwnd","vwnd","air.2m","uwnd.10m","vwnd.10m","apcp") # narr file name pattern; var.<yyyy><mm>.nc
    NARVAR = c("hgt","uwnd","vwnd",
# narr variable lat(y,x), lon(y,x), hgt(time, level, y, x), level [hPa]
    
}
