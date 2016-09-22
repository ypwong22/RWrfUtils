compare.narr.wrf <- function(path, pattern, subset, varname, calc = c("ASIS","MEAN","RMSE"), pct = FALSE, narrpath, prlevs = c(500, 700, 850)){
    # Calculate the difference between NARR and WRF output on common 0.25x0.25 grids (the same as CPC)
    # subset = POSIXt objects or c("%Y-%m-%d %H:%M:%d", "%Y-%m-%d %H:%M:%d")
    # calc = "ASIS" - return time series
    #        "MEAN" - return mean(WRF-NARR)
    #        "SD"   - return rmse
    # prlevs - used only for 4D variables

    require("RNetCDF")
    require("akima")
    source("get.wrf.tcon.r")
    source("get.wrf.tvar.r")
    source("get.narr.tcon.r")
    source("get.narr.tvar.r")
    source("condense.r")

    # the rectangular grids
    latCONUS = c(20.125, 20.375, 20.625, 20.875, 21.125, 21.375, 21.625, 21.875, 22.125, 22.375, 22.625, 22.875, 23.125, 23.375, 23.625, 23.875, 24.125, 24.375, 24.625, 24.875, 25.125, 25.375, 25.625, 25.875, 26.125, 26.375, 26.625, 26.875, 27.125, 27.375, 27.625, 27.875, 28.125, 28.375, 28.625, 28.875, 29.125, 29.375, 29.625, 29.875, 30.125, 30.375, 30.625, 30.875, 31.125, 31.375, 31.625, 31.875, 32.125, 32.375, 32.625, 32.875, 33.125, 33.375, 33.625, 33.875, 34.125, 34.375, 34.625, 34.875, 35.125, 35.375, 35.625, 35.875, 36.125, 36.375, 36.625, 36.875, 37.125, 37.375, 37.625, 37.875, 38.125, 38.375, 38.625, 38.875, 39.125, 39.375, 39.625, 39.875, 40.125, 40.375, 40.625, 40.875, 41.125, 41.375, 41.625, 41.875, 42.125, 42.375, 42.625, 42.875, 43.125, 43.375, 43.625, 43.875, 44.125, 44.375, 44.625, 44.875, 45.125, 45.375, 45.625, 45.875, 46.125, 46.375, 46.625, 46.875, 47.125, 47.375, 47.625, 47.875, 48.125, 48.375, 48.625, 48.875, 49.125, 49.375, 49.625, 49.875)
    
    lonCONUS = -360 + c(230.125, 230.375, 230.625, 230.875, 231.125, 231.375, 231.625, 231.875, 232.125, 232.375, 232.625, 232.875, 233.125, 233.375, 233.625, 233.875, 234.125, 234.375, 234.625, 234.875, 235.125, 235.375, 235.625, 235.875, 236.125, 236.375, 236.625, 236.875, 237.125, 237.375, 237.625, 237.875, 238.125, 238.375, 238.625, 238.875, 239.125, 239.375, 239.625, 239.875, 240.125, 240.375, 240.625, 240.875, 241.125, 241.375, 241.625, 241.875, 242.125, 242.375, 242.625, 242.875, 243.125, 243.375, 243.625, 243.875, 244.125, 244.375, 244.625, 244.875, 245.125, 245.375, 245.625, 245.875, 246.125, 246.375, 246.625, 246.875, 247.125, 247.375, 247.625, 247.875, 248.125, 248.375, 248.625, 248.875, 249.125, 249.375, 249.625, 249.875, 250.125, 250.375, 250.625, 250.875, 251.125, 251.375, 251.625, 251.875, 252.125, 252.375, 252.625, 252.875, 253.125, 253.375, 253.625, 253.875, 254.125, 254.375, 254.625, 254.875, 255.125, 255.375, 255.625, 255.875, 256.125, 256.375, 256.625, 256.875, 257.125, 257.375, 257.625, 257.875, 258.125, 258.375, 258.625, 258.875, 259.125, 259.375, 259.625, 259.875, 260.125, 260.375, 260.625, 260.875, 261.125, 261.375, 261.625, 261.875, 262.125, 262.375, 262.625, 262.875, 263.125, 263.375, 263.625, 263.875, 264.125, 264.375, 264.625, 264.875, 265.125, 265.375, 265.625, 265.875, 266.125, 266.375, 266.625, 266.875, 267.125, 267.375, 267.625, 267.875, 268.125, 268.375, 268.625, 268.875, 269.125, 269.375, 269.625, 269.875, 270.125, 270.375, 270.625, 270.875, 271.125, 271.375, 271.625, 271.875, 272.125, 272.375, 272.625, 272.875, 273.125, 273.375, 273.625, 273.875, 274.125, 274.375, 274.625, 274.875, 275.125, 275.375, 275.625, 275.875, 276.125, 276.375, 276.625, 276.875, 277.125, 277.375, 277.625, 277.875, 278.125, 278.375, 278.625, 278.875, 279.125, 279.375, 279.625, 279.875, 280.125, 280.375, 280.625, 280.875, 281.125, 281.375, 281.625, 281.875, 282.125, 282.375, 282.625, 282.875, 283.125, 283.375, 283.625, 283.875, 284.125, 284.375, 284.625, 284.875, 285.125, 285.375, 285.625, 285.875, 286.125, 286.375, 286.625, 286.875, 287.125, 287.375, 287.625, 287.875, 288.125, 288.375, 288.625, 288.875, 289.125, 289.375, 289.625, 289.875, 290.125, 290.375, 290.625, 290.875, 291.125, 291.375, 291.625, 291.875, 292.125, 292.375, 292.625, 292.875, 293.125, 293.375, 293.625, 293.875, 294.125, 294.375, 294.625, 294.875, 295.125, 295.375, 295.625, 295.875, 296.125, 296.375, 296.625, 296.875, 297.125, 297.375, 297.625, 297.875, 298.125, 298.375, 298.625, 298.875, 299.125, 299.375, 299.625, 299.875, 300.125, 300.375, 300.625, 300.875, 301.125, 301.375, 301.625, 301.875, 302.125, 302.375, 302.625, 302.875, 303.125, 303.375, 303.625, 303.875, 304.125, 304.375, 304.625, 304.875)

    # NARR path
    if (is.null(narrpath)){
        narrpath = "/nfs/gpfs/PAS0661/DATA/NARR"
    }

    # lookup table between WRF and NARR variables
    WRFVAR = c("hgt","u","v","T2","U10","V10","RAINNC") # SST
    NARFILE = c("hgt.yyyymm.nc","uwnd.yyyymm.nc","vwnd.yyyymm.nc","air.2m.yyyy.nc","uwnd.10m.yyyy.nc","vwnd.10m.yyyy.nc","apcp.yyyy.nc") # narr file name pattern
    NARVAR = c("hgt","uwnd","vwnd","air","uwnd","vwnd","apcp") # narr variable lat(y,x), lon(y,x), var(time, (level,) y, x), level [hPa], apcp [kg/m^2]

    # read the WRF variable (need to convert to daily)
    if (varname == "T2"){
        offset = 273.15
    } else {
        offset = 0
    }
    if (varname == "RAINNC"){
        calcC = "SUM"
    } else {
        calcC = "MEAN"
    }

    vvv = get.wrf.tvar(path, pattern, subset, varname, calc = "ASIS", prlevs = prlevs, offset = offset, return_tstamp = TRUE)
    # convert the WRF variable to daily
    wrfvar = condense(vvv$var, vvv$timestamp, calc = calcC, toscale = "daily")

    # read the NARR variable (daily)
    narrvarname = NARVAR[ which(WRFVAR == varname) ]
    if (narrvarname == "air"){
        offset = 273.15
    } else {
        offset = 0
    }

    narrvar = get.narr.tvar(narrvarname, subset, narrpath, NARFILE[ which(WRFVAR == varname) ], prlevs = prlevs)

    # check that number of dimensions match
    wrfndim = length(dim(wrfvar))
    narrndim = length(dim(narrvar))
    if (wrfndim != narrndim){
        stop("The WRF variable and NARR variable are not the same number of dimensions!")
    }

    # interplate WRF variable to 0.25x0.25
    # - subset latCONUS, lonCONUS to a more reasonable range covered by WRF
    wrflat = get.wrf.tcon(path, pattern, "XLAT")
    wrflon = get.wrf.tcon(path, pattern, "XLONG")

    lato = latCONUS[ latCONUS > min(wrflat, na.rm=TRUE) & latCONUS < max(wrflat, na.rm=TRUE) ]
    lono = lonCONUS[ lonCONUS > min(wrflon, na.rm=TRUE) & lonCONUS < max(wrflon, na.rm=TRUE) ]

    if (wrfndim == 3){
        wrfvar_intrp = array(data=NA, dim=c( length(lono), length(lato), dim(wrfvar)[3] ))
        for (i in 1:dim(wrfvar)[3]){
            wrfvar_intrp[,,i] = interp(as.vector(wrflon), as.vector(wrflat), as.vector(wrfvar[,,i]), xo = lono, yo = lato)
        }
    } else if (wrfndim == 4){
        wrfvar_intrp = array(data=NA, dim=c( length(lono), length(lato), dim(wrfvar)[3], dim(wrfvar)[4] ))
        for (i in 1:dim(wrfvar)[3]){
            for (j in 1:dim(wrfvar)[4]){
                wrfvar_intrp[,,i,j] = interp(as.vector(wrflon), as.vector(wrflat), as.vector(wrfvar[,,i,j]), xo = lono, yo = lato)
            }
        }
    }

    # interpolate NARR variable to 0.25x0.25
    narrlat = get.narr.tcon("lat", narrpath, "*.nc")
    narrlon = get.narr.tcon("lon", narrpath, "*.nc")

    if (narrndim == 3){
        narrvar_intrp = array(data=NA, dim=c( length(lono), length(lato), dim(narrvar)[3] ))
        for (i in 1:dim(narrvar)[3]){
            narrvar_intrp[,,i] = interp(as.vector(narrlon), as.vector(narrlat), as.vector(narrvar[,,i]), xo = lono, yo = lato)
        }
    } else if (narrndim == 4){
        narrvar_intrp = array(data=NA, dim=c( length(lono), length(lato), dim(narrvar)[3], dim(narrvar)[4] ))
        for (i in 1:dim(narrvar)[3]){
            for (j in 1:dim(narrvar)[4]){
                narrvar_intrp[,,i,j] = interp(as.vector(narrlon), as.vector(narrlat), as.vector(narrvar[,,i,j]), xo = lono, yo = lato)
            }
        }
    }

    # check that the dimensions match
    if (sum( dim(narrvar_intrp) - dim(wrfvar_intrp) ) != 0){
        stop("Interpolated dimension mismatch?")
    }

    # calculate the difference
    diff = wrfvar_intrp - narrvar_intrp
    if (pct){
        diff = diff / narrvar_intrp * 100
    }

    # ASIS, MEAN, RMSE, PCTASIS, PCTMEAN, PCTRMSE
    if (calc == "ASIS"){
        return( diff )
    } else if (calc == "MEAN"){
        return( apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = mean, na.rm = TRUE) )
    } else if (calc == "RMSE"){
        return( apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = sd, na.rm = TRUE) )
    }
}
