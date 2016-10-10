compare.met_em.wrf <- function(path, pattern, subset, varname, calc = c("ASIS","MEAN","RMSE"), pct = FALSE, metpath, metpattern, prlevs = c(500, 700, 850)){
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

    # lookup table between WRF and MET_EM variable
    WRFVAR = c("hgt","U","V","rh","T2","RH2","U10","V10")
    METVAR = c("GHT","UU","VV","RH","T2","RH2","U10","V10")

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

    # read the NARR variable (3-hourly)
    narrvarname = NARVAR[ which(WRFVAR == varname) ]
    if (narrvarname == "air"){
        offset = 273.15
    } else {
        offset = 0
    }
    narrvar = get.narr.tvar(narrvarname, subset, metpath, NARFILE[ which(WRFVAR == varname) ], prlevs = prlevs, calc = "ASIS")
    # convert NARR variable to daily
    wrfvar = condense(vvv$var, vvv$timestamp, calc = calcC, toscale = "daily")

    # check that number of dimensions match
    wrfndim = length(dim(wrfvar))
    narrndim = length(dim(narrvar))
    if (wrfndim != narrndim){
        stop("The WRF variable and NARR variable are not the same number of dimensions!")
    }

    # interplate WRF variable to 0.25x0.25
    # - subset latCONUS, lonCONUS to a more reasonable range covered by WRF
    wrflat = get.wrf.tcon("XLAT", path, pattern)
    wrflon = get.wrf.tcon("XLONG", path, pattern)

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
    narrlat = get.narr.tcon("lat", metpath, "*.nc")
    narrlon = get.narr.tcon("lon", metpath, "*.nc")

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
