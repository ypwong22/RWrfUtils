compare.narr.wrf <- function(varname, vardim, subset, path, pattern, res = c("3-hourly","6-hourly","daily","monthly"), calc = c("ASIS","MEAN","RMSE"), pct = FALSE, narrpath, narrfile = NULL, prlevs = c(500, 700, 850), return_tstamp = FALSE){
    # Calculate the difference between NARR and WRF output on common 0.25x0.25 grids (the same as CPC)
    # subset = POSIXt objects or c("%Y-%m-%d %H:%M:%d", "%Y-%m-%d %H:%M:%d")
    # calc = "ASIS" - return time series
    #        "MEAN" - return mean(WRF-NARR)
    #        "SD"   - return rmse
    # prlevs - used only for 4D variables

    # Issue: Need yet to change the interpolation for precipitation to IDW; currently using bilinear for all

    require("RNetCDF")
    require("akima")
    source("get.wrf.tcon.r")
    source("get.wrf.tvar.r")
    source("get.narr.tcon.r")
    source("get.narr.tvar.r")
    source("condense.r")

    # lookup table between WRF and NARR variables
    WRFVAR = c("hgt","U","V","U10","V10","PSFC","TT","T2","RH","RH2","SH","SH2","RAINNC") # "PR","SST"
    NARFILE = c("hgt.yyyymm.nc","uwnd.yyyymm.nc","vwnd.yyyymm.nc","uwnd.10m.yyyy.nc","vwnd.10m.yyyy.nc","pres.sfc.yyyy.nc","air.yyyymm.nc","air.2m.yyyy.nc","shum.yyyy.nc","rhum.2m.yyyy.nc","shum.yyyymm.nc","shum.2m.yyyy.nc","apcp.yyyy.nc") # narr file name pattern
    NARVAR = c("hgt","uwnd","vwnd","uwnd","vwnd","pres","air","air","shum","rhum","shum","shum","apcp") # narr variable lat(y,x), lon(y,x), var(time, (level,) y, x), level [hPa], apcp [kg/m^2]

    # read the WRF variable
    if (varname == "RAINNC"){
        calcC = "SUM"
    } else {
        calcC = "MEAN"
    }

    vvv = get.wrf.tvar(varname, vardim, subset, path, pattern, calc = "ASIS", prlevs = prlevs, return_tstamp = TRUE)
    # convert the WRF variable to specified resolution
    wrfvar = condense(vvv$var, vvv$timestamp, calc = calcC, toscale = res, return_tstamp = TRUE)

    # get the WRF lat & lon to subset the NARR data to a more reasonable range covered by WRF
    wrflat = get.wrf.tcon("XLAT", path, pattern)
    wrflon = get.wrf.tcon("XLONG", path, pattern)

    wrflatrng = c(min(wrflat,na.rm=TRUE), max(wrflat,na.rm=TRUE))
    wrflonrng = c(min(wrflon,na.rm=TRUE), max(wrflon,na.rm=TRUE))

    # read the NARR variable (3-hourly)
    narrvarname = NARVAR[ which(WRFVAR == varname) ]
    if (is.null(narrfile)){
        narrfile = NARFILE[ which(WRFVAR == varname) ]
    }
    rrr = get.narr.tvar(narrvarname, subset, sublat = wrflatrng, sublon = wrflonrng, narrpath = narrpath, narrfile = narrfile, calc = "ASIS", prlevs = prlevs, return_tstamp = TRUE)
    # convert rhum to shum because pressure-level rhum is not available
    if (varname == "RH"){ # should have 4 dimensions
        Rd = 287 # [J kg-1 K-1]
        Rv = 462 # [J kg-1 K-1]
        e0 = 6.11 # [hPa]
        T0 = 273.15 # [K]
        L = 2.5 * 10^6 # [J kg-1]
        tt = get.narr.tvar("TT", subset, sublat = wrflatrng, sublon = wrflonrng, narrpath, narrfile, calc = "ASIS", prlevs = prlevs, return_tstamp = TRUE) + 273.15 # convert from oC back to K
        es = e0 * exp( L / Rv * (1/T0 - 1/tt) )
        for (i in 1:length(prlevs)){
            rrr$var[,,i,] = rrr$var[,,i,] * Rv * prlevs[i] / ( rrr$var[,,i,]*Rv + Rd ) / es[,,i] * 100
        }
    }
    # convert NARR variable to specified resolution
    narrvar = condense(rrr$var, rrr$timestamp, calc = calcC, toscale = res, return_tstamp = TRUE)

    # check that number of dimensions match
    wrfndim = length(dim(wrfvar$var))
    narrndim = length(dim(narrvar$var))
    if (wrfndim != narrndim){
        stop("The WRF variable and NARR variable are not the same number of dimensions!")
    }

    # create rectilinear grids that are approx. the same as WRF's resolution
    latCONUS = seq(from = 20., to = 50., by = mean( wrflat[,2:dim(wrflat)[2]] - wrflat[,1:(dim(wrflat)[2]-1)] ))
    lonCONUS = seq(from = -120, to = -55., by = mean( wrflon[2:dim(wrflat)[1],] - wrflon[1:(dim(wrflat)[1]-1),] ))

    # interplate WRF variable
    # - subset latCONUS, lonCONUS
    lato = latCONUS[ latCONUS >= wrflatrng[1] & latCONUS <= wrflatrng[2] ]
    lono = lonCONUS[ lonCONUS >= wrflonrng[1] & lonCONUS <= wrflonrng[2] ]

    if (wrfndim == 3){
        wrfvar_intrp = array(data=NA, dim=c( length(lono), length(lato), dim(wrfvar$var)[3] ))
        for (i in 1:dim(wrfvar$var)[3]){
            # missing values not allowed in interp
            a = as.vector(wrflon)
            b = as.vector(wrflat)
            c = as.vector(as.vector(wrfvar$var[,,i]))
            
            a = a[ !is.na(c) ]
            b = b[ !is.na(c) ]
            c = c[ !is.na(c) ]

            wrfvar_intrp[,,i] = interp(a,b,c, xo = lono, yo = lato)$z
        }
    } else if (wrfndim == 4){
        wrfvar_intrp = array(data=NA, dim=c( length(lono), length(lato), dim(wrfvar$var)[3], dim(wrfvar$var)[4] ))
        for (i in 1:dim(wrfvar$var)[3]){
            for (j in 1:dim(wrfvar$var)[4]){
                # missing values not allowed in interp
                a = as.vector(wrflon)
                b = as.vector(wrflat)
                c = as.vector(as.vector(wrfvar$var[,,i,j]))
                
                a = a[ !is.na(c) ]
                b = b[ !is.na(c) ]
                c = c[ !is.na(c) ]
                
                wrfvar_intrp[,,i,j] = interp(a,b,c, xo = lono, yo = lato)$z
            }
        }
    }

    # interpolate NARR variable to 0.25x0.25
    narrlat = get.narr.tcon("lat", narrpath, "*.nc", sublat = wrflatrng, sublon = wrflonrng)
    narrlon = get.narr.tcon("lon", narrpath, "*.nc", sublat = wrflatrng, sublon = wrflonrng)

    if (narrndim == 3){
        narrvar_intrp = array(data=NA, dim=c( length(lono), length(lato), dim(narrvar$var)[3] ))
        for (i in 1:dim(narrvar$var)[3]){
            # missing values not allowed in interp
            a = as.vector(narrlon)
            b = as.vector(narrlat)
            c = as.vector(narrvar$var[,,i])

            a = a[ !is.na(c) ]
            b = b[ !is.na(c) ]
            c = c[ !is.na(c) ]

            narrvar_intrp[,,i] = interp(a, b, c, xo = lono, yo = lato)$z
        }
    } else if (narrndim == 4){
        narrvar_intrp = array(data=NA, dim=c( length(lono), length(lato), dim(narrvar$var)[3], dim(narrvar$var)[4] ))
        for (i in 1:dim(narrvar$var)[3]){
            for (j in 1:dim(narrvar$var)[4]){
                # missing values not allowed in interp
                a = as.vector(narrlon)
                b = as.vector(narrlat)
                c = as.vector(narrvar$var[,,i,j])

                a = a[ !is.na(c) ]
                b = b[ !is.na(c) ]
                c = c[ !is.na(c) ]
                
                narrvar_intrp[,,i,j] = interp(a, b, c, xo = lono, yo = lato)$z
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
        if (return_tstamp){
            return( list(diff = diff, lat = lato, lon = lono, timestamp = narrvar$timestamp) )
        } else {
            return( list(diff = diff, lat = lato, lon = lono) )
        }
    } else if (calc == "MEAN"){
        if (return_tstamp){
            return( list(diff = apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = mean, na.rm = TRUE), lat = lato, lon = lono, timestamp = narrvar$timestamp) )
        } else {
            return( list(diff = apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = mean, na.rm = TRUE), lat = lato, lon = lono) )
        }
    } else if (calc == "RMSE"){
        if (return_tstamp){
            return( list(diff = apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = sd, na.rm = TRUE), lat = lato, lon = lono, timestamp = narrvar$timestamp) )
        } else {
            return( list(diff = apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = sd, na.rm = TRUE), lat = lato, lon = lono) )
        }
    }
}
