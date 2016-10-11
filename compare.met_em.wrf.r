compare.met_em.wrf <- function(varname, vardim, subset, path, pattern, res = c("6-hourly","daily","monthly"), calc = c("ASIS","MEAN","RMSE"), pct = FALSE, metpath, metpattern, prlevs = c(500, 700, 850), return_tstamp = FALSE){
    # Calculate the difference between MET_EM input and WRF output (both are on WRF grids)
    # subset = POSIXt objects or c("%Y-%m-%d %H:%M:%d", "%Y-%m-%d %H:%M:%d")
    # calc = "ASIS" - return time series of (WRF-MET_EM)
    #        "MEAN" - return mean(WRF-MET_EM)
    #        "RMSE"   - return rmse
    # prlevs - used only for 4D variables
    # return_tstamp = "TRUE" then return the timestamps before the calculation of "MEAN"/"RMSE"

    require("RNetCDF")
    require("akima")
    source("get.wrf.tcon.r")
    source("get.wrf.tvar.r")
    source("get.met_em.tcon.r")
    source("get.met_em.tvar.r")
    source("condense.r")

    # lookup table between WRF and MET_EM variable
    WRFVAR = c("hgt","U","V","U10","V10","PR","PSFC","TT","T2","RH","RH2","SH","SH2")
    METVAR = c("GHT","UU","VV","U10","V10","PRES","PSFC","TT","T2","RH","RH2","SH","SH2")

    # read the WRF variable (need to convert to daily)
    if (varname == "RAINNC"){
        calcC = "SUM"
    } else {
        calcC = "MEAN"
    }

    vvv = get.wrf.tvar(varname, vardim, subset, path, pattern, calc = "ASIS", prlevs = prlevs, return_tstamp = TRUE)
    # convert the WRF variable to 6-hourly, daily or monthly
    wrfvar = condense(vvv$var, vvv$timestamp, calc = calcC, toscale = res)

    # read the MET_EM variable (6-hourly)
    metvarname = METVAR[ which(WRFVAR == varname) ]
    rrr = get.met_em.tvar(metvarname, vardim, subset, metpath, metpattern, calc = "ASIS", prlevs = prlevs, return_tstamp = TRUE)
    # convert the MET_EM variable to 6-hourly, daily or monthly
    metvar = condense(rrr$var, rrr$timestamp, calc = calcC, toscale = res)

    # check that number of dimensions match
    wrfndim = length(dim(wrfvar$var))
    metndim = length(dim(metvar$var))
    if (wrfndim != metndim){
        stop("The WRF variable and NARR variable are not the same number of dimensions!")
    }

    # calculate the difference
    diff = wrfvar$var - metvar$var
    if (pct){
        diff = diff / metvar$var * 100
    }

    # ASIS, MEAN, RMSE, PCTASIS, PCTMEAN, PCTRMSE
    if (calc == "ASIS"){
        if (return_tstamp){
            return( list(diff = diff, timestamp = metvar$timestamp ) )
        } else {
            return( diff )
        }
    } else if (calc == "MEAN"){
        if (return_tstamp){
            return( list(diff = apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = mean, na.rm = TRUE), timestamp = metvar$timestamp ) )
        } else {
            return( apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = mean, na.rm = TRUE) )
        }
    } else if (calc == "RMSE"){
        if (return_tstamp){
            return( list(diff = apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = sd, na.rm = TRUE), timestamp = metvar$timestamp ) )
        } else {
            return( apply(diff, MARGIN = c(1:(wrfndim-1)), FUN = sd, na.rm = TRUE) )
        }
    }
}
