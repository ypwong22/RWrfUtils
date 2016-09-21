get.wrf.tvar <- function(path, pattern, subset, varname, calc = c("ASIS","SUM","MEAN","SD"), prlevs = c(500, 700, 850), offset = NULL ){
    # path = directory of the wrf files
    # pattern = the files of interest, to reduce the amount of memory needed
    # subset = inclusive time range to plot the data, e.g. c("1995-01-01 00:00:00", "1995-01-31 23:00:00"), or POSIXct objects
    # prlevs (needed only when dim(var)=4) = the pressure levels to interpolate the data to
    # offset = 273.15, when reading temperatures

    # Obtain time-varying WRF variables

    require("RNetCDF")
    require("abind")
    source("interpfn.r")

    filelist = list.files(path = path, pattern = pattern, full.names = TRUE)

    if ( inherits(subset[1],"POSIXct") | inherits(subset[1],"POSIXlt") ){
        # do nothing
        Timerange = subset
    } else {
        # treat as string
        Timerange = strptime(subset, "%Y-%m-%d %H:%M:S")
    }

    for (i in c(1:length(filelist))){
        ncid = open.nc(filelist[i])

        # get the Time stamps
        Times = strptime(var.get.nc(ncid, "Times"), "%Y-%m-%d_%H:%M:S")

        # get the variable
        if (varname == "hgt"){
            pr = var.get.nc(ncid, "P") + var.get.nc(ncid, "PB")
            temp = interp_vert( get_hgt(ncid), pr, prlevs, case = 1 )

            ndim = 4
        } else if (varname == "u") {
            pr = var.get.nc(ncid, "P") + var.get.nc(ncid, "PB")
            temp = interp_vert( get_u(ncid), pr, prlevs, case = 0 )

            ndim = 4
        } else if (varname == "v"){
            pr = var.get.nc(ncid, "P") + var.get.nc(ncid, "PB")
            temp = interp_vert( get_v(ncid), pr, prlevs, case = 0 )

            ndim = 4
        } else {
            # assume is on mass grid, only need to interpolate vertically
            temp = var.get.nc(ncid, varname)
            ndim = length(dim(temp))
            if ( ndim == 4 ){
                pr = var.get.nc(ncid, "P") + var.get.nc(ncid, "PB")
                temp = interp_vert(temp, pr, prlevs, case = 0)
            }
        }

        # apply the offset
        if ( !is.null(offset) ){
            temp = temp - offset
        }

        # subset to the needed time stamps
        if (ndim == 3){
            temp = temp[,,which(Times>=Timerange[1] & Times<=Timerange[2])]
        } else if (ndim == 4){
            temp = temp[,,,which(Times>=Timerange[1] & Times<=Timerange[2])]
        }

        # concatenate
        if (i == 1){
            var = temp
        } else {
            var = abind(var, temp, along = ndim)
        }

        close.nc(ncid)
    }

    # calculate statistics
    if (calc == "ASIS"){
        return( var )
    } else if (calc == "SUM"){
        if (ndim == 3){
            return( apply(var, MARGIN = c(1,2), FUN = sum, na.rm=TRUE) )
        } else if (ndim == 4) {
            return( apply(var, MARGIN = c(1,2,3), FUN = sum, na.rm=TRUE) )
        }
    } else if (calc == "MEAN"){
        if (ndim == 3){
            return( apply(var, MARGIN = c(1,2), FUN = mean, na.rm=TRUE) )
        } else if (ndim == 4) {
            return( apply(var, MARGIN = c(1,2,3), FUN = mean, na.rm=TRUE) )
        }            
    } else if (calc == "SD"){
        if (ndim == 3){
            return( apply(var, MARGIN = c(1,2), FUN = sd, na.rm=TRUE) )
        } else if (ndim == 4){
            return( apply(var, MARGIN = c(1,2,3), FUN = sd, na.rm=TRUE) )
        }
    } else {
        stop("Un-recognized statistics")
    }
}
