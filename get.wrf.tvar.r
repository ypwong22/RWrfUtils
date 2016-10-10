get.wrf.tvar <- function(varname, subset, path, pattern, calc = c("ASIS","SUM","MEAN","SD"), prlevs = c(500, 700, 850), offset = NULL, return_tstamp = FALSE ){
    # path = directory of the wrf files
    # pattern = the files of interest, to reduce the amount of memory needed
    # subset = inclusive time range to plot the data, e.g. c("1995-01-01 00:00:00", "1995-01-31 23:00:00"), or POSIXct objects
    # prlevs (needed only when dim(var)=4) = the pressure levels to interpolate the data to
    # offset = 273.15, when reading temperatures
    # if return_tstamp = TRUE, return a vector of the time stamps of the slices of var

    # Obtain time-varying WRF variables

    require("RNetCDF")
    require("abind")
    source("intrpfn.r")

    filelist = list.files(path = path, pattern = pattern, full.names = TRUE)

    if ( inherits(subset[1],"POSIXlt") ){
        # do nothing
        Timerange = subset
    } else if (inherits(subset[1],"POSIXct")){
        Timerange = as.POSIXlt(subset, tz = "GMT")
    } else {
        # treat as string
        Timerange = strptime(subset, "%Y-%m-%d %H:%M:%S", tz = "GMT")
    }

    # decide whether each file is in range
    fileinrange = c()
    timeinrange = list() # whether the time stamps in each file is in range
    for (i in c(1:length(filelist))){
        ncid = open.nc(filelist[i])

        # get the Time stamps
        Times = strptime(var.get.nc(ncid, "Times"), "%Y-%m-%d_%H:%M:%S", tz = "GMT")

        timeinrange[[i]] = Times >= Timerange[1] & Times<=Timerange[2]

        if (sum(timeinrange[[i]]) > 0){
            fileinrange[i] = TRUE
        } else {
            fileinrange[i] = FALSE
        }

        if (return_tstamp){
            if (i == 1){
                time = Times[ timeinrange[[i]] ]
            } else {
                time =c(time, Times[ timeinrange[[i]] ])
            }
            
            # print(Times)
            # print(Times[ timeinrange[[i]] ] )
            # print(time)
        }
        close.nc(ncid)
    }

    timeinrange = timeinrange[ fileinrange ]
    filelist = filelist[ fileinrange ]

    for (i in c(1:length(filelist))){
        print(paste("Begin process file ",i," = ", filelist[i],sep=""))

        ncid = open.nc(filelist[i])

        keeptime = which(timeinrange[[i]])

        # get the variable
        if (varname == "hgt"){
            pr = var.get.nc(ncid, "P", start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE) + var.get.nc(ncid, "PB", start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE)
            temp = interp_vert( get_hgt(ncid, start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE), pr, prlevs, case = 1 )

            ndim = 4
        } else if (varname == "u") {
            pr = var.get.nc(ncid, "P", start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE) + var.get.nc(ncid, "PB", start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE)
            temp = interp_vert( get_u(ncid, start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE), pr, prlevs, case = 0 )

            ndim = 4
        } else if (varname == "v"){
            pr = var.get.nc(ncid, "P", start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE) + var.get.nc(ncid, "PB", start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE)
            temp = interp_vert( get_v(ncid, start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE), pr, prlevs, case = 0)

            ndim = 4
        } else {
            # assume is on mass grid, only need to interpolate vertically
            temp = var.get.nc(ncid, varname, start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE)
            ndim = length(dim(temp))
            if ( ndim == 4 ){
                pr = var.get.nc(ncid, "P", start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE) + var.get.nc(ncid, "PB", start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE)
                temp = interp_vert(temp, pr, prlevs, case = 0)
            }
        }

        # apply the offset
        if ( !is.null(offset) ){
            temp = temp - offset
        }

        ## subset to the needed time stamps
        #if (ndim == 3){
        #    temp = temp[,, timeinrange[[i]], drop = FALSE]
        #} else if (ndim == 4){
        #    temp = temp[,,, timeinrange[[i]], drop = FALSE]
        #}

        # concatenate
        if (i == 1){
            var = temp
        } else {
            var = abind(var, temp, along = ndim)
        }

        # print(dim(temp))
        # print(dim(var))

        close.nc(ncid)
    }

    # calculate statistics
    if (calc == "ASIS"){
        if (return_tstamp){
            return( list(var = var, timestamp = time) )
        } else {
            return( var )
        }
    } else if (calc == "SUM"){
        if (ndim == 3){
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2), FUN = sum, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2), FUN = sum, na.rm=TRUE) )
            }
        } else if (ndim == 4) {
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2,3), FUN = sum, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2,3), FUN = sum, na.rm=TRUE) )
            }
        }
    } else if (calc == "MEAN"){
        if (ndim == 3){
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2), FUN = mean, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2), FUN = mean, na.rm=TRUE) )
            }
        } else if (ndim == 4) {
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2,3), FUN = mean, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2,3), FUN = mean, na.rm=TRUE) )
            }
        }
    } else if (calc == "SD"){
        if (ndim == 3){
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2), FUN = sd, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2), FUN = sd, na.rm=TRUE) )
            }
        } else if (ndim == 4){
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2,3), FUN = sd, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2,3), FUN = sd, na.rm=TRUE) )
            }
        }
    } else {
        stop("Un-recognized statistics")
    }
}
