get.wrf.tvar <- function(varname, vardim, subset, path, pattern, calc = c("ASIS","SUM","MEAN","SD"), prlevs = c(500, 700, 850), return_tstamp = FALSE, bucket_mm= NULL){
    # vardim = 3 or 4, the dimension of the variable
    # path = directory of the wrf files
    # pattern = the files of interest, to reduce the amount of memory needed
    # subset = inclusive time range to plot the data, e.g. c("1995-01-01 00:00:00", "1995-01-31 23:00:00"), or POSIXct objects
    # prlevs (needed only when dim(var)=4) = the pressure levels to interpolate the data to
    # if return_tstamp = TRUE, return a vector of the time stamps of the slices of var (before SUM/MEAN/SD was performed)
    # bucket_mm = the value in "nameplist.input" where rainfall accumulation resets to zero

    # Obtain time-varying WRF variables

    require("RNetCDF")
    require("abind")
    source("interp_vert.r")
    source("get_wrf_var.r")

    filelist = list.files(path = path, pattern = pattern, full.names = TRUE)

    if ( inherits(subset[1],"POSIXct") ){
        # do nothing
        Timerange = subset
    } else if (inherits(subset[1],"POSIXlt")){
        Timerange = as.POSIXct(subset, tz = "GMT")
    } else {
        # treat as string
        Timerange = as.POSIXct( strptime(subset, "%Y-%m-%d %H:%M:%S", tz = "GMT") )
    }

    # decide whether each file is in range
    fileinrange = c()
    timeinrange = list() # whether the time stamps in each file is in range
    for (i in c(1:length(filelist))){
        ncid = open.nc(filelist[i])

        # get the Time stamps
        Times = as.POSIXct( strptime(var.get.nc(ncid, "Times"), "%Y-%m-%d_%H:%M:%S", tz = "GMT") )

        timeinrange[[i]] = which( Times >= Timerange[1] & Times<=Timerange[2] )

        if (length(timeinrange[[i]]) > 0){
            fileinrange[i] = TRUE
        } else {
            fileinrange[i] = FALSE
        }

        close.nc(ncid)
    }

    timeinrange = timeinrange[ fileinrange ]
    filelist = filelist[ fileinrange ]

    for (i in c(1:length(filelist))){
        print(paste("Begin process file ",i," = ", filelist[i],sep=""))

        ncid = open.nc(filelist[i])

        keeptime = timeinrange[[i]]

        if (return_tstamp){
            Times = as.POSIXct( strptime(var.get.nc(ncid, "Times"), "%Y-%m-%d_%H:%M:%S", tz = "GMT") )
            if (i == 1){
                time = Times[ keeptime ]
            } else {
                time = rbind( data.frame(time = time), data.frame(time = Times[ keeptime ]) )[[1]] # only in this way does it keep the time zone
            }
        }

        if (vardim == 4){
            start = c(NA,NA,NA,keeptime[1])
            count = c(NA,NA,NA,length(keeptime))
        } else if (vardim == 3) {
            start = c(NA,NA,keeptime[1])
            count = c(NA,NA,length(keeptime))
        } else {
            stop(paste("Cannot handle vardim = ",vardim,"yet."))
        }

        if (varname == "hgt"){
            case = 1
        } else {
            case = 0
        }

        # get the variable
        temp = get_wrf_var(ncid, varname, bucket_mm = bucket_mm, start=start, count=count, collapse = FALSE)

        if (vardim == 4){
            # subset to pressure levels
            pr = get_wrf_var(ncid, "PR", start=c(NA,NA,NA,keeptime[1]), count=c(NA,NA,NA,length(keeptime)), collapse=FALSE)
            temp = interp_vert( temp, pr, prlevs, case = case )
        }

        # concatenate
        if (i == 1){
            var = temp
        } else {
            var = abind(var, temp, along = vardim)
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
        if (vardim == 3){
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2), FUN = sum, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2), FUN = sum, na.rm=TRUE) )
            }
        } else if (vardim == 4) {
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2,3), FUN = sum, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2,3), FUN = sum, na.rm=TRUE) )
            }
        }
    } else if (calc == "MEAN"){
        if (vardim == 3){
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2), FUN = mean, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2), FUN = mean, na.rm=TRUE) )
            }
        } else if (vardim == 4) {
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2,3), FUN = mean, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2,3), FUN = mean, na.rm=TRUE) )
            }
        }
    } else if (calc == "SD"){
        if (vardim == 3){
            if (return_tstamp){
                return( list( var = apply(var, MARGIN = c(1,2), FUN = sd, na.rm=TRUE), timestamp = time ) )
            } else {
                return( apply(var, MARGIN = c(1,2), FUN = sd, na.rm=TRUE) )
            }
        } else if (vardim == 4){
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
