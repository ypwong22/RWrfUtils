get.wrf.tseries <- function(varname, vardim, subset, path, pattern, lat, lon, prlevs = c(500, 700, 850), return_tstamp = FALSE){
    # path = directory of the wrf files
    # pattern = the files of interest, to reduce the amount of memory needed
    # subset = inclusive time range to plot the data, e.g. c("1995-01-01 00:00:00", "1995-01-31 23:00:00"), or POSIXct objects
    # prlevs (needed only when dim(var)=4) = the pressure levels to interpolate the data to
    # offset = 273.15, when reading temperatures

    # Obtain time-varying WRF variables as a time series at specified lat & lon

    require("RNetCDF")
    require("abind")
    source("interp_vert.r")
    source("get.wrf.tcon.r")
    source("nearest.index.r")

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

    # get latitude & longitude
    lat2 = get.wrf.tcon("XLAT", path, pattern)
    lon2 = get.wrf.tcon("XLONG", path, pattern)
    ind = nearest.index(lat, lon, lat2, lon2)

    print(paste("Found the nearest (lat,lon) = (",lat2[ind[1],ind[2]],",",lon2[ind[1],ind[2]],") to given (",lat,",",lon,")",sep=""))

    for (i in c(1:length(filelist))){
        print(paste("Begin process file ",i," = ", filelist[i],sep=""))

        ncid = open.nc(filelist[i])

        keeptime = timeinrange[[i]]

        if (return_tstamp){
            Times = as.POSIXct( strptime(var.get.nc(ncid, "Times"), "%Y-%m-%d_%H:%M:%S", tz = "GMT") )
            if (i == 1){
                time = Times[ keeptime ]
            } else {
                time = rbind(data.frame(time = time), data.frame(time = Times[ keeptime ]))[[1]] # only in this way does it keep the time zone
            }
        }

        if (vardim == 4){
            start = c(ind[1],ind[2],NA,keeptime[1])
            count = c(ind[1],ind[2],NA,length(keeptime))
        } else if (vardim == 3) {
            start = c(ind[1],ind[2],keeptime[1])
            count = c(ind[1],ind[2],length(keeptime))
        } else {
            stop(paste("Cannot handle vardim = ",vardim,"yet."))
        }

        # get the variable
        temp = get_wrf_var(ncid, varname, start = start, count = count, collapse = FALSE)

        if (vardim == 4){
            # subset to pressure levels
            pr = get_wrf_var(ncid, "PR", start=c(ind[1],ind[2],NA,keeptime[1]), count=c(ind[1],ind[2],NA,length(keeptime)), collapse=FALSE)
            temp = interp_vert( temp, pr, prlevs, case = case )
        }

        if (vardim == 4){
            # Time, pressure levels
            temp = aperm(temp,c(4,3,1,2))
        } else if (vardim == 3){
            temp = as.vector(temp)
        }

        # concatenate
        if (i == 1){
            var = temp
        } else {
            if (vardim == 3){
                var = c(var, temp)
            } else if (vardim == 4){
                var = abind(var, temp, along = 1)
            }
        }

        close.nc(ncid)
    }

    if (return_tstamp){
        return( list(var = var, timestamp = time) )
    } else {
        return( var )
    }
}
