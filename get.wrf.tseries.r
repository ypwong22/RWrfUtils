get.wrf.tseries <- function(varname, subset, path, pattern, lat, lon, prlevs = c(500, 700, 850), offset = NULL ){
    # path = directory of the wrf files
    # pattern = the files of interest, to reduce the amount of memory needed
    # subset = inclusive time range to plot the data, e.g. c("1995-01-01 00:00:00", "1995-01-31 23:00:00"), or POSIXct objects
    # prlevs (needed only when dim(var)=4) = the pressure levels to interpolate the data to
    # offset = 273.15, when reading temperatures

    # Obtain time-varying WRF variables as a time series at specified lat & lon

    require("RNetCDF")
    require("abind")
    source("interpfn.r")
    source("get.wrf.tcon.r")
    source("nearest.index.r")

    filelist = list.files(path = path, pattern = pattern, full.names = TRUE)

    if ( inherits(subset[1],"POSIXct") | inherits(subset[1],"POSIXlt") ){
        # do nothing
        Timerange = subset
    } else {
        # treat as string
        Timerange = strptime(subset, "%Y-%m-%d %H:%M:S")
    }

    # get latitude & longitude
    lat2 = get.wrf.tcon("XLAT", path, pattern)
    lon2 = get.wrf.tcon("XLONG", path, pattern)
    ind = nearest.index(lat, lon, lat2, lon2)

    print(paste("Found the nearest (lat,lon) = (",lat2[ind[1],ind[2]],",",lon2[ind[1],ind[2]],") to given (",lat,",",lon,")",sep=""))

    for (i in c(1:length(filelist))){
        ncid = open.nc(filelist[i])

        # get the Time stamps
        Times = strptime(var.get.nc(ncid, "Times"), "%Y-%m-%d_%H:%M:S")

        # get the variable
        if (varname == "hgt"){
            pr = var.get.nc(ncid, "P", start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE) + var.get.nc(ncid, "PB", start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE)
            temp = interp_vert( get_hgt(ncid, start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE), pr, prlevs, case = 1 )
            ndim = 4
        } else if (varname == "u") {
            pr = var.get.nc(ncid, "P", start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE) + var.get.nc(ncid, "PB", start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE)
            temp = interp_vert( get_u(ncid, start=c(ind[1],ind[2],NA,keeptime[1]), count=c(2,1,NA,length(keeptime)), collapse=FALSE), pr, prlevs, case = 0 ) # note: account for the stagger
            ndim = 4
        } else if (varname == "v"){
            pr = var.get.nc(ncid, "P", start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE) + var.get.nc(ncid, "PB", start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE)
            temp = interp_vert( get_v(ncid, start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,2,NA,length(keeptime)), collapse=FALSE), pr, prlevs, case = 0 ) # note: account for the stagger
            ndim = 4
        } else {
            # assume is on mass grid, only need to interpolate vertically
            temp = var.get.nc(ncid, varname, start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE)

            ndim = length(dim(temp))

            if ( ndim == 4 ){
                pr = var.get.nc(ncid, "P", start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE) + var.get.nc(ncid, "PB", start=c(ind[1],ind[2],NA,keeptime[1]), count=c(1,1,NA,length(keeptime)), collapse=FALSE)
                temp = interp_vert(temp, pr, prlevs, case = 0)
            }

            ## extract the nearest point
            #if ( ndim == 4 ){
            #    temp = temp[ind[1], ind[2], ,]
            #} else if ( ndim == 3 ){
            #    temp = temp[ind[1], ind[2], ]
            #}
        }

        ## subset to the needed time stamps
        #ndim = length( dim(var) )
        #if (ndim == 3){
        #    temp = temp[ which(Times>=Timerange[1] & Times<=Timerange[2]) ]
        #} else if (ndim == 4){
        #    temp = temp[ , which(Times>=Timerange[1] & Times<=Timerange[2]) ]
        #}

        # apply the offset
        if ( !is.null(offset) ){
            temp = temp - offset
        }

        # concatenate
        if (i == 1){
            var = temp
        } else {
            if (ndim == 3){
                var = abind(var, temp, along = 1)
            } else if (ndim == 4){
                var = abind(var, temp, along = 2)
            }
        }

        close.nc(ncid)
    }

    return( var )

}
