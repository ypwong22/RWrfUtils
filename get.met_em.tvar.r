get.met_em.tvar <- function(varname, subset, path, pattern, calc = c("ASIS","SUM","MEAN","SD"), prlevs = c(500, 700, 850), offset = NULL, return_tstamp = FALSE ){
    require("RNetCDF")
    require("abind")

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

        # get the variable (with subset to the needed time stamps)
        if (varname == "UU") {
            temp = var.get.nc(ncid, "UU", start=c(NA,NA,NA,min(keeptime)), count=c(NA,NA,NA,length(keeptime)), collapse = FALSE)
            # remove the south_north_stag
            temp = 0.5 * (temp[,1:(dim(temp)[2]-1),,] + temp[,2:dim(temp)[2],,])
            ndim = 4
        } else if (varname == "VV"){
            temp = var.get.nc(ncid, "VV", start=c(NA,NA,NA,min(keeptime)), count=c(NA,NA,NA,length(keeptime)), collapse = FALSE)
            # remove the west_east_stag
            temp = 0.5 * (temp[1:(dim(temp)[1]-1),,,] + temp[2:dim(temp)[1],,,])
            ndim = 4
        } else if (varname == "U10"){
            var = var.get.nc(ncid, "UU", start=c(NA,NA,1,min(keeptime)), count=c(NA,NA,1,length(keeptime)), collapse = TRUE)
            # if time dimension was collapsed, add back
            if (length(dim(var)) == 2){
                var = as.array(var, dim=c(dim(var)[1], dim(var)[2], 1))
            }
            # remove the south_north_stag
            temp = 0.5 * (temp[,1:(dim(temp)[2]-1),] + temp[,2:dim(temp)[2],])
            # if four dimension remain, rearrange the time dimension to last
            if (length(dim(var)) == 4){
                var = aperm(var, c(1,2,4,3))
            }
            ndim = 3
        } else if (varname == "V10"){
            var = var.get.nc(ncid, "VV", start=c(NA,NA,1,min(keeptime)), count=c(NA,NA,1,length(keeptime)), collapse = TRUE)
            # if time dimension was collapsed, add back
            if (length(dim(var)) == 2){
                var = as.array(var, dim=c(dim(var)[1], dim(var)[2], 1))
            }
            # remove the west_east_stag
            temp = 0.5 * (temp[1:(dim(temp)[1]-1),,] + temp[2:dim(temp)[2],,])
            # if four dimension remain, rearrange the time dimension to last
            if (length(dim(var)) == 4){
                var = aperm(var, c(1,2,4,3))
            }
            ndim = 3
        } else if (varname == "T2"){
            var = var.get.nc(ncid, "TT", start=c(NA,NA,1,min(keeptime)), count=c(NA,NA,1,length(keeptime)), collapse = TRUE)
            # if time dimension was collapsed, add back
            if (length(dim(var)) == 2){
                var = as.array(var, dim=c(dim(var)[1], dim(var)[2], 1))
            }
            # if four dimension remain, rearrange the time dimension to last
            if (length(dim(var)) == 4){
                var = aperm(var, c(1,2,4,3))
            }
            ndim = 3
        } else {
            # no stagger and not the 1st layer
            var = var.get.nc(ncid, varname, start=c(NA,NA,NA,min(keeptime)), count=c(NA,NA,NA,length(keeptime)), collapse = FALSE)
            ndim = length(dim(var))
        }

        if (ndim == 4){
            # pressure levels - subset to one column
            pr = var.get.nc(ncid, "PRES", start = c(NA,NA,NA,1), count = c(NA,NA,NA,1))
            pr = lapply(pr, MARGIN=3, FUN=mean, na.rm=TRUE)
            keep = c()
            for (i in 1:length(prlevs)){
                keep[i] = which( pr == prlevs[i] )
            }
            var = var[,,keep,]
        }

        # apply the offset
        if ( !is.null(offset) ){
            temp = temp - offset
        }

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
