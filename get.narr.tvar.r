get.narr.tvar <- function(varname, subset, sublat, sublon, narrpath, narrfile, calc = c("ASIS","SUM","MEAN","SD"), prlevs = c(500, 700, 850), offset, return_tstamp = FALSE){
    # varname - the name of the variable to read in NARR files
    # subset - time range of the variables to read; POSIXlt/ct objects, or "%Y-%m-%d %H:%M:%S", length = 2 vector
    # sublat - c(min_lat, max_lat)
    # sublon - c(min_lon, max_lon)
    # narrpath - directory of the NARR files
    # narrpattern - subset the files to reduce the work
    # prlevs - if 4D variable, subset to pressure levels

    # Note: NARFILE = c("hgt.yyyymm.nc","uwnd.yyyymm.nc","vwnd.yyyymm.nc","air.2m.yyyy.nc","uwnd.10m.yyyy.nc","vwnd.10m.yyyy.nc","apcp.yyyy.nc") # narr file name pattern

    require("RNetCDF")
    require("abind")
    source("addhour.r")
    source("get.narr.tcon.r")

    # decide the time range
    if (inherits(subset,"POSIXlt")){
        Timerange = subset
    } else if (inherits(subset,"POSIXct")) {
        Timerange = as.POSIXlt( subset, tz = "GMT" )
    } else {
        Timerange = strptime( subset, "%Y-%m-%d %H:%M:%S", tz = "GMT" )
    }

    # decide the relevant files
    ttt = as.POSIXlt( seq(from = Timerange[1], to = Timerange[2], by = "1 month"), tz = "GMT" )
    print(format(ttt,"%Y",tz ="GMT"))
    print(format(ttt,"%m",tz="GMT"))
    if ( grep("yyyymm", narrfile) ){
        year = unique( paste( format(ttt,"%Y",tz ="GMT"), format(ttt,"%m",tz="GMT"), sep = "") )
        filelist = c()
        for (i in 1:length(year)){
            filelist[i] = sub("yyyymm", year[i], narrfile)
        }
    } else if ( grep("yyyy", narrfile) ){
        year = unique( format(ttt,"%Y",tz="GMT") )
        filelist = c()
        for (i in 1:length(year)){
            filelist[i] = sub("yyyy", year[i], narrfile)
        }
    }

    filelist = paste(narrpath, filelist, sep = "/")

    # get a subset of lat & lon because otherwise the file will be too large
    lat = get.narr.tcon("lat", narrpath, tail(strsplit(filelist[1],"/")[[1]],1))
    lon = get.narr.tcon("lon", narrpath, tail(strsplit(filelist[1],"/")[[1]],1))
    
    latrange = which(lat >= sublat[1] & lat <= sublat[2], arr.ind = TRUE)
    lonrange = which(lon >= sublon[1] & lon <= sublon[2], arr.ind = TRUE)
    
    yrange = c(min(latrange[,2]), max(latrange[,2]) - min(latrange[,2]) + 1)
    xrange = c(min(lonrange[,1]), max(lonrange[,1]) - min(lonrange[,1]) + 1)

    # loop through the relevant files
    for (i in c(1:length(filelist))){
        ncid = open.nc( filelist[i] )

        # get time and convert to POSIXt time
        time.offset = strsplit( att.get.nc(ncid,"time","units"), " " )[[1]]
        time.offset = strptime( paste(time.offset[3],time.offset[4]," "), format = "%Y-%m-%d %H:%M:%S ", tz="GMT" ) # start hour
        time = addhour( time.offset, var.get.nc(ncid,"time") )

        # which of the times are within subset
        keep = which( time >= Timerange[1] & time <= Timerange[2] )
        
        # number of dimensions of the variable
        ndim = var.inq.nc(ncid, varname)$ndims

        # subset the pressure level dimension
        if (ndim == 4){
            level = var.get.nc(ncid, "level") # milibar
            keep2 = c()
            for (j in c(1:length(prlevs))){
                keep2[j] = which( level == prlevs[j] )
            }
        }

        # read the variable with time and spatial dimensions sub-setted
        if (ndim == 4){
            temp = var.get.nc(ncid, varname, start = c(xrange[1],yrange[1],NA,keep[1]), count = c(xrange[2],yrange[2],NA,length(keep)),unpack=TRUE,collapse=FALSE)
            # subset the pressure level dimension
            if (ndim == 3){
                temp = temp[,,keep2]
            } else if (ndim == 4) {
                temp = temp[,,keep2,]
            }
        } else {
            temp = var.get.nc(ncid, varname, start = c(xrange[1],yrange[1],keep[1]), count = c(xrange[2],yrange[2],length(keep)),unpack=TRUE,collapse=FALSE)
        }

        # concatenate
        if (i == 1){
            var = temp
        } else {
            var = abind( var, temp )
        }

        close.nc(ncid)
    }

    # offset - for temperature
    var = var - offset

    # if precipitation, convert to [mm]
    if (varname == "apcp"){
        var = var * 86400; 
    }

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

    return(var)
}
