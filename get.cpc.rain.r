get.cpc.rain <- function(cpcpath, cpcfile, subset, sublat = NULL, sublon = NULL,calc = c("ASIS","SUM","MEAN","SD"), return_tstamp = FALSE){
    # subset - time range of the variables to read; POSIXlt/ct objects, or "%Y-%m-%d %H:%M:%S", length = 2 vector
    # cpcpath - directory of the CPC
    # cpcfile - subset the files to reduce the work

    require("RNetCDF")
    require("abind")
    source("addhour.r")

    # decide the time range
    if (inherits(subset,"POSIXlt")){
        Timerange = subset
    } else if (inherits(subset,"POSIXct")) {
        Timerange = as.POSIXlt( subset, tz = "GMT" )
    } else {
        Timerange = strptime( subset, "%Y-%m-%d %H:%M:%S", tz = "GMT" )
    }

    # decide the relevant files
    ttt = seq(from = Timerange[1], to = Timerange[2], by = 1)
    year = unique(format(ttt, "%Y", tz = "GMT"))
    filelist = c()
    for (i in 1:length(year)){
        filelist[i] = sub("yyyy", year[i], cpcfile)
    }
    print(filelist)

    filelist = paste(cpcpath, filelist, sep = "/")

    if (!is.null(sublat)){
        ncid = open.nc( filelist[1] )
        lat = var.get.nc(ncid, "lat")
        lon = var.get.nc(ncid, "lon") - 360.
        xrange = which(lon >= sublon[1] & lon <= sublon[2])
        yrange = which(lat >= sublat[1] & lat <= sublat[2])
        close.nc(ncid)
    }

    # loop through the relevant files
    for (i in c(1:length(filelist))){
        ncid = open.nc( filelist[i] )

        # get time and convert to POSIXt time
        time.offset = strsplit( att.get.nc(ncid,"time","units"), " " )[[1]]
        time.offset = strptime( paste(time.offset[3],time.offset[4]," "), format = "%Y-%m-%d %H:%M:%S " ) # start hour
        time = addhour( time.offset, var.get.nc(ncid,"time") )

        # which of the times are within subset
        keep = which( time >= Timerange[1] & time <= Timerange[2] )

        # read the preciptation (originally in [mm])
        # (with time dimension sub-setted)
        if (!is.null(sublat)){
            temp = var.get.nc(ncid, "precip", start=c(min(xrange),min(yrange),min(keep)), count=c(length(xrange),length(yrange),length(keep)))
        } else {
            temp = var.get.nc(ncid, "precip", start=c(NA,NA,min(keep)), count=c(NA,NA,length(keep)))
        }

        # concatenate
        if (i == 1){
            var = temp
        } else {
            var = abind( var, temp )
        }

        # concatenate time
        if (return_tstamp){
            if (i == 1){
                tstamp = time[keep]
            } else {
                tstamp = c( tstamp, time[keep] )
            }
        }
        close.nc(ncid)
    }

    # reduction
    if (calc == "ASIS"){
        var = var
    } else if (calc == "SUM"){
        var = apply(var, MARGIN = c(1,2), FUN = sum, na.rm=TRUE)
    } else if (calc == "MEAN"){
        var = apply(var, MARGIN = c(1,2), FUN = mean, na.rm=TRUE)
    } else if (calc == "SD"){
        var = apply(var, MARGIN = c(1,2), FUN = sd, na.rm=TRUE)
    } else {
        stop("Un-recognized statistics")
    }

    if (return_tstamp){
        return(list(var = var, timestamp = tstamp))
    } else {
        return(var)
    }
}
