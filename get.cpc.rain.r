get.cpc.rain <- function(cpcpath, cpcfile, subset, return_tstamp = FALSE){
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
    ttt = seq(from = Timerange[1], to = Timerange[2], by = "1 year")
    year = unique( sprintf(ttt$year, "%4d") )
    filelist = sub("yyyy", year, cpcfile)

    filelist = paste(cpcpath, filelist, sep = "/")

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
        temp = var.get.nc(ncid, varname)
        
        # subset the time dimension
        temp = temp[, ,keep]

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

    if (return_tstamp){
        return(list(var = var, timestamp = tstamp))
    } else {
        return(var)
    }
}
