get.narr.tvar <- function(varname, subset, narrpath, narrfile, prlevs = c(500, 700, 850), offset){
    # varname - the name of the variable to read in NARR files
    # subset - time range of the variables to read; POSIXlt/ct objects, or "%Y-%m-%d %H:%M:%S", length = 2 vector
    # narrpath - directory of the NARR files
    # narrpattern - subset the files to reduce the work
    # prlevs - if 4D variable, subset to pressure levels

    # Note: NARFILE = c("hgt.yyyymm.nc","uwnd.yyyymm.nc","vwnd.yyyymm.nc","air.2m.yyyy.nc","uwnd.10m.yyyy.nc","vwnd.10m.yyyy.nc","apcp.yyyy.nc") # narr file name pattern

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
    ttt = seq(from = Timerange[1], to = Timerange[2], by = "1 month")
    if ( pmatch("yyyymm", narrfile) == 1 ){
        year = unique( paste(sprintf(ttt$year,"%4d"), sprintf(ttt$month,"%02d"), sep = "") )
        filelist = sub("yyyymm", year, narrfile)
    } else if ( pmatch("yyyy", narrfile) == 1 ){
        year = unique( sprintf(ttt$year, "%4d") )
        filelist = sub("yyyy", year, narrfile)
    }

    filelist = paste(narrpath, filelist, sep = "/")

    # loop through the relevant files
    for (i in c(1:length(filelist))){
        ncid = open.nc( filelist[i] )

        # get time and convert to POSIXt time
        time.offset = strsplit( att.get.nc(ncid,"time","units"), " " )[[1]]
        time.offset = strptime( paste(time.offset[3],time.offset[4]," "), format = "%Y-%m-%d %H:%M:%S " ) # start hour
        time = addhour( time.offset, var.get.nc(ncid,"time") )

        # which of the times are within subset
        keep = which( time >= Timerange[1] & time <= Timerange[2] )
        
        # read the variable
        temp = var.get.nc(ncid, varname)
        if ( var.inq.nc(ncid, varname)$type == "short" ){
            # unpack
            add_offset = att.get.nc(ncid, varname, "add_offset")
            scale_factor = att.get.nc(ncid, varname, "scale_factor")
            # the same as "short2flt" http://www.ncl.ucar.edu/Applications/narr.shtml
            temp = temp*scale_factor + add_offset
        }
        
        # subset the time dimension
        ndim = length(dim(temp))
        if (ndim == 3){
            temp = temp[, ,keep]
        } else if (ndim == 4){
            level = var.get.nc(ncid, "level") # milibar
            keep2 = c()
            for (j in c(1:length(prlevs))){
                keep2[j] = which( level == prlevs[j] )
            }
            temp = temp[, , keep2, keep]
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

    return(var)
}
