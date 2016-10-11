get.wrf.tcon <- function(varname, path, pattern){
    # path = directory of the wrf files
    # pattern = the files of interest, to reduce the amount of memory needed
    # subset = inclusive time range to plot the data, e.g. c("1995-01-01 00:00:00", "1995-01-31 23:00:00")

    # Obtain time-constant WRF variables
    # "XLAT" "XLON"

    require("RNetCDF")

    filelist = list.files(path = path, pattern = pattern, full.names = TRUE)

    # if multiple files matched, take the 1st one
    filelist = filelist[1]

    ncid = open.nc(filelist)

    var = var.get.nc(ncid, varname)

    # if multiple time steps, subset to the 1st one
    ndim = length( dim(var) )
    if (ndim == 3){
        var = var[,,1]
    } else if (ndim == 4){
        var = var[,,,1]
    }

    return(var)
}
