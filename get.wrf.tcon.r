get.wrf.tcon <- function(path, filename, varname){
    # path = directory of the wrf files
    # pattern = the files of interest, to reduce the amount of memory needed
    # subset = inclusive time range to plot the data, e.g. c("1995-01-01 00:00:00", "1995-01-31 23:00:00")

    # Obtain time-constant WRF variables

    require("RNetCDF")

    filelist = list.files(path = path, pattern = filename, full.names = TRUE)

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
