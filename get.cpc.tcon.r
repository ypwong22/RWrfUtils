get.cpc.tcon <- function(path, pattern, varname){
    # path = directory of the cpc files
    # pattern = the files of interest, to reduce the amount of memory needed

    # Obtain time-constant WRF variables

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
