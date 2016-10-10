get.met_em.tcon <- function(varname, path, pattern){

    # "CLAT", "CLON" (west_east, south_north, Time)
    # "SOILHGT" (west_east, south_north, Time) terrain height

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
