get.narr.tcon <- function(varname, narrpath, narrpattern){
    # get a constant from a file

    require("RNetCDF")

    filename = list.files(narrpath, narrpattern, full.names = TRUE)[1]

    ncid = open.nc(filename)
    var = var.get.nc(ncid, varname)

    ndim = length(dim(var))
    if (ndim == 3){
        var = var[,,1]
    } else if (ndim == 4){
        var = var[,,,1]
    }
    
    close.nc(ncid)
}
