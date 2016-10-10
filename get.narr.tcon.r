get.narr.tcon <- function(varname, narrpath, narrpattern, sublat = NULL, sublon = NULL){
    # get a constant from a file
    # sublat - c(min_lat, max_lat)
    # sublon - c(min_lon, max_lon)

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

    if (!is.null(sublat)) {
        # subset spatially
        lat = var.get.nc(ncid,"lat")
        lon = var.get.nc(ncid,"lon")

        latrange = which(lat >= sublat[1] & lat <= sublat[2], arr.ind = TRUE)
        lonrange = which(lon >= sublon[1] & lon <= sublon[2], arr.ind = TRUE)
        
        yrange = c(min(latrange[,2]):max(latrange[,2]))
        xrange = c(min(lonrange[,1]):max(lonrange[,1]))

        if (ndim == 2 | ndim == 3){
            var = var[xrange, yrange]
        } else if (ndim == 4){
            var = var[xrange, yrange, ]
        }
    }

    close.nc(ncid)

    return(var)
}
