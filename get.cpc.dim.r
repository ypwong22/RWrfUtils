get.cpc.dim <- function(varname, path, pattern, subset = NULL){
    # path = directory of the cpc files
    # pattern = the files of interest, to reduce the amount of memory needed
    # subset = c(min, max) of latitude or longitude

    # Obtain lat/lon/time in the CPC files

    require("RNetCDF")

    filelist = list.files(path = path, pattern = pattern, full.names = TRUE)

    # if multiple files matched, take the 1st one
    filelist = filelist[1]

    ncid = open.nc(filelist)

    var = as.vector( var.get.nc(ncid, varname) )

    if (varname == "lon"){
        var = as.vector( var.get.nc(ncid,"lon") - 360. )
    }

    if (!is.null(sublat)){
        # subset spatially from the CONUS domain
        var = var[ which(var >= subset[1] & var <= subset[2]) ]
    }
    
    return(var)
}
