plot2d <- function(variable, lat, lon, add = FALSE, close = FALSE, filename = NULL, grid = NULL, ...){
    # plot 2-D fields over an US map; add to existing plot or create a new file
    # variable = [lon, lat]
    # ... = additional input into quilt.plot
    
    require("maps")
    require("fields")

    # if provided filename, create a new file
    if (!is.null(filename)){
        pdf(filename)
        par(mfrow = grid)
    }

    if (is.null(dim(lat)) & is.null(dim(lon)) & !is.vector(variable)){
        # create meshgrid
        lat2 = matrix(data = lat, nrow = length(lon), ncol = length(lat), byrow = TRUE)
        lon2 = matrix(data = lon, nrow = length(lon), ncol = length(lat), byrow = FALSE)
        lat = lat2
        lon = lon2
    }

    # variable contour
    quilt.plot(x = as.vector(lon), y = as.vector(lat), z = as.vector(variable), add = add, ...)

    # add state outlines
    map("state", add = TRUE)

    if (close){
        dev.off()
    }
}
