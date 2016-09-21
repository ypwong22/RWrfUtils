nearest.index <- function(lat, lon, lat2, lon2){
    # given 2-D arrays lat2, lon2, find the data points nearest to lat & lon

    distance = (lat2 - lat)^2 + (lon2 - lon)^2

    return( which( distance == min(distance, na.rm=TRUE), arr.ind = TRUE ) )
}
