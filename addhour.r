addhour <- function(startpoint, increments){
    if ( !inherits(startpoint, "POSIXlt") ){
        stop("Please convert the input to POSIXlt object")
    }

    dummy = rep(startpoint, times = length(increments))
    dummy$hour = dummy$hour + increments

    return(dummy)
}
