condense <- function( variable, timestamp, calc = c("MEAN","SUM"), toscale = c("daily","monthly","annual")){
    # timestamp and the last dimension of variable must have equal length
    # variable = [time], [y, x, time] or [y, x, levels, time]
    # timestamp = "%Y-%m-%d %H:%M:%S" or POSIXlt objects
    # use the timestamp to convert variable to daily/monthly/annual

    if (is.vector(variable)){
        variable = as.matrix(variable, nrow = length(variable))
        ndim = 1
    } else {
        ndim = length(dim(variable))
    }

    if ( dim(variable)[ndim] != length(timestamp) ){
        stop("timestamp and the last dimension of variable must have equal length!")
    }

    if (inherits(timestamp, "string")){
        timestamp = strptime( timestamp, "%Y-%m-%d %H:%M:%S" )
    } else {
        if (inherits(timestamp, "POSIXct")){
            timestamp = as.POSIXlt( timestamp )
        } else {
            if (!inherits(timestamp, "POSIXlt")){
                stop("timestamp must be string, POSIXct, or POSIXlt")
            }
        }
    }

    if (toscale == "daily"){
        # the year-month-days
        timetemp = timestamp
        timetemp$hour = 0
        timetemp$min = 0
        timetemp$sec = 0
        timetemp = unique(timetemp)

        # append the last day + 1
        ttt = timetemp[ length(timetemp) ]
        ttt$day = ttt$day + 1
        timetemp = c(timetemp, ttt)
    } else if (toscale == "monthly"){
        timetemp = timestamp
        timetemp$mday = 1
        timetemp$hour = 0
        timetemp$min = 0
        timetemp$sec = 0
        timetemp = unique(timetemp)

        ttt = timetemp[ length(timetemp) ]
        ttt$month = ttt$month + 1
        timetemp = c(timetemp, ttt)
    } else if (toscale == "annual"){
        timetemp = timestamp
        timetemp$mon = 1
        timetemp$mday = 1
        timetemp$hour = 0
        timetemp$min = 0
        timetemp$sec = 0
        timetemp = unique(timetemp)

        ttt = timetemp[ length(timetemp) ]
        ttt$year = ttt$year + 1
        timetemp = c(timetemp, ttt)
    }

    var = array(data = NA, dim = c( dim(variable)[1:(ndim-1)], length(temp) ))

    if (calc == "SUM"){

        for (i in 1:(length(timetemp)-1)){
            if (ndim == 3){
                var[,,i] = apply( variable[,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2), FUN = sum, na.rm = FALSE )
            } else if (ndim == 4) {
                var[,,,i] = apply( variable[,,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2), FUN = sum, na.rm = FALSE )
            }
        }
        
    } else if (calc == "MEAN"){

        for (i in 1:(length(timetemp)-1)){
            if (ndim == 3){
                var[,,i] = apply( variable[,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2), FUN = mean, na.rm = FALSE )
            } else if (ndim == 4) {
                var[,,,i] = apply( variable[,,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2), FUN = mean, na.rm = FALSE )
            }
        }
        
    }

    return(var)
}
