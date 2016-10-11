condense <- function( variable, timestamp, calc = c("MEAN","SUM","SD"), toscale = c("3-hourly","6-hourly","daily","monthly","annual") ){
    # timestamp and the last dimension of variable must have equal length
    # variable = [time], [y, x, time] or [y, x, levels, time]
    # timestamp = "%Y-%m-%d %H:%M:%S" or POSIXlt objects
    # use the timestamp to convert variable to daily/monthly/annual

    if (is.vector(variable)){
        variable = as.matrix(variable, ncol = length(variable))
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

    if (toscale == "3-hourly"){
        pt = c(0, 3, 6, 9, 12, 15, 18, 21)
        # the year-month-days-3hourly
        timetemp = timestamp
        for (i in 1:length(timetemp)){
            timetemp[i]$hour = pt[ pt <= timetemp[i]$hour & pt > ( timetemp[i]$hour - 3 ) ]
            timetemp[i]$min = 0
            timetemp[i]$sec = 0
        }
        timetemp = unique(timetemp)

        # append the last day + 3 hours (keep time zone)
        ttt = timetemp[ length(timetemp) ]
        ttt$hour = ttt$hour + 3
        timetemp = rbind(data.frame(x = timetemp), data.frame(x = ttt))[[1]]
    } else if (toscale == "6-hourly"){
        pt = c(0, 6, 12, 18)
        # the year-month-days-6hourly
        timetemp = timestamp
        for (i in 1:length(timetemp)){
            timetemp[i]$hour = pt[ pt <= timetemp[i]$hour & pt > ( timetemp[i]$hour - 6 ) ]
            timetemp[i]$min = 0
            timetemp[i]$sec = 0
        }
        timetemp = unique(timetemp)

        # append the last day + 6 hours
        ttt = timetemp[ length(timetemp) ]
        ttt$hour = ttt$hour + 6
        timetemp = rbind(data.frame(x = timetemp), data.frame(x = ttt))[[1]]
    } else if (toscale == "daily"){
        # the year-month-days
        timetemp = timestamp
        for (i in 1:length(timetemp)){
            timetemp[i]$hour = 0
            timetemp[i]$min = 0
            timetemp[i]$sec = 0
        }
        timetemp = unique(timetemp)

        # append the last day + 1
        ttt = timetemp[ length(timetemp) ]
        ttt$mday = ttt$mday + 1
        timetemp = rbind(data.frame(x = timetemp), data.frame(x = ttt))[[1]]
    } else if (toscale == "monthly"){
        timetemp = timestamp
        for (i in 1:length(timetemp)){
            timetemp[i]$mday = 1
            timetemp[i]$hour = 0
            timetemp[i]$min = 0
            timetemp[i]$sec = 0
        }
        timetemp = unique(timetemp)
        
        ttt = timetemp[ length(timetemp) ]
        ttt$mon = ttt$mon + 1
        timetemp = rbind(data.frame(x = timetemp), data.frame(x = ttt))[[1]]
    } else if (toscale == "annual"){
        timetemp = timestamp
        for (i in 1:length(timetemp)){
            timetemp$mon = 0
            timetemp$mday = 1
            timetemp$hour = 0
            timetemp$min = 0
            timetemp$sec = 0
        }
        timetemp = unique(timetemp)

        ttt = timetemp[ length(timetemp) ]
        ttt$year = ttt$year + 1
        timetemp = rbind(data.frame(x = timetemp), data.frame(x = ttt))[[1]]
    }

    var = array(data = NA, dim = c( dim(variable)[1:(ndim-1)], length(timetemp) -1 ))

    if (calc == "SUM"){

        for (i in 1:(length(timetemp)-1)){
            if (ndim == 3){
                var[,,i] = apply( variable[,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2), FUN = sum, na.rm = FALSE )
            } else if (ndim == 4) {
                var[,,,i] = apply( variable[,,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2,3), FUN = sum, na.rm = FALSE )
            }
        }

    } else if (calc == "MEAN"){

        for (i in 1:(length(timetemp)-1)){
            if (ndim == 3){
                var[,,i] = apply( variable[,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2), FUN = mean, na.rm = FALSE )
            } else if (ndim == 4) {
                var[,,,i] = apply( variable[,,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2,3), FUN = mean, na.rm = FALSE )
            }
        }

    } else if (calc == "SD"){

        for (i in 1:(length(timetemp)-1)){
            if (ndim == 3){
                var[,,i] = apply( variable[,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2), FUN = sd, na.rm = FALSE )
            } else if (ndim == 4) {
                var[,,,i] = apply( variable[,,, which( timestamp >= timetemp[i] & timestamp < timetemp[i+1] )], MARGIN = c(1,2,3), FUN = sd, na.rm = FALSE )
            }
        }

    }

    return(list(var = var, timestamp = timetemp[1:(length(timetemp)-1)]))
}
