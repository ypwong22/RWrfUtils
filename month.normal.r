month.normal <- function(variable, timestamp, calc = c("MEAN","SUM","MAX","MIN","SD","FRQ","SD-W","IXX","IXX-W"), threshold = NULL){
    # timestamp and the last dimension of variable must have equal length
    # variable = [time], [y, x, time], or [y, x, levels, time]
    # FRQ & SD-W: for precipitation, frequency & standard deviation of wet-days only
    # IXX: replace XX by any number >0 & <100 to obtain quantiles; can be decimals
    # IXX-W: for precipitation, quantile on wet days only
    # threshold - used by precipitation unit:[mm], to remove small values

    # Note: the temporal resolution of the input is not changed, mind you are sending hourly or daily data!

    if (is.vector(variable)){
        variable = as.matrix(variable, nrow = length(variable))
        ndim = 1
        month.normal = c()
    } else {
        ndim = length(dim(variable))
        month.normal = array(data = NA, dim = c(dim(variable)[1:(ndim-1)], 12))
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

    if (!is.null(threshold)){
        variable = variable * (variable >= threshold)
    }

    if (calc == "MEAN"){
        if (ndim == 1){
            for (i in 1:12){
               month.normal[i] = mean(variable[timestamp$mon == i-1], na.rm=TRUE)
            }
        } else if (ndim == 3){
            for (i in 1:12){
                month.normal[,, i] = apply( variable[,, timestamp$mon == i-1], MARGIN = c(1,2), FUN = mean, na.rm=TRUE )
            }
        } else if (ndim == 4){
            for (i in 1:12){
                month.normal[,,, i] = apply( variable[,,, timestamp$mon == i-1], MARGIN = c(1,2,3), FUN = mean, na.rm=TRUE )
            }
        }
    } else if (calc == "SUM"){
        daysofmonth = c(31,28,31,30,31,30,31,31,30,31,30,31]
        if (ndim == 1){
            for (i in 1:12){
                month.normal[i] = mean(variable[timestamp$mon == i-1], na.rm=TRUE) * daysofmonth[i]
            }
        } else if (ndim == 3){
            for (i in 1:12){
                month.normal[,, i] = apply( variable[,, timestamp$mon == i-1], MARGIN = c(1,2), FUN = mean, na.rm=TRUE ) * daysofmonth[i]
            }
        } else if (ndim == 4){
            for (i in 1:12){
                month.normal[,,, i] = apply( variable[,,, timestamp$mon == i-1], MARGIN = c(1,2,3), FUN = mean, na.rm=TRUE ) * daysofmonth[i]
            }
        }
    } else if (calc == "MAX"){
        if (ndim == 1){
            for (i in 1:12){
                month.normal[i] = max(variable[timestamp$mon == i-1], na.rm=TRUE)
            }
        } else if (ndim == 3){
            for (i in 1:12){
                month.normal[,, i] = apply( variable[,, timestamp$mon == i-1], MARGIN = c(1,2), FUN = max, na.rm=TRUE )
            }
        } else if (ndim == 4){
            for (i in 1:12){
                month.normal[,,, i] = apply( variable[,,, timestamp$mon == i-1], MARGIN = c(1,2,3), FUN = max, na.rm=TRUE )
            }
        }
    } else if (calc == "MIN"){
        if (ndim == 1){
            for (i in 1:12){
                month.normal[i] = min(variable[timestamp$mon == i-1], na.rm=TRUE)
            }
        } else if (ndim == 3){
            for (i in 1:12){
                month.normal[,, i] = apply( variable[,, timestamp$mon == i-1], MARGIN = c(1,2), FUN = min, na.rm=TRUE )
            }
        } else if (ndim == 4){
            for (i in 1:12){
                month.normal[,,, i] = apply( variable[,,, timestamp$mon == i-1], MARGIN = c(1,2,3), FUN = min, na.rm=TRUE )
            }
        }
    } else if (calc == "SD"){
        if (ndim == 1){
            for (i in 1:12){
                month.normal[i] = sd(variable[timestamp$mon == i-1], na.rm=TRUE)
            }
        } else if (ndim == 3){
            for (i in 1:12){
                month.normal[,, i] = apply( variable[,, timestamp$mon == i-1], MARGIN = c(1,2), FUN = sd, na.rm=TRUE )
            }
        } else if (ndim == 4){
            for (i in 1:12){
                month.normal[,,, i] = apply( variable[,,, timestamp$mon == i-1], MARGIN = c(1,2,3), FUN = sd, na.rm=TRUE )
            }
        }
    } else if (calc == "FRQ"){
        if (ndim == 1){
            for (i in 1:12){
                month.normal[i] = sum( variable[timestamp$mon == i-1] ) / sum( !is.na(variable[timestamp$mon == i-1]) )
            }
        } else if (ndim == 3){
            for (i in 1:12){
                temp = variable[,, timestampe$mon == i-1] > 0
 
                month.normal[,, i] = apply( temp, MARGIN = c(1,2), FUN = sum, na.rm=TRUE ) / apply( !is.na(temp), MARGIN = c(1,2), FUN = sum, na.rm=TRUE )
            }
        } else if (ndim == 4){
            for (i in 1:12){
                temp = variable[,,, timestampe$mon == i-1] > 0

                month.normal[,,, i] = apply( temp, MARGIN = c(1,2), FUN = sum, na.rm=TRUE ) / apply( !is.na(temp), MARGIN = c(1,2), FUN = sum, na.rm=TRUE )
            }
        }
    } else if (calc == "SD-W"){
        if (ndim == 1){
            for (i in 1:12){
                temp = variable[timestamp$mon == i-1]
                month.normal[i] = sd( temp[ temp > 0 & !is.na(temp) ] )
            }
        } else if (ndim == 3){
            for (i in 1:12){
                for (j in 1:dim(temp)[1]){
                    for (k in 1:dim(temp)[2]){
                        temp = variable[j,k,timestamp$mon == i-1]
                        month.normal[j,k,i] = sd(temp[temp>0 & !is.na(temp)])
                    }
                }
            }
        } else if (ndim == 4){
            for (i in 1:12){
                for (j in 1:dim(temp)[1]){
                    for (k in 1:dim(temp)[2]){
                        for (m in 1:dim(temp)[3]){
                            temp = variable[j,k,m,timestamp$mon == i-1]
                            month.normal[j,k,m,i] = sd(temp[temp>0 & !is.na(temp)])
                        }
                    }
                }
            }
        }
    } else {
        dummy = strsplit( dummy, split = "" )
        if (dummy[1] == "I"){
            if (dummy[ length(dummy] ] == "W"){
                qtl = as.numeric( paste(dummy[2:(length(dummy)-2)], sep="") ) / 100
                if (ndim == 1){
                    for (i in 1:12){
                        temp = variable[timestamp$mon == i-1]
                        month.normal[i] = quantile( temp[ temp > 0 ], probs = qtl, na.rm =TRUE )
                    }
                } else if (ndim == 3){
                    for (i in 1:12){
                        for (j in 1:dim(temp)[1]){
                            for (k in 1:dim(temp)[2]){
                                temp = variable[j,k,timestamp$mon == i-1]
                                month.normal[j,k,i] = quantile(temp[temp>0], probs = qtl, na.rm = TRUE)
                            }
                        }
                    }
                } else if (ndim == 4){
                    for (i in 1:12){
                        for (j in 1:dim(temp)[1]){
                            for (k in 1:dim(temp)[2]){
                                for (m in 1:dim(temp)[3]){
                                    temp = variable[j,k,m,timestamp$mon == i-1]
                                    month.normal[j,k,m,i] = quantile(temp[temp>0], probs = qtl, na.rm = TRUE)
                                }
                            }
                        }
                    }
                }
            } else {
                qtl = as.numeric( paste(dummy[2:length(dummy)], sep="") ) / 100
                
                if (ndim == 1){
                    for (i in 1:12){
                        month.normal[i] = quantile(variable[timestamp$mon == i-1], probs = qtl, na.rm=TRUE)
                    }
                } else if (ndim == 3){
                    for (i in 1:12){
                        month.normal[,, i] = apply( variable[,, timestamp$mon == i-1], MARGIN = c(1,2), FUN = quantile, probs = qtl, na.rm=TRUE )
                    }
                } else if (ndim == 4){
                    for (i in 1:12){
                        month.normal[,,, i] = apply( variable[,,, timestamp$mon == i-1], MARGIN = c(1,2,3), FUN = quantile, probs = qtl, na.rm=TRUE )
                    }
                }
            }
        }
    }

    return(month.normal)
}
