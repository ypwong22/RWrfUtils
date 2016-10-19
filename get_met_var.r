get_met_var <- function(ncid, varname, ...){

    # Surface variables are sub-set from the 1st dimension
    if (varname == "PSFC"){
        var = var.get.nc(ncid, "PRES",...) / 100 # [Pa] to [hPa]
    } else if (varname == "U10"){
        var = var.get.nc(ncid, "UU",...)
        # remove the south_north_stag
        if (length(dim(var)) == 3){
            var = 0.5 * (var[,1:(dim(var)[2]-1),] + var[,2:dim(var)[2],])
        } else if (length(dim(var)) == 4){
            var = 0.5 * (var[,1:(dim(var)[2]-1),,] + var[,2:dim(var)[2],,])
        }
    } else if (varname == "V10"){
        var = var.get.nc(ncid, "VV",...)
        # remove the west_east_stag
        if (length(dim(var)) == 3){
            var = 0.5 * (var[1:(dim(var)[1]-1),,] + var[2:dim(var)[2],,])
        } else if (length(dim(var)) == 4){
            var = 0.5 * (var[1:(dim(var)[1]-1),,,] + var[2:dim(var)[2],,,])
        }
    } else if (varname == "T2"){
        var = var.get.nc(ncid, "TT",...) - 273.15 # [K] to [oC]
    } else if (varname == "RH2"){
        var = var.get.nc(ncid, "RH",...) # unit: [%]
    } else if (varname == "SH2" | varname == "SH"){
        # will convert to SH
        var = var.get.nc(ncid, "RH",...) # unit: [%]
    } else {
        print(paste("Read",varname,"as is."))
        var = var.get.nc(ncid, varname,...)
    }

    if (varname == "PSFC" | varname == "U10" | varname == "V10" | varname == "T2" | varname == "RH2" | varname == "SH2"){
        # if 4 dimensions remain, retain only the 1st vertical level
        if (length(dim(var)) == 4){
            var = var[,,1,] # 3rd dimension dropped
        }
        # if time dimension was collapsed, add back
        if (length(dim(var)) == 2){
            var = array(var, dim=c(dim(var)[1], dim(var)[2], 1))
        }
    }

    if (varname == "PRES"){
        var = var / 100 # [Pa] to [hPa]
    }

    if (varname == "SH2"){
        # convert from relative humidity to specific humidity
        Rd = 287 # [J kg-1 K-1]
        Rv = 462 # [J kg-1 K-1]
        e0 = 6.11 # [hPa]
        T0 = 273.15 # [K]
        L = 2.5 * 10^6 # [J kg-1]

        T = var.get.nc(ncid, "TT", start = c(NA,NA,1,keeptime[1]), count = c(NA,NA,1,length(keeptime)), collapse = FALSE) # [K]
        P = var.get.nc(ncid, "PRES", start = c(NA,NA,1,keeptime[1]), count = c(NA,NA,1,length(keeptime)), collapse = FALSE) / 100 # [Pa] to [hPa]

        # if 4 dimensions remain, retain only the 1st vertical level
        if (length(dim(T)) == 4){
            T = T[,,1,] # 3rd dimension dropped
        }
        # if time dimension was collapsed, add back
        if (length(dim(T)) == 2){
            T = array(T, dim=c(dim(T)[1], dim(T)[2], 1))
        }
        if (length(dim(P)) == 4){
            P = P[,,1,] # 3rd dimension dropped
        }
        # if time dimension was collapsed, add back
        if (length(dim(P)) == 2){
            P = array(P, dim=c(dim(P)[1], dim(P)[2], 1))
        }

        es = e0 * exp( L / Rv * (1/T0 - 1/T) )

        ws = Rd/Rv * var * es / (P - var*es) # mixing ratio

        var = ws / (ws + 1)
    }

    if (varname == "SH"){
        # convert from relative humidity to specific humidity
        Rd = 287 # [J kg-1 K-1]
        Rv = 462 # [J kg-1 K-1]
        e0 = 6.11 # [hPa]
        T0 = 273.15 # [K]
        L = 2.5 * 10^6 # [J kg-1]

        T = var.get.nc(ncid, "TT", start = c(NA,NA,NA,keeptime[1]), count = c(NA,NA,NA,length(keeptime)), collapse = FALSE)
        P = var.get.nc(ncid, "PRES", start = c(NA,NA,NA,keeptime[1]), count = c(NA,NA,NA,length(keeptime)), collapse = FALSE) / 100 # [Pa] to [hPa]
        # if 4 dimensions remain, retain only the 1st vertical level
        if (length(dim(T)) == 4){
            T = T[,,1,] # 3rd dimension dropped
        }
        # if time dimension was collapsed, add back
        if (length(dim(T)) == 2){
            T = array(T, dim=c(dim(T)[1], dim(T)[2], 1))
        }
        if (length(dim(P)) == 4){
            P = P[,,1,] # 3rd dimension dropped
        }
        # if time dimension was collapsed, add back
        if (length(dim(P)) == 2){
            P = array(P, dim=c(dim(P)[1], dim(P)[2], 1))
        }

        es = e0 * exp( L / Rv * (1/T0 - 1/T) )

        ws = Rd/Rv * var * es / (P - var*es) # mixing ratio

        var = ws / (ws + 1)        
    }

    return(var)

}
