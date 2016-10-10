get_wrf_var <- function(ncid, varname, ...){
    require("RNetCDF")

    if (varname == "hgt"){

        # after "wrf_interp.F90"
        # hgt is on staggered levels, interpolate to mass levels
        # did not flip vertically
        temp = ( var.get.nc(ncid, "PH", ...) + var.get.nc(ncid, "PHB", ...) )/9.81
        # Since ZNU & ZNW are time-invariant we only need the first time
        znu = var.get.nc(ncid, "ZNU")[,1]
        znw = var.get.nc(ncid, "ZNW")[,1]

        nx = dim(temp)[1]
        ny = dim(temp)[2]
        nz = dim(temp)[3] - 1
        nt = dim(temp)[4]

        hgt = array(data = NA, dim = c(nx,ny,nz,nt))
        for (i in 1:nz){
            fac = (znw[i] - znu[i]) / (znw[i] - znw[i+1])
            hgt[,,i,] = fac * temp[,,i+1,] + (1 - fac) * temp[,,i,]
        }
        return( hgt ) # unit: [m] (west_east, south_north, bottom_top, Time)

    } else if (varname == "U"){
        
        # U (Time, bottom_top, south_north, west_east_stag) [m s-1]
        # average from the staggered grid to mass grid
        # note: U at 10m is not staggered (Time, south_north, west_east) [m s-1]
        temp = var.get.nc(ncid, "U", ...)    
        nx = dim(temp)[1]
        return( 0.5 * ( temp[1:(nx-1),,,] + temp[2:nx,,,] ) )
        
    } else if (varname == "V"){
        
        # V (Time, bottom_top, south_north_stag, west_east) [m s-1]
        # average from the staggered grid to mass grid
        # note: V at 10m is not staggered (Time, south_north, west_east) [m s-1]
        temp = var.get.nc(ncid, "V", ...)
        ny = dim(temp)[2]
        return( 0.5 * ( temp[,1:(ny-1),,] + temp[,2:ny,,] ) )
        
    } else if (varname == "U10"){
        
        return( var.get.nc(ncid, "U10", ...) )
        
    } else if (varname == "V10"){
        
        return( var.get.nc(ncid, "V10", ...) )
        
    } else if (varname == "PR"){

        # pressure convert from [Pa] to [hPa] (=milibar)
        return( (var.get.nc(ncid, "P", ...) + var.get.nc(ncid, "PB", ...))/100 )

    } else if (varname == "TT"){
        
        # T (west_east, south_north, bottom_top, Time) [K]
        # - perturbation potential temperature
        # Total potential temperature = T + 300 [K]
        # Convert to temperature using Holton 2004 (2.44)
        pot = var.get.nc(ncid,"T", ...) + 300
        ps = 1000 # [hPa]
        p = get_wrf_var(ncid, "PR", ...)
        Rd = 287 # [J K-1 kg-1]
        cp = 1004 # [J K-1 kg-1]
        return( pot * (p/ps)^(Rd/cp) - 273.15 ) # unit: oC
        
    } else if (varname == "T2"){

        # 2m temperature
        return( var.get.nc(ncid, "T2", ...) - 273.15 ) # unit: oC
        
    } else if (varname == "RH"){

        # QVAPOR (west_east, south_north, bottom_top, Time)
        # - water vapor mixing ratio [kg kg-1]
        # Convert to relative humidity using temperature and pressure
        Rd = 287 # [J kg-1 K-1]
        Rv = 462 # [J kg-1 K-1]
        e0 = 6.11 # [hPa]
        T0 = 273.15 # [K]
        L = 2.5 * 10^6 # [J kg-1]

        T = get_wrf_var(ncid, "TT", ...)
        P = get_wrf_var(ncid, "PR", ...)
        r = var.get.nc(ncid, "QVAPOR", ...)
        
        es = e0 * exp( L / Rv * (1/T0 - 1/T) )

        return( r*Rv*P / (r*Rv + Rd) / es * 100 ) # convert to [%]

    } else if (varname == "RH2"){

        # Q2 (west_east, south_north, Time)
        # - water vapor mixing ratio [kg kg-1]
        # Convert to relative humidity using temperature and pressure
        Rd = 287 # [J kg-1 K-1]
        Rv = 462 # [J kg-1 K-1]
        e0 = 6.11 # [hPa]
        T0 = 273.15 # [K]
        L = 2.5 * 10^6 # [J kg-1]

        T2 = get_wrf_var(ncid, "T2", ...)
        PSFC = get_wrf_var(ncid, "PSFC", ...)
        r = var.get.nc(ncid, "Q2", ...)
        
        es = e0 * exp( L / Rv * (1/T0 - 1/T2) )

        return( r*Rv*PSFC / (r*Rv + Rd) / es * 100) # convert to [%]

    }else if (varname == "SH"){

        q = var.get.nc(ncid, "QVAPOR", ...)
        return( q / (1+q) )

    } else if (varname == "SH2"){

        q = var.get.nc(ncid, "Q2", ...)
        return( q / (1+q) )

    }else if (varname == "PSFC"){

        # surface temperature - convert from [Pa] to [hPa] (=milibar)
        # Note: NOT sea level pressure!!!!
        return( var.get.nc(ncid,"PSFC", ...) / 100 )

    }
}
