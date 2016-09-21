interp_vert <- function(datain, pr, interp_levels, case){
    # after "wrf_interp.F90" - SUBROUTINE special_intrp, SUBROUTINE intrp_value
    nx = dim(datain)[1]
    ny = dim(datain)[2]
    nz = dim(datain)[3] # non-staggered
    nt = dim(datain)[4]

    rgas = 287.04 # J/K/kg
    ussalr = .0065 # deg C per m
    sclht = rgas * 256./9.81

    data_interp = array(data = NA, dim = c(nx,ny,length(interp_levels),nt))

    for (i in 1:nx){
        for (j in 1:ny){
            for (k in 1:nt){
                dummy = pr[i,j,,k] / 100 # convert to hPa
                for (levs in 1:length(interp_levels)){
                    a = min( which( dummy < interp_levels[levs] ) )
                    b = max( which( dummy > interp_levels[levs] ) )

                    if (case == 1){
                        warning("Interpolating as hgt")
                        valp1 = exp( - datain[i,j,a,k] / sclht )
                        valp0 = exp( - datain[i,j,b,k] / sclht )
                        
                        rvalue = (interp_levels[levs] - dummy[b])*(valp1 - valp0) / (dummy[a] - dummy[b]) + valp0

                        data_interp[i,j,levs,k] = -sclht * log(rvalue)
                    } else {
                        data_interp[i,j,levs,k] = (interp_levels[levs] - dummy[b])*(datain[i,j,a,k] - datain[i,j,b,k]) / (dummy[a] - dummy[b]) + datain[i,j,b,k]
                    }
                }
            }
        }
    }

    return( data_interp )
}

################################################################
################################################################
get_u <- function(ncid){
    # U (Time, bottom_top, south_north, west_east_stag) [m s-1]
    # average from the staggered grid to mass grid
    # note: U at 10m is not staggered (Time, south_north, west_east) [m s-1]
    temp = var.get.nc(ncid, "U")
    nx = dim(temp)[1]
    return( 0.5 * ( temp[1:(nx-1),,,] + temp[2:nx,,,] ) )
}
get_v <- function(ncid){
    # V (Time, bottom_top, south_north_stag, west_east) [m s-1]
    # average from the staggered grid to mass grid
    # note: V at 10m is not staggered (Time, south_north, west_east) [m s-1]
    temp = var.get.nc(ncid, "V")
    ny = dim(temp)[2]
    return( 0.5 * ( temp[,1:(ny-1),,] + temp[,2:ny,,] ) )
}
################################################################
################################################################

get_hgt <- function(ncid){
  # after "wrf_interp.F90"
  # hgt is on staggered levels, interpolate to mass levels
  # did not flip vertically

    temp = ( var.get.nc(ncid, "PH") + var.get.nc(ncid, "PHB") )/9.81

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
}
