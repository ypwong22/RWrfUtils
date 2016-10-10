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
                dummy = pr[i,j,,k] # input be hPa
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
