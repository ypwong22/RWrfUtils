# Not an excutable

lat "XLAT" # (Time, south_north, west_east) south is negative
lon "XLONG" # (Time, south_north, west_east) west is negative
lu "LU_INDEX" # land-use category

time "Times" # time in character string

u "U" # (Time, bottom_top, south_north, west_east_stag) [m s-1]
v "V" # (Time, bottom_top, south_north_stag, west_east) [m s-1]
t2 "T2" # (Time, south_north, west_east) [K] Temperature at 2m
psfc "PSFC" # (Time, south_north, west_east) [Pa] Surface pressure
u10 "U10" # (Time, south_north, west_east) [m s-1] U at 10m
v10 "V10" # (Time, south_north, west_east) [m s-1] V at 10m

hgt "(PH + PHB)/9.81" # (Time, bottom_top_stag, south_north, west_east) [m2 s-2] base-state + perturbation geopotential, divided by gravity
pr "P + PB" # (Time, bottom_top, south_north, west_east) [Pa] perturbation pressure + base state pressure

"Q2" # (west_east, south_north, Time) 2m water vapor mixing ratio
"QVAPOR" # (west_east, south_north, bottom_top, Time) water vapor mixing ratio at model levels

rainc "RAINC" # (Time, south_north, west_east) [mm] Total cumulus precipitation
rainsh "RAINSCH" # (Time, south_north, west_east) [mm] Total shallow cumulus precipitation
rainnc "RAINNC" # (Time, south_north, west_east) [mm] Accumulated total grid scale precipitation

landmask "LANDMASK" # (Time, south_north, west_east) [] 1 for land, 0 for water
lakemask "LAKEMASK" # (Time, south_north, west_east) [] 1 for lake, 0 for non-lake

sst "SST" # (Time, south_north, west_east) [K] Sea surface temperature
# sst_input "SST_INPUT" # (Time, south_north, west_east) [K] Sea surface temperature from WRFLOWINPUT - seems to be always zero

mapfactor "MAPFAC_M" # Map scale factor on mass grid

eta "ZNU" # (Time, bottom_top) [] eta values on half (mass) levels
eta_w "ZNW" # (Time, bottom_top) [] eta values on full (w) levels
