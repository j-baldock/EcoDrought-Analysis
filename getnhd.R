library(nhdplusTools)
library(sf)
library(mapview)



48.079957, -114.118190




# get point
start_point <- st_sfc(st_point(c(-114.118190, 48.079957)), crs = 4269)
hu <- get_huc(st_sfc(st_point(c(-114.118190, 48.079957)), crs = 4269), type = "huc04")
hu$huc4

# download NHD (low res) to get extent
start_comid <- discover_nhdplus_id(start_point)
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 200)
mapview(flowline$UT_flowlines)

# get NHDplusHR
unlink(nhdplusTools_data_dir(), recursive = TRUE) # delete existing directory and contents
# work_dir <- file.path(nhdplusTools_data_dir(), "hr_v_cache")
# hr_gpkg <- file.path(work_dir, "hr_data.gpkg")
# download_nhdplushr(work_dir, "0102") # TRUE will download files.
# hr_data <- get_nhdplushr(work_dir, out_gpkg = hr_gpkg)
work_dir <- file.path(nhdplusTools_data_dir(), "hr_v_cache")
download_dir <- download_nhdplushr(work_dir, "1701")
#unlink(file.path(download_dir, "nhdplus_out.gpkg"))
hr_data <- get_nhdplushr(download_dir, file.path(download_dir, "nhdplus_out.gpkg"),
                         layers = c("NHDFlowline","NHDPlusBurnWaterbody"), 
                         check_terminals = FALSE)



# crop NHDplusHR to extent of low res NHD
mynet <- st_transform(hr_data$NHDFlowline, crs = st_crs(flowline$UT_flowlines))
mynet <- st_crop(mynet, flowline$UT_flowlines)
range(mynet$StreamOrde, na.rm = TRUE)

# check 
mapview(mynet, zcol = "StreamOrde")

# write to file
st_write(mynet, "C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Spatial data/NHD/nhdplushr_flathead.shp", append = FALSE)
