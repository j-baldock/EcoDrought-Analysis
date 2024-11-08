####---------------------------------------------#
#### Delineate watersheds for monitoring sites ####
####---------------------------------------------#

library(tidyverse)
library(sf)
library(mapview)
library(terra)
library(whitebox)


####---------------------------------------------#
#### DEM and site information                 ####
####---------------------------------------------#

# dummy dem to get crs
dumdem <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/DuckCreek_DEM_10m_nc.tif")

# site locations
dat <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/EcoDrought_SiteInformation.csv") %>%
  select(site_id, site_name, lat, long, basin, subbasin, region) #%>% filter(region == "Mass")

# view unique regions
regions <- unique(dat$region)
myregion <- regions[6]

# Set focal region and filter
dat2 <- dat %>% filter(region == myregion)

# create spatial data object
datsp <- vect(dat2, c("long", "lat"), crs = "+proj=longlat")
datsp <- terra::project(datsp, dumdem)

# view
mapview(st_as_sf(datsp), legend = F)

# write shapefile
writeVector(datsp, "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_Sites_SpatialLocations.shp", overwrite = TRUE)


# load appropriate DEMs
# 1. Mass
dem <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/WestBrook_DEM_10m_nc.tif")
# 2. Shen
dem <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/Shenandoah_DEM_10m_nc.tif")
# 3. Flat
dem1 <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/NorthForkFlathead_DEM_10m_nc.tif")
dem2 <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/MiddleForkFlathead_DEM_10m_nc.tif")
# 4. Shields
dem1 <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/DuckCreek_DEM_10m_nc.tif")
dem2 <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/Shields_DEM_10m_nc.tif")
# 5. Snake
dem1 <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/SpreadCreek_DEM_10m_nc.tif")
dem2 <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/Yellowstone_DEM_10m_nc.tif")
# 6. Oreg
dem <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/DonnerBlitzen_DEM_10m_nc.tif")

# merge if needed
dem <- merge(dem1, dem2)

# write out to working directory
writeRaster(dem, "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_DEM.tif", overwrite = TRUE)
dem <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_DEM.tif")
crs(dem)
plot(dem)


####---------------------------------------------#
#### Burn streams into dem                    ####
####---------------------------------------------#

# # buffer flowline and rasterize to burn into dem
# flowbuff <- buffer(flowline, width = 20)
# flowbuff.rast <- rast(ncols = ncol(dem), nrows = nrow(dem), ext(dem))
# res(flowbuff.rast) <- res(dem) # make sure resolution is identical
# flowbuff.rast <- terra::rasterize(x = flowbuff, y = flowbuff.rast)
# flowbuff.rast2 <- subst(flowbuff.rast, from = c(1, NaN), to = c(10, 0)) # reclassify
# crs(flowbuff.rast2) <- crs(basin)
# 
# # burn buffered flowline into dem
# dem_burn <- dem - flowbuff.rast2
# writeRaster(dem_burn, "Landscape Covariates/Watershed Delineation/Working/Snake_mask_dem_burn.tif", overwrite = TRUE)
# 
# # then burn streams into dem
# wbt_fill_burn(dem = "Landscape Covariates/Watershed Delineation/Working/Snake_mask_dem_burn.tif",
#               streams = "Landscape Covariates/Watershed Delineation/SnakeGreys_flowline.shp",
#               output = "Landscape Covariates/Watershed Delineation/Working/Snake_mask_dem_burn2.tif")


####---------------------------------------------#
#### Watershed delineation                    ####
####---------------------------------------------#

# breach and fill DEM depressions
wbt_breach_depressions_least_cost(dem = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_DEM.tif", 
                                  output = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_DEM_breached.tif", 
                                  dist = 5, fill = TRUE)
wbt_fill_depressions_wang_and_liu(dem = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_DEM_breached.tif", 
                                  output = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_DEM_breached_filled.tif")

# flow accumulation
wbt_d8_flow_accumulation(input = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_DEM_breached_filled.tif",
                         output = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_D8FA.tif")

# pointer grid
wbt_d8_pointer(dem = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_DEM_breached_filled.tif",
               output = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_D8pointer.tif")

# extract streams
wbt_extract_streams(flow_accum = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_D8FA.tif",
                    output = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_RasterStreams.tif",
                    threshold = 15000)

# check streams
wbt_raster_streams_to_vector(streams = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_RasterStreams.tif",
                             d8_pntr = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_D8pointer.tif",
                             output = paste("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Streams/", myregion, "_Streams.shp", sep = ""))
streams <- vect(paste("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Streams/", myregion, "_Streams.shp", sep = ""))
crs(streams) <- crs(dem)
mapview(streams) + mapview(st_as_sf(datsp))

# snap points to (raster) stream network
wbt_jenson_snap_pour_points(pour_pts = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_Sites_SpatialLocations.shp",
                            streams = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_RasterStreams.tif",
                            output = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_Sites_SpatialLocations_snapped.shp",
                            snap_dist = 0.005) 


# check correct snapping
pts.snap <- read_sf(dsn = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working", layer = "Working_Sites_SpatialLocations_snapped")
mapview(list(st_as_sf(streams), st_as_sf(datsp), st_as_sf(pts.snap)), col.regions = list("blue", "blue", "red"), col = list("blue", "blue", "red"), legend = F)

# iteratively delineate each watershed...allows for overlap
sites <- unique(pts.snap$site_id)
watershed.sec.list <- list()
st <- Sys.time()
for (i in 1:length(sites)) {
  pt.sub <- pts.snap[pts.snap$site_id == sites[i],]
  st_write(pt.sub, "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/single_temporary.shp", append = FALSE, delete_layer = TRUE)
  wbt_watershed(d8_pntr = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/Working_D8pointer.tif",
                pour_pts = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/single_temporary.shp",
                output = "C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/single_temporary_watershed.tif")
  ws <- rast("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Working/single_temporary_watershed.tif")
  watershed.sec.list[[i]] <- st_as_sf(as.polygons(ws))
  print(paste(i, " - ", sites[i], sep = ""))
}
et <- Sys.time()
et - st
watersheds.sec <- do.call(rbind, watershed.sec.list) %>% st_set_crs(crs(dem)) 
watersheds.sec <- watersheds.sec %>% mutate(site_id = sites, area_sqmi = as.numeric(st_area(watersheds.sec)/1000000)*0.386102)
mapview(list(watersheds.sec, pts.snap, st_as_sf(streams)), legend = F)
mapview(watershed.sec.list[[13]])
view(watersheds.sec[watersheds.sec$areasqkm<0.1,])


# save CSV of watershed elevation and area
areaelev <- tibble(watersheds.sec) %>% select(site_id, area_sqmi) %>% mutate(elev_ft = extract(x = dem, y = vect(pts.snap))[,2] * 3.28084) 
write_csv(areaelev, paste("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Area and Elevation/", myregion, "_AreaElevation.csv", sep = ""))

# save shapefile of watersheds
st_write(watersheds.sec, paste("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Watersheds/", myregion, "_Watersheds.shp", sep = ""), append = FALSE, delete_layer = TRUE)


