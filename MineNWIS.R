

library(sf)
library(nhdplusTools)
library(mapview)
library(tidyverse)

gages2 <- st_read(dsn = "C:/Users/jbaldock/OneDrive - DOI/Desktop/Baldock-Temp/gagesII_9322_point_shapefile", layer = "gagesII_9322_sept30_2011")
mapview(gages2)

gages_class <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Desktop/Baldock-Temp/basinchar_and_report_sept_2011/GagesClass.csv") %>%
  filter(CLASS == "Ref")

gages2_ref <- gages2 %>% filter(STAID %in% gages_class$STAID)
mapview(gages2_ref)
mapview(gages2_ref %>% filter(DRAIN_SQKM <= 30))


nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-06037500")
us_gages <- navigate_nldi(nldi_feature = nldi_nwis, mode = "UT", data_source = "nwissite", distance_km = 20)
mapview(us_gages$origin, col.regions = list("red")) + mapview(us_gages$UT_nwissite)






