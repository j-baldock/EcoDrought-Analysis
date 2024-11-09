############################################################################
#
# Project: EcoDrought
# Purpose: Collate EcoDrought streamflow and temperature data
# Author: Jeff Baldock, jbaldock@usgs.gov
#
###########################################################################

library(tidyverse)
library(mapview)
library(sf)
library(zoo)
library(dataRetrieval)
library(nhdplusTools)
library(ggpubr)
library(fasstr)
library(dygraphs)
# library(xts)
library(ggforce)
library(smwrBase)


####--------------------------------------------###
#### Site information ####
####--------------------------------------------###

# West Brook
siteinfo_wb <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Raw data/Mass/MA_site_info.csv") %>%
  mutate(Station_No = factor(Station_No), Site_Name = factor(Site_Name)) %>%
  rename(site_id = Site_ID, station_no = Station_No, site_name = Site_Name, lat = Latitude_dec_deg, long = Longitude_dec_deg, elev_ft = Elevation_ft, area_sqmi = Drainage_Area_sqmi) %>%
  mutate(designation = "little", basin = "West Brook", region = "Mass") #%>% select(-c(elev_ft, area_sqmi)) 

# Shenandoah
siteinfo_shen <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Raw data/Virg/VA_site_info.csv") %>%
  mutate(Station_No = factor(Station_No), Site_Name = factor(Site_Name)) %>%
  rename(site_id = Site_ID, station_no = Station_No, site_name = Site_Name, lat = Latitude_dec_deg, long = Longitude_dec_deg, elev_ft = Elevation_ft, area_sqmi = Drainage_Area_sqmi) %>%
  mutate(designation = ifelse(str_detect(site_id, "10FL"), "big", "little"), 
         basin = str_sub(site_name, 1, str_length(site_name)-3), region = "Shen") #%>% select(-c(elev_ft, area_sqmi))

# Flathead/Muhlfeld
siteinfo_flat <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Raw data/Flathead/Flathead_SiteInfo_UpdateOct25.csv") %>% 
  select(basin, site_name, site_id, region, designation, lat, long) %>%
  rename(subbasin = basin) %>%
  mutate(basin = "Flathead", region = "Flat") %>%
  select(site_id, site_name, lat, long, designation, basin, subbasin, region) %>%
  filter(designation == "little")
  
# GYA/Al-Chokhachy
siteinfo_gya <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Raw data/Al-Chokhachy/Al-Chokhachy_sites.csv") %>%
  mutate(region = ifelse(basin == "Snake River", "Snake", "Shields"), 
         designation = "little") %>%
  select(site_id, site_name, latitude, longitude, designation, basin, region) %>% 
  rename(lat = latitude, long = longitude)
  

# NWIS Medium/Big/Super G
sites <- c("01169900", # South River at Conway, Massachusetts
           "13011500", # Pacific Creek, Snake River, Wyoming
           "06195600", # Shields River at Livingston, Montana
           "12355500", # North Fork Flathead River, Montana
           "10396000", # Donner Blitzen River near Frenchglen, Oregon
           # Medium G
           "12355347", # Big Creek (Flathead)
           "12355342", # Hallowat Creek (Flathead)
           "06192980", # Shields Rivera above Smith Creek (GYA)
           "06192900", # Dugout Creek Mouth (GYA)
           "13012475", # South Fork Spread Creek (GYA)
           "13012465", # Leidy Creek, lower (GYA)
           "01171100", # West Brook (Mass)
           "01171000",  # Avery Brook (Mass)
           "424551118503200", # Fish Creek at DB confluence (Oreg)
           "424547118503500", # DB above Fish Creek (Oreg)
           "424325118495900", # DB near Burnt Car Spring (Oreg)
           "424003118453700", # Little Blitzen River (Oreg)
           "423830118453200", # Indian Creek (Oreg)
           "423815118453900" # DB above Indian Creek (Oreg)
           )
siteinfo_nwis <- tibble(readNWISsite(sites)[,c(2:3,7,8,20,30)]) # get site info
names(siteinfo_nwis) <- c("station_no", "site_name", "lat", "long", "elev_ft", "area_sqmi") # rename columns
siteinfo_nwis <- siteinfo_nwis %>% mutate(site_name = c("South River Conway NWIS", 
                                                        "Avery Broook NWIS", 
                                                        "West Brook NWIS", 
                                                        "Dugout Creek NWIS", 
                                                        "Shields River ab Smith NWIS", 
                                                        "Shields River nr Livingston NWIS", 
                                                        "Donner Blitzen River nr Frenchglen NWIS", 
                                                        "Hallowat Creek NWIS", 
                                                        "Big Creek NWIS", 
                                                        "North Fork Flathead River NWIS", 
                                                        "Pacific Creek at Moran NWIS", 
                                                        "Leidy Creek Mouth NWIS", 
                                                        "SF Spread Creek Lower NWIS",
                                                        "Donner Blitzen ab Indian NWIS",
                                                        "Indian Creek NWIS",
                                                        "Little Blizten River NWIS",
                                                        "Donner Blitzen nr Burnt Car NWIS",
                                                        "Donner Blitzen ab Fish NWIS",
                                                        "Fish Creek"),
                                          site_id = c("SRC", "AVB", "WBR", "DUG", "SRS", "SRL", "DBF", "HAL", "BIG", "NFF", "PCM", "LEI", "SFS", "DBI", "IND", "LBL", "DBB", "DBA", "FSH"),
                                          designation = c("big", "medium", "medium", "medium", "medium", "big", "big", "medium", "medium", "big", "big", "medium", "medium", "medium", "medium", "medium", "medium", "medium", "medium"),
                                          basin = c("West Brook", "West Brook", "West Brook", "Shields River", "Shields River", "Shields River", "Donner Blitzen", "Flathead", "Flathead", "Flathead", "Snake River", "Snake River", "Snake River", "Donner Blitzen", "Donner Blitzen", "Donner Blitzen", "Donner Blitzen", "Donner Blitzen", "Donner Blitzen"),
                                          region = c("Mass", "Mass", "Mass", "Shields", "Shields", "Shields", "Oreg", "Flat", "Flat", "Flat", "Snake", "Snake", "Snake","Oreg", "Oreg", "Oreg", "Oreg", "Oreg", "Oreg")) %>% 
  select(site_id, site_name, lat, long, station_no, designation, basin, region, elev_ft, area_sqmi)
mapview(st_as_sf(siteinfo_nwis, coords = c("long", "lat"), crs = 4326))


# bind together, fill in ragged subbasin
siteinfo <- bind_rows(siteinfo_wb, siteinfo_shen, siteinfo_flat, siteinfo_gya, siteinfo_nwis)
siteinfo$subbasin[siteinfo$site_name == "Hallowat Creek NWIS"] <- "Big Creek"
siteinfo$subbasin[siteinfo$site_name == "Big Creek NWIS"] <- "Big Creek"
siteinfo <- siteinfo %>% mutate(subbasin = ifelse(is.na(subbasin), basin, subbasin))


# fix Shields River Valley Ranch site locations
siteinfo$lat[siteinfo$site_id == "SH07"] <- siteinfo$lat[siteinfo$site_id == "SRS"]
siteinfo$long[siteinfo$site_id == "SH07"] <- siteinfo$long[siteinfo$site_id == "SRS"]


# add elevation and area variables (from watershed delineation)
areafiles <- list.files("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Area and Elevation")
arealist <- list()
for (i in 1:length(areafiles)) { arealist[[i]] <- read_csv(paste("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Area and Elevation/", areafiles[i], sep = ""))}
areaelev <- do.call(rbind, arealist)
# how well do provided and delineation area/elevation match?
test <- siteinfo %>% left_join(areaelev, by = "site_id")
test %>% ggplot() + geom_point(aes(x = area_sqmi.x, y = area_sqmi.y)) + geom_abline(intercept = 0, slope = 1) + facet_wrap(~basin, scales = "free")
test %>% ggplot() + geom_point(aes(x = elev_ft.x, y = elev_ft.y)) + geom_abline(intercept = 0, slope = 1) + facet_wrap(~basin, scales = "free")
# add delineated variables
siteinfo <- siteinfo %>% select(-c(area_sqmi, elev_ft)) %>% left_join(areaelev)
# fix NF Flathead (no dem from Canada)
siteinfo$area_sqmi[siteinfo$site_id == "NFF"] <- 1556


# write out
write_csv(siteinfo, "C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_SiteInformation.csv")
siteinfo <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_SiteInformation.csv")
unique(siteinfo$basin)

# convert to spatial object and view on map
siteinfo_sp <- st_as_sf(siteinfo, coords = c("long", "lat"), crs = 4326)
mapview(siteinfo_sp, zcol = "designation")


####--------------------------------------------###
#### Load flow and temperature data ####
####--------------------------------------------###

# West Brook
dat_wb <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Raw data/Mass/EcoDrought Continuous_MA.csv") %>%
  mutate(Station_No = factor(Station_No), Site_Name = factor(Site_Name)) %>%
  rename(station_no = Station_No, site_name = Site_Name, datetime = DateTime_EST, height = GageHeight_Hobo_ft, flow = Discharge_Hobo_cfs, tempf = WaterTemperature_HOBO_DegF) %>%
  mutate(tempc = (tempf - 32)*(5/9)) %>% select(station_no, site_name, datetime, height, flow, tempc) %>%
  left_join(siteinfo %>% select(-station_no))
# # supplementary data from Jenn does not match data release
# dat_wb2 <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Raw data/Mass/predictions_and_observations.csv") %>% 
#   filter(!is.na(streamflow_cfs)) %>% select(site_number, site_name, gage_id, timestamp_aligned, streamflow_cfs) %>% mutate(gage_id = as.factor(gage_id)) %>%
#   rename(datetime = timestamp_aligned, flow = streamflow_cfs, station_no = gage_id)
# test <- dat_wb %>% left_join(dat_wb2 %>% select(station_no, datetime, flow), by = c("station_no" = "station_no", "datetime" = "datetime"))
# jpeg("./Explore Data/WestBrook_compare.jpg", height = 7, width = 9, units = "in", res = 500)
# test %>% ggplot(aes(x = flow.x, y = flow.y)) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "red") +
#   xlab("Flow (ScienceBase Data Release)") + ylab("Flow (predictions_and_observations.csv)") + 
#   facet_wrap(~site_name, scales = "free")
# dev.off()

# # pull West Brook 0 and Avery Brook Directly from NWIS
# sites <- c("01171100", "01171000")
# siteinfo_wb_nwis <- tibble(readNWISsite(sites)[,c(2:3,7,8,20,30)]) # get site info
# names(siteinfo_wb_nwis) <- c("station_no", "site_name", "lat", "long", "elev_ft", "area_sqmi") # rename columns
# siteinfo_wb_nwis <- siteinfo_wb_nwis %>% mutate(site_name = c("Avery Brook", "West Brook 0"),
#                                                 site_id = c("AB", "WM"),
#                                                 designation = c("little", "big"),
#                                                 basin = "West Brook",
#                                                 region = "Mass") %>% select(names(siteinfo_wb))
# mapview(st_as_sf(siteinfo_wb_nwis, coords = c("long", "lat"), crs = 4326))
# # extract daily mean discharge and temp data from USGS NWIS website
# dat_wb_nwis <- tibble(readNWISdata(sites = sites[1], parameterCd = c("00010", "00060"), service = "uv", startDate = "2020-01-01", endDate = "2023-12-31")[,c(2:4)])
# names(dat_wb_nwis) <- c("station_no", "datetime", "flow")
# # convert GMT (NWIS) to EST (data release)
# dat_wb_nwis <- dat_wb_nwis %>% mutate(station_no = as.factor(station_no), datetime = datetime - hours(4))
# # compare NWIS to data release, note discrepancies
# wbcomb <- dat_wb_nwis %>% 
#   left_join(dat_wb %>% select(station_no, datetime, flow), by = c("station_no" = "station_no", "datetime" = "datetime"))
# # 1:1 comparison
# jpeg("./Explore Data/WestBrook_CompareDataReleaseNWIS1.jpg", height = 5, width = 5, units = "in", res = 500)
# par(mar = c(4,4,1,1), mgp = c(2.5,1,0))
# plot(flow.x ~ flow.y, wbcomb, lim = c(0,700), ylim = c(0,700), xlab = "Data Release (cfs)", ylab = "NWIS (cfs)")
# abline(a = 0, b = 1, col = "red")
# legend("topleft", legend = c("1:1"), lty = 1, col = "red", bty = "n")
# dev.off()
# # time series
# jpeg("./Explore Data/WestBrook_CompareDataReleaseNWIS2.jpg", height = 5, width = 7, units = "in", res = 500)
# wbcomb %>% rename(NWIS = flow.x, DataRelease = flow.y) %>% gather(key = source, value = cfs, NWIS:DataRelease) %>% 
#   filter(year(datetime) %in% c(2020:2021) & month(datetime) == 3 & day(datetime) %in% c(1:12), station_no == "01171000") %>%
#   # filter(datetime >= "2020-03-01" & datetime <= "2020-03-12", station_no == "01171000") %>% 
#   ggplot() + geom_line(aes(x = datetime, y = cfs, color = source)) + facet_wrap(~year(datetime), ncol = 1, scales = "free")
# dev.off()


# Shenandoah
dat_shen <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Raw data/Virg/EcoDrought_Continuous_VA.csv") %>%
  mutate(Station_No = factor(Station_No), Site_ID = factor(Site_ID), Discharge_Hobo_cfs = as.numeric(Discharge_Hobo_cfs)) %>%
  rename(station_no = Station_No, site_id = Site_ID, datetime = DateTime_EST, height = GageHeight_Hobo_ft, flow = Discharge_Hobo_cfs, tempf = WaterTemperature_HOBO_DegF) %>%
  mutate(tempc = (tempf - 32)*(5/9)) %>% select(station_no, site_id, datetime, height, flow, tempc) %>%
  left_join(siteinfo)
# pull in Big G data separately (UVA long-term gage sites)
dat_shen_uva_q <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Raw data/Virg/Shen_BigG_Discharge_hourly_UVA.csv") %>%
  rename(flow = cfs)
dat_shen_uva_t <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Raw data/Virg/Shen_BigG_TempEtc_hourly_UVA.csv") %>%
  select(site_id, datetime, tempc_mean) %>% rename(tempc = tempc_mean)
dat_shen_uva <- dat_shen_uva_q %>% left_join(dat_shen_uva_t) %>% left_join(siteinfo)
# dat_shen_uva %>% ggplot(aes(x = datetime, y = tempc)) + geom_line() + facet_wrap(~site_id, nrow = 3)
# dat_shen_uva %>% ggplot(aes(x = datetime, y = log(flow))) + geom_line() + facet_wrap(~site_id, nrow = 3)
# bind usgs and uva data
dat_shen <- bind_rows(dat_shen, dat_shen_uva)


# Flathead/Muhlfeld
flatfiles <- list.files("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Raw data/Flathead/Export2/Continuous Data")
flatlist <- list()
for (i in 1:length(flatfiles)) { 
  print(flatfiles[i])
  flatlist[[i]] <- read_csv(paste("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Raw data/Flathead/Export2/Continuous Data/", flatfiles[i], sep = "")) %>%
    mutate(DateTime = mdy_hm(DateTime, tz = "MST"),
           site_name = gsub("EMA.csv", "", flatfiles[i]))
  }
dat_flat <- bind_rows(flatlist) %>% select(DateTime, GageHeightFT, DischargeCFS, TempF, TempC, site_name, DischargeReliability, TempReliability) %>% 
  rename(datetime = DateTime, height = GageHeightFT, flow = DischargeCFS, tempf = TempF, tempc = TempC) %>%
  mutate(DischargeReliability = as_factor(DischargeReliability),
         TempReliability = as_factor(TempReliability)) %>%
  mutate(tempf = ifelse(is.na(tempf), (tempc * (9/5)) + 32, tempf),
         tempc = ifelse(is.na(tempc), (tempf - 32) * (5/9), tempc)) %>%
  left_join(siteinfo) %>% select(-tempf)
# dat_flat %>% ggplot(aes(x = datetime, y = tempc)) + geom_line() + facet_wrap(~site_name)
# dat_flat %>% ggplot(aes(x = datetime, y = log(flow))) + geom_line(aes(color = DischargeReliability)) + facet_wrap(~site_name, scales = "free_y")

# Greater Yellowstone/Al-Chokhachy
gyafiles <- list.files("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Raw data/Al-Chokhachy/Al-Chokhachy data files")
gyalist <- list()
for (i in 1:length(gyafiles)) { 
  print(gyafiles[i])
  gyalist[[i]] <- read_csv(paste("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Raw data/Al-Chokhachy/Al-Chokhachy data files/", gyafiles[i], sep = "")) %>%
    mutate(date = mdy(date), DateTime = ymd_hms(paste(date, time, sep = " "), tz = "MST"), discharge = as.numeric(discharge))
}
# gyalist[[10]] %>% ggplot() + geom_line(aes(x = DateTime, y = discharge, color = location))
# gyalist[[6]] %>% ggplot() + geom_line(aes(x = DateTime, y = discharge, color = location))
# gyalist[[7]] %>% ggplot() + geom_line(aes(x = DateTime, y = discharge, color = location))
dat_gya <- bind_rows(gyalist) %>% select(DateTime, depth, discharge, temperature, location) %>% 
  rename(datetime = DateTime, height = depth, flow = discharge, tempc = temperature, site_name = location) %>%
  filter(site_name != "EF Henrys") %>% # drop weird duplicate site/year?
  mutate(site_name = recode(site_name,
                            "EF Above Confluence" = "EF Duck Creek ab HF",
                            "EF Below Confluence" = "EF Duck Creek be HF",
                            "NF Spread Creek" = "NF Spread Creek Lower",
                            "Upper NF Spread Creek" = "NF Spread Creek Upper",
                            "SF Spread Creek" = "SF Spread Creek Lower",
                            "Upper SF Spread Creek" = "SF Spread Creek Upper",
                            "Shields River above Dugout Creek" = "Shields River ab Dugout",
                            "Upper Leidy Creek" = "Leidy Creek Upper", 
                            "Leidy Creek" = "Leidy Creek Mouth",
                            "Spread Creek" = "Spread Creek Dam",
                            "Shields River above Smith Creek" = "Shields River Valley Ranch")) %>%
  left_join(siteinfo) %>% filter(tempc <= 100)
# dat_gya %>% ggplot(aes(x = datetime, y = tempc)) + geom_line() + facet_wrap(~site_name)
# dat_gya %>% ggplot(aes(x = datetime, y = log(flow))) + geom_line() + facet_wrap(~site_name, scales = "free_y")


####--------------------------------------------###
#### bind data ####
####--------------------------------------------###

# bind together
dat <- bind_rows(dat_wb, dat_shen, dat_flat, dat_gya)
unique(dat$site_name)
write_csv(dat, "C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_Raw.csv")
dat <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_Raw.csv")

unique(dat$designation)
unique(dat$region)
unique(dat$basin)
unique(dat$subbasin)


####--------------------------------------------###
#### Pull (daily) NWIS (Big G) data ####
####--------------------------------------------###

# extract daily mean discharge and temp data from USGS NWIS website
dat_superg_nwis <- tibble(readNWISdv(siteNumbers = sites, parameterCd = c("00010", "00060"), startDate = "1980-01-01", endDate = Sys.Date(), statCd = c("00003", "00001", "00002")))[,-c(1,5,7,9,11)]
names(dat_superg_nwis) <- c("station_no", "date", "tempc_max", "tempc_min", "tempc_mean", "flow_mean")
dat_superg_nwis <- dat_superg_nwis %>% left_join(siteinfo)

unique(dat_superg_nwis$basin)
unique(dat_superg_nwis$subbasin)
unique(dat_superg_nwis$region)
unique(dat_superg_nwis$designation)

# # calculate 7-day rolling mean
# dat_superg_nwis <- fill_missing_dates(dat_superg_nwis, dates = date, groups = site_name)
# dat_superg_nwis_7 <- dat_superg_nwis %>% mutate(year = year(date)) %>% 
#   group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation) %>%
#   mutate(tempc_mean_7 = rollapply(tempc_mean, FUN = mean, width = 7, align = "center", fill = NA),
#          flow_mean_7 = rollapply(flow_mean, FUN = mean, width = 7, align = "center", fill = NA)) %>%
#   ungroup() %>% filter(!is.na(site_id))
# 
# # plot data
dat_superg_nwis %>% filter(designation == "big") %>% ggplot() + geom_line(aes(x = date, y = log(flow_mean))) + facet_wrap(~site_name)
dat_superg_nwis %>% filter(designation == "medium") %>% ggplot() + geom_line(aes(x = date, y = log(flow_mean))) + facet_wrap(~site_name)
# 
# dat_superg_nwis_7 %>% filter(designation == "big") %>% ggplot() + geom_line(aes(x = date, y = (tempc_mean_7))) + facet_wrap(~site_name)
# dat_superg_nwis_7 %>% filter(designation == "medium") %>% ggplot() + geom_line(aes(x = date, y = (tempc_mean_7))) + facet_wrap(~site_name)


####--------------------------------------------###
#### Calculate daily means ####
####--------------------------------------------###

# daily flow/temp summaries
dat_daily <- dat %>% mutate(date = as_date(datetime)) %>% 
  group_by(station_no, site_name, site_id, basin, subbasin, region, lat, long, elev_ft, area_sqmi, designation, date) %>% 
  summarize(disch_reli = max(DischargeReliability),
            temp_reli = max(TempReliability),
            flow_mean = mean(flow), flow_min = min(flow), flow_max = max(flow),
            tempc_mean = mean(tempc), tempc_min = min(tempc), tempc_max = max(tempc)) %>%
  arrange(region, basin, site_name, date) %>%
  ungroup()

# cbind EcoD and NWIS datasets
dat_daily <- bind_rows(dat_daily %>% select(-flow_min, -flow_max, -tempc_min, -tempc_max), 
                       dat_superg_nwis %>% select(-tempc_max, -tempc_min),
                       dat_superg_nwis %>% filter(site_id == "SRL") %>% mutate(subbasin = "Duck Creek") %>% select(-tempc_max, -tempc_min))

# add missing dates
dat_daily <- fill_missing_dates(dat_daily, dates = date, groups = site_name)

# explore daily times series data
dat_daily %>% filter(site_name == "CoalCreekLower") %>% select(date, flow_mean) %>% dygraph() %>% dyRangeSelector()


####--------------------------------------------###
#### Interpolate missing data ####
####--------------------------------------------###

# explore data gaps
mysites <- unique(dat_daily$site_name)
mynas <- list()
for (i in 1:length(mysites)) {
  mydisch <- unlist(dat_daily$flow_mean[dat_daily$site_name == mysites[i]])
  runsna <- rle(is.na(mydisch))
  mynas[[i]] <- tibble(site_name = mysites[i], run = runsna$lengths[runsna$values == TRUE])
}
mynas <- do.call(rbind, mynas)
mynas %>% ggplot() + geom_histogram(aes(x = run))
mynas %>% filter(run < 100) %>% ggplot() + geom_histogram(aes(x = run))
mynas %>% filter(run < 100) %>% ggplot() + geom_histogram(aes(x = run)) + facet_wrap_paginate(~site_name, nrow = 4, ncol = 4, page = 1)
mynas %>% filter(run < 100) %>% ggplot() + geom_histogram(aes(x = run)) + facet_wrap_paginate(~site_name, nrow = 4, ncol = 4, page = 2)
mynas %>% filter(run < 100) %>% ggplot() + geom_histogram(aes(x = run)) + facet_wrap_paginate(~site_name, nrow = 4, ncol = 4, page = 3)
mynas %>% filter(run < 100) %>% ggplot() + geom_histogram(aes(x = run)) + facet_wrap_paginate(~site_name, nrow = 4, ncol = 4, page = 4)


# fill short gaps (<=14 days...2x smoothing period) using time series interpolation
datalist <- list()
for (i in 1:length(mysites)) { datalist[[i]] <- dat_daily %>% filter(site_name == mysites[i]) %>% mutate(flow_mean_filled = fillMissing(flow_mean, max.fill = 14, span = 100)) }
# bind and check 1:1
dat_daily_fill <- do.call(rbind, datalist)
dat_daily_fill %>% ggplot() + geom_point(aes(x = flow_mean, y = flow_mean_filled)) + facet_wrap(~site_name, scales = "free")
# explore individual sites
dat_daily_fill %>% filter(site_name == "LangfordCreekLower") %>% select(date, flow_mean, flow_mean_filled) %>% dygraph() %>% dyRangeSelector() %>% 
  dySeries("flow_mean", strokeWidth = 5, color = "black") %>% dySeries("flow_mean_filled", strokeWidth = 1, color = "red")



####--------------------------------------------###
#### Calculate 7-day means ####
####--------------------------------------------###

dat_daily_fill <- dat_daily_fill %>%
  group_by(site_name) %>%
  mutate(flow_mean_7 = rollapply(flow_mean, FUN = mean, width = 7, align = "center", fill = NA),
         flow_mean_filled_7 = rollapply(flow_mean_filled, FUN = mean, width = 7, align = "center", fill = NA),
         tempc_mean_7 = rollapply(tempc_mean, FUN = mean, width = 7, align = "center", fill = NA)) %>%
  ungroup() %>% filter(!is.na(site_id))

dat_daily_fill %>% filter(site_name == "Rock Creek") %>% select(date, flow_mean_filled, flow_mean_filled_7) %>% dygraph() %>% dyRangeSelector() %>% 
  dySeries("flow_mean_filled", strokeWidth = 5, color = "black") %>% dySeries("flow_mean_filled_7", strokeWidth = 1, color = "red")

unique(dat_daily_fill$basin)
unique(dat_daily_fill$subbasin)
unique(dat_daily_fill$region)
unique(dat_daily_fill$disch_reli)
unique(dat_daily_fill$temp_reli)

# write out
write_csv(dat_daily_fill, "C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_DailyWeekly.csv")
dat_daily <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_DailyWeekly.csv")



####--------------------------------------------###
#### data availability ####
####--------------------------------------------###

# summarize data availability
dat_summ <- dat_daily %>% filter(site_id != "MRN") %>%
  group_by(basin, date, designation) %>% summarize(numflow = sum(!is.na(flow_mean)), numtemp = sum(!is.na(tempc_mean))) %>% 
  gather(type, avail, numflow:numtemp) %>% mutate(type2 = as.factor(paste(designation, type, sep = "_"))) %>% 
  mutate(type3 = as.numeric(type2), avail = na_if(avail, 0)) %>% ungroup() %>% filter(!is.na(avail))
levels(dat_summ$type2)
unique(dat_summ$basin)

# plot all years
jpeg("./Data Availability/DataAvailability_byBasin.jpg", height = 6, width = 12, units = "in", res = 500)
dat_summ %>% ggplot(aes(x = date, y = type3)) + 
  geom_errorbarh(aes(xmax = date, xmin = date, color = avail), size = 0.001) +
  scale_color_continuous(trans = "reverse", na.value = "grey60") +
  scale_y_discrete(limits = c("Big G flow", "Big G temp", "Medium g flow", "Medium g temp", "Little g flow", "Little g temp")) + 
  labs(colour = "Number \nof sites") + ylab("") + xlab("Date") + 
  facet_wrap(~ basin, nrow = 4,
             labeller = as_labeller(c(`West Brook` = "West Brook: G = South River Conway (NWIS)", 
                                      `Staunton River` = "Staunton River: G = Staunton River 10 (UVA)",
                                      `Paine Run` = "Paine Run: G = Paine Run 10 (UVA)",
                                      `Piney River` = "Piney River: G = Piney River 10 (UVA)",
                                      `Snake River` = "Snake River: G = Pacific Creek Moran (NWIS)",
                                      `Shields River` = "Shields River: G = Shields River Livingston (NWIS)",
                                      `Flathead` = "NF Flathead River: G = NF Flathead (NWIS)",
                                      `Donner Blitzen` = "Donner Blitzen River: G = DB Frenchglen (NWIS)",
                                      `Duck Creek` = "Duck Creek: G = Shields River Livingston (NWIS)")))
dev.off()


# plot recent years
jpeg("./Data Availability/DataAvailability_byBasin_recent.jpg", height = 6, width = 12, units = "in", res = 500)
dat_summ %>% filter(date >= "2018-10-01") %>% ggplot(aes(x = date, y = type3)) + 
  geom_errorbarh(aes(xmax = date, xmin = date, color = avail), size = 0.001) +
  scale_color_continuous(trans = "reverse", na.value = "grey60") +
  scale_y_discrete(limits = c("Big G flow", "Big G temp", "Medium g flow", "Medium g temp", "Little g flow", "Little g temp")) + 
  labs(colour = "Number \nof sites") + ylab("") + xlab("Date") + 
  facet_wrap(~ basin, nrow = 4,
             labeller = as_labeller(c(`West Brook` = "West Brook: G = South River Conway (NWIS)", 
                                      `Staunton River` = "Staunton River: G = Staunton River 10 (UVA)",
                                      `Paine Run` = "Paine Run: G = Paine Run 10 (UVA)",
                                      `Piney River` = "Piney River: G = Piney River 10 (UVA)",
                                      `Snake River` = "Snake River: G = Pacific Creek Moran (NWIS)",
                                      `Shields River` = "Shields River: G = Shields River Livingston (NWIS)",
                                      `Flathead` = "NF Flathead River: G = NF Flathead (NWIS)",
                                      `Donner Blitzen` = "Donner Blitzen River: G = DB Frenchglen (NWIS)",
                                      `Duck Creek` = "Duck Creek: G = Shields River Livingston (NWIS)")))
dev.off()



####--------------------------------------------###
#### compare co-located gages ####
####--------------------------------------------###

sort(unique(dat_daily$site_name))

# WEST BROOK
p1 <- dat_daily %>% filter(site_name == "West Brook 0") %>% select(date, flow_mean_7) %>% rename(little = flow_mean_7) %>% 
  left_join(dat_daily %>% filter(site_name == "West Brook NWIS") %>% select(date, flow_mean_7) %>% rename(medium = flow_mean_7)) %>%
  ggplot() + geom_point(aes(x = log(medium), y = log(little))) + geom_abline(intercept = 0, slope = 1, color = "red")

p2 <- dat_daily %>% filter(site_name == "Avery Brook") %>% select(date, flow_mean_7) %>% rename(little = flow_mean_7) %>% 
  left_join(dat_daily %>% filter(site_name == "Avery Broook NWIS") %>% select(date, flow_mean_7) %>% rename(medium = flow_mean_7)) %>%
  ggplot() + geom_point(aes(x = log(medium), y = log(little))) + geom_abline(intercept = 0, slope = 1, color = "red")

# FLATHEAD
p3 <- dat_daily %>% filter(site_name == "BigCreekMiddle") %>% select(date, flow_mean_7) %>% rename(little = flow_mean_7) %>% 
  left_join(dat_daily %>% filter(site_name == "Big Creek NWIS") %>% select(date, flow_mean_7) %>% rename(medium = flow_mean_7)) %>%
  ggplot() + geom_point(aes(x = log(medium), y = log(little))) + geom_abline(intercept = 0, slope = 1, color = "red")


jpeg("./Data Availability/LittleMedium_Co-Located_Gages.jpg", height = 6, width = 6, units = "in", res = 500)
ggarrange(p1, p2, p3, ncol = 2, nrow = 2, labels = c("West Brook 0", "Avery Brook", "Big Creek (Flathead)"))
dev.off()



####--------------------------------------------###
#### Donner-Blitzen River ####
#### Note: very limited data availability. Really only June-October for ~4 years for little g sites (continuous data for Big G site). 
#### Unclear whether period of record captures annual high/low flows...thus, not really helpful?
####--------------------------------------------###

# get site information
sites <- c("10396000", # Donner Blitzen near Frenchglen
           "424551118503200", # Fish Creek at DB confluence
           "424547118503500", # DB above Fish Creek
           "424325118495900", # DB near Burnt Car Spring
           "424003118453700", # Little Blitzen River
           "423830118453200", # Indian Creek
           "423815118453900") # DB above Indian Creek
siteinfo_db <- tibble(readNWISsite(sites)[,c(2:3,7,8,20,30)]) # get site info
names(siteinfo_db) <- c("station_no", "site_name", "lat", "long", "elev_ft", "area_sqmi") # rename columns
siteinfo_db <- siteinfo_db %>% mutate(site_name = c("Donner Blitzen Lower", "Donner Blitzen above Indian", "Indian Creek", "Little Blitzen", "Donner Blitzen near Burnt Car Spring", "Donner Blitzen above Fish", "Fish Creek"),
                                      site_id = c("DB0", "DBI", "INC", "LBL", "DBS", "DBF", "FSC"),
                                      designation = c("big", rep("little", 6)),
                                      basin = "Donner Blitzen",
                                      region = "Oreg") %>% select(names(siteinfo_wb))
mapview(st_as_sf(siteinfo_db, coords = c("long", "lat"), crs = 4326))

# extract daily mean discharge and temp data from USGS NWIS website
dat_db <- tibble(readNWISdv(siteNumbers = sites, parameterCd = c("00060", "00010"), statCd = c("00001", "00002", "00003"), startDate = "2018-01-01", endDate = "2024-01-01"))[,c(2,3,4,6,8,10)]
# hourly data takes forever to download. Note: data is really only available June - October
# dat_db <- readNWISdata(sites = sites, parameterCd = c("00010", "00060"), service = "uv", startDate = "2018-01-01", endDate = "2023-01-01")

# plot daily data...note very limited availability
dat_db %>% ggplot(aes(x = Date, y = X_00060_00003)) + geom_line() + facet_wrap(~site_no)
dat_db %>% ggplot(aes(x = Date, y = X_00010_00003)) + geom_line() + facet_wrap(~site_no)
























####--------------------------------------------###
#### Explore ####
####--------------------------------------------###




unique(matdat_daily$site_name)

# 1 site
madat_weekly %>% filter(site_name == "West Brook Lower") %>% ggplot(aes(x = date, y = log(flow_mean_7))) + geom_line()

# all sites
madat_weekly %>% filter(year(date) == 2020) %>%
  ggplot(aes(x = date, y = log(flow_mean_7 + 0.001), group = site_name, col = site_name)) + geom_line()

madat_daily %>% filter(year(date) == 2020) %>%
  ggplot(aes(x = date, y = (flow_mean), group = site_name, col = site_name)) + geom_line()




sevendaymin <- madat_weekly %>% 
  filter(year(date) == 2020) %>%
  group_by(site_name) %>% 
  filter(flow_mean_7 == min(flow_mean_7, na.rm = T)) %>% 
  summarize(flow_mean_7 = mean(flow_mean_7),
            date = mean(date),
            designation = unique(designation))
range()
boxplot(sevendaymin$date)
points(jitter(sevendaymin$date))

sevendaymin %>% ggplot(aes(x = designation, y = date)) + geom_violin() + geom_jitter()
