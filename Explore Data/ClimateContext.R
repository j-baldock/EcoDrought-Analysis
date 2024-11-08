############################################################################
#
# Project: EcoDrought
# Purpose: Explore climatic context of EcoDrought years
# Author: Jeff Baldock, jbaldock@usgs.gov
#
###########################################################################

library(tidyverse)
library(daymetr)
library(zoo)
library(RColorBrewer)
library(sf)
library(mapview)
library(fasstr)
library(nhdplusTools)
library(SPEI)


####--------------------------------------------###
#### Site information and daily data ####
####--------------------------------------------###

# site information and locations
siteinfo <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/EcoDrought_SiteInformation.csv")
siteinfo_sp <- st_as_sf(siteinfo, coords = c("long", "lat"), crs = 4326)
mapview(siteinfo_sp, zcol = "designation")

# flow (and temp) data
dat <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_DailyWeekly.csv")

# aggregate by month and calculate monthly standardized mean/min flow 
dat2 <- dat %>% 
  filter(designation == "big") %>% 
  mutate(year = year(date), month = month(date)) %>%
  group_by(site_name, basin, year, month) %>% 
  summarize(flow_mean_monthly = mean(log(flow_mean+0.01)),
            flow_min_monthly = min(log(flow_mean+0.01))) %>%
  ungroup() %>%
  group_by(site_name, basin, month) %>%
  mutate(z_flow_mean_monthly = scale((flow_mean_monthly), center = TRUE, scale = TRUE)[,1],
         z_flow_min_monthly = scale((flow_min_monthly), center = TRUE, scale = TRUE)[,1]) %>%
  ungroup()

# monthly mean log streamflow over time, by site
dat2 %>% ggplot() + 
  geom_line(aes(x = year, y = flow_mean_monthly, group = month, color = month)) + 
  facet_wrap(~site_name, scales = "free_y")

# standardized monthly mean log streamflow over time, by site
dat2 %>% ggplot() + 
  geom_line(aes(x = year, y = z_flow_mean_monthly, group = month, color = month)) + 
  facet_wrap(~site_name)

# monthly minimum log streamflow over time, by site
dat2 %>% ggplot() + 
  geom_line(aes(x = year, y = flow_min_monthly, group = month, color = month)) + 
  facet_wrap(~site_name, scales = "free_y")

# standardized monthly mean log streamflow over time, by site
dat2 %>% ggplot() + 
  geom_line(aes(x = year, y = z_flow_min_monthly, group = month, color = month)) + 
  facet_wrap(~site_name)

# correlation between mean and minimum
# correlation is strong and does not differ markedly among sites or months
dat2 %>% 
  ggplot(aes(x = z_flow_mean_monthly, y = z_flow_min_monthly, group = month, color = month)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site_name)



# # pull Big/Super G for other basins
# sites <- c("01171500", # Mill River at Northampton, Massachusetts
#            "13011500", # Pacific Creek, Snake River, Wyoming
#            "06195600", # Shields River at Livingston, Montana
#            "12355500", # North Fork Flathead River, Montana
#            "10396000") # Donner Blitzen River near Frenchglen, Oregon
# siteinfo_superg <- tibble(readNWISsite(sites)[,c(2:3,7,8,20,30)]) # get site info
# names(siteinfo_superg) <- c("station_no", "site_name", "lat", "long", "elev_ft", "area_sqmi") # rename columns
# siteinfo_superg <- siteinfo_superg %>% mutate(site_name = c("Mill River Northampton", "Shields River Livingston", "Donner Blitzen River Frenchglen", "North Fork Flathead River", "Pacific Creek at Moran"),
#                                               site_id = c("MRN", "SRL", "DBF", "NFF", "PCM"),
#                                               designation = c("big", "big", "big", "big", "big"),
#                                               basin = c("West Brook", "Shields", "Donner Blitzen", "Flathead", "Snake"),
#                                               region = c("Mass", "Rock", "Oreg", "Rock", "Rock")) %>% select(names(siteinfo))
# mapview(st_as_sf(siteinfo_superg, coords = c("long", "lat"), crs = 4326))
# # extract daily mean discharge and temp data from USGS NWIS website
# dat_superg_nwis <- tibble(readNWISdv(siteNumbers = sites, parameterCd = c("00010", "00060"), startDate = "1980-01-01", endDate = "2023-12-31", statCd = c("00003", "00001", "00002")))[,-c(1,5,7,9,11)]
# names(dat_superg_nwis) <- c("station_no", "date", "tempc_max", "tempc_min", "tempc_mean", "flow_mean")
# dat_superg_nwis <- dat_superg_nwis %>% left_join(siteinfo_superg)
# 
# # calculate 7-day rolling mean
# dat_superg_nwis_7 <- dat_superg_nwis %>% mutate(year = year(date)) %>% 
#   group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation) %>%
#   mutate(tempc_mean_7 = rollapply(tempc_mean, FUN = mean, width = 7, align = "center", fill = NA),
#          flow_mean_7 = rollapply(flow_mean, FUN = mean, width = 7, align = "center", fill = NA)) %>%
#   ungroup() 


# cbind datasets
# dat <- bind_rows(dat_daily, dat_superg_nwis_7)



####--------------------------------------------###
#### Daymet air temperature and precipitation ####
####--------------------------------------------###

mycols <- brewer.pal(4, "Dark2")

siteinfo_big <- siteinfo %>% filter(designation == "big")

# download point location Daymet data
climlist <- vector("list", length = dim(siteinfo_big)[1])
for (i in 1:dim(siteinfo_big)[1]) {
  clim <- download_daymet(site = siteinfo_big$site_name[i], lat = siteinfo_big$lat[i], lon = siteinfo_big$long[i], start = 1980, end = 2023, internal = T)
  climlist[[i]] <- tibble(clim$data) %>% 
    mutate(air_temp_mean = (tmax..deg.c. + tmin..deg.c.)/2, 
           date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"),
           site_name = siteinfo_big$site_name[i]) %>%
    select(12,2,11,10,4,6) %>% rename(precip_mmday = 5, swe_kgm2 = 6)
  print(i)
}

# combine and calculate 7-day moving averages
climdf <- do.call(rbind, climlist) %>% left_join(siteinfo_big) %>% mutate(year = year(date)) %>% 
  group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation) %>%
  mutate(air_mean_7 = rollapply(air_temp_mean, FUN = mean, width = 7, align = "center", fill = NA),
         precip_mean_7 = rollapply(precip_mmday, FUN = mean, width = 7, align = "center", fill = NA),
         swe_mean_7 = rollapply(swe_kgm2, FUN = mean, width = 7, align = "center", fill = NA)) %>%
  ungroup() 

# combine and calculate monthly totals
climdf_monthly <- do.call(rbind, climlist) %>% left_join(siteinfo_big) %>% mutate(year = year(date), month = month(date)) %>% 
  group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation, year, month) %>%
  summarize(precip_mmmonth = sum(precip_mmday)) %>%
  ungroup() %>%
  mutate(date = date(paste(year, month, "01", sep = "-")))

# calculate SPI at various time scales
spi_list <- list()
for (i in 1:dim(siteinfo_big)[1]) {
  d <- climdf_monthly %>% filter(site_name == siteinfo_big$site_name[i])
  for (j in 1:24) {
    myspi <- spi(unlist(d %>% select(precip_mmmonth)), scale = j)
    myspi <- myspi$fitted
    myspi[is.infinite(myspi)] <- NA
    d[,paste("spi", j, sep = "_")] <- myspi
  }
  spi_list[[i]] <- d
  print(i)
}
spi_monthly <- do.call(rbind, spi_list)
view(spi_monthly)

spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_1))
spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_3))
spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_6))
spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_12))
spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_24))




# trim to Big G sites
climdf_big <- climdf %>% filter(designation == "big")

# long-term trends in mean annual air temperature
jpeg("./Explore Data/ClimaticContext_Daymet_AirTemp_AnnualTrend.jpg", height = 7, width = 7, units = "in", res = 500)
climdf_big_summ <- climdf_big %>% group_by(site_name, year) %>% summarize(anntemp = mean(air_temp_mean)) %>% ungroup()
climdf_big_summ %>% 
  ggplot(aes(x = year, y = anntemp)) + geom_point() + facet_wrap(~site_name) + 
  geom_smooth(method = "lm", se = TRUE) + xlab("Year") + ylab("Mean annual air temperature") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()


# plot annual time series - all years
jpeg("./Explore Data/ClimaticContext_Daymet_AirTemp_All.jpg", height = 7, width = 7, units = "in", res = 500)
ggplot() + 
  geom_line(data = climdf_big, aes(x = yday, y = air_mean_7, group = year, color = year), size = 0.2) +
  facet_wrap(~ site_name) + xlab("Day of year") + ylab("7-day mean air temperature") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()


# plot annual time series - highlight recent years
climdf_big_recent <- climdf_big %>% filter(year %in% c(2020:2024))
jpeg("./Explore Data/ClimaticContext_Daymet_AirTemp_Recent.jpg", height = 7, width = 7, units = "in", res = 500)
ggplot() + 
  geom_line(data = climdf_big, aes(x = yday, y = air_mean_7, group = year), color = "grey70", size = 0.25) +
  geom_line(data = climdf_big_recent, aes(x = yday, y = air_mean_7, group = as.factor(year), color = as.factor(year))) +
  scale_colour_manual(values = mycols) + facet_wrap(~ site_name) +
  xlab("Day of year") + ylab("7-day mean air temperature") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()



####--------------------------------------------###
#### Big G Flow and Temperature time series ####
####--------------------------------------------###


dat_daily_G <- dat %>% filter(designation == "big", site_name != "West Brook 0") %>% mutate(yday = yday(date), year = year(date))
dat_daily_G_recent <- dat_daily_G %>% filter(year %in% c(2020:2024))
year_range <- dat_daily_G %>% group_by(site_name) %>% summarize(minyear = min(year), maxyear = max(year)) %>% mutate(yearrange = paste(minyear, maxyear, sep = "-"))
dat_daily_G %>% group_by(site_name) %>% summarize(mindate = min(date), maxdate = max(date))

mycols <- brewer.pal(4, "Dark2")

# In-situ stream temperature
jpeg("./Explore Data/ClimaticContext_StreamTemp_Recent.jpg", height = 9, width = 11, units = "in", res = 500)
ggplot() + 
  geom_line(data = dat_daily_G, aes(x = yday, y = tempc_mean_7, group = year), color = "grey70", size = 0.25) +
  geom_line(data = dat_daily_G_recent, aes(x = yday, y = tempc_mean_7, group = as.factor(year), color = as.factor(year))) +
  scale_colour_manual(values = mycols) + facet_wrap(~ site_name, nrow = 3) + xlab("Day of year") + ylab("7-day mean stream temperature") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) + labs(color = "Year")
dev.off()


# In-situ streamflow
jpeg("./Explore Data/ClimaticContext_StreamFlow_Recent_Shen.jpg", height = 7, width = 5, units = "in", res = 500)
ggplot() + 
  geom_line(data = dat_daily_G %>% filter(region == "Shen"), aes(x = yday, y = log(flow_mean_7), group = year), color = "grey70", size = 0.25) +
  geom_line(data = dat_daily_G_recent %>% filter(region == "Shen"), aes(x = yday, y = log(flow_mean_7), group = as.factor(year), color = as.factor(year))) +
  scale_colour_manual(values = mycols) + facet_wrap(~ site_name, nrow = 3) + xlab("Day of year") + ylab("ln 7-day mean streamflow (cfs)") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(color = "Year")
dev.off()




# In-situ stream temperature (all sites)
jpeg("./Explore Data/ClimaticContext_StreamTemp_Recent.jpg", height = 9, width = 11, units = "in", res = 500)
ggplot() + 
  geom_line(data = dat_daily_G, aes(x = yday, y = tempc_mean_7, group = year), color = "grey70", size = 0.25) +
  geom_line(data = dat_daily_G_recent, aes(x = yday, y = tempc_mean_7, group = as.factor(year), color = as.factor(year))) +
  scale_colour_manual(values = mycols) + facet_wrap(~ site_name, nrow = 3) + xlab("Day of calendar year") + ylab("7-day mean temperature (deg C)") + ylim(0,22) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) + labs(color = "Year")
dev.off()

# In-situ streamflow (all sites)
jpeg("./Explore Data/ClimaticContext_StreamFlow_Recent.jpg", height = 9, width = 11, units = "in", res = 500)
ggplot() + 
  geom_line(data = dat_daily_G, aes(x = yday, y = log(flow_mean_7), group = year), color = "grey70", size = 0.25) +
  geom_line(data = dat_daily_G_recent, aes(x = yday, y = log(flow_mean_7), group = as.factor(year), color = as.factor(year))) +
  scale_colour_manual(values = mycols) + 
  geom_text(data = year_range, aes(x = -Inf, y = -Inf, label = yearrange), hjust = -0.1, vjust = -1) +
  facet_wrap(~ site_name, nrow = 3, scales = "free_y") + 
  xlab("Day of calendar year") + ylab("ln 7-day mean streamflow (cfs)") + labs(color = "Year") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0))
dev.off()



####--------------------------------------------###
#### Exceedance Probability ####
####--------------------------------------------###

# Exceedance probability
exceedance <- dat_daily_G %>% filter(!is.na(flow_mean)) %>% 
  mutate(flow_mean_log = log(flow_mean)) %>%
  group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation, year) %>% 
  arrange(desc(flow_mean_log), .by_group = TRUE) %>% 
  mutate(exceedance = 100/length(flow_mean_log)*1:length(flow_mean_log)) %>%
  ungroup()
exceedance_recent <- exceedance %>% filter(year %in% c(2020:2024))


jpeg("./Explore Data/ClimaticContext_StreamFlow_Recent_Exceedance.jpg", height = 9, width = 11, units = "in", res = 500)
ggplot() + 
  geom_line(data = exceedance, aes(x = exceedance, y = flow_mean_log, group = year), color = "grey70", size = 0.25) +
  geom_line(data = exceedance_recent, aes(x = exceedance, y = flow_mean_log, group = as.factor(year), color = as.factor(year))) +
  scale_colour_manual(values = mycols) + 
  geom_text(data = year_range, aes(x = Inf, y = Inf, label = yearrange), hjust = 1.2, vjust = 1.2) +
  facet_wrap(~ site_name, nrow = 3, scales = "free_y") + 
  xlab("Exceedance probability") + ylab("ln daily mean streamflow (cfs)") + labs(color = "Year") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0))
dev.off()



####--------------------------------------------###
#### Big G Yield ####
####--------------------------------------------###

# convert cfs and basin area to metric
dat_daily_G <- dat_daily_G %>% mutate(flow_mean_cms = flow_mean*0.02831683199881, area_sqkm = area_sqmi*2.58999)
# sites
sites <- unique(dat_daily_G$site_name)
# site-specific basin area in square km
basinarea <- dat_daily_G %>% group_by(site_name) %>% summarize(area_sqkm = unique(area_sqkm))

yield_list <- list()
for (i in 1:length(sites)) {
  d <- dat_daily_G %>% filter(site_name == sites[i])
  ba <- unlist(basinarea %>% filter(site_name == sites[i]) %>% select(area_sqkm))
  yield_list[[i]] <-  add_daily_yield(data = d, values = flow_mean_cms, basin_area = as.numeric(ba))
}
yield <- do.call(rbind, yield_list)
yield <- yield %>% mutate(Yield_mm_mean_7 = rollapply(Yield_mm, FUN = mean, width = 7, align = "center", fill = NA))
yield_recent <- yield %>% filter(year %in% c(2020:2024))

# log scale
jpeg("./Explore Data/ClimaticContext_StreamFlow_Recent_Yield.jpg", height = 9, width = 11, units = "in", res = 500)
ggplot() + 
  geom_line(data = yield, aes(x = yday, y = log(Yield_mm_mean_7), group = year), color = "grey70", size = 0.25) +
  geom_line(data = yield_recent, aes(x = yday, y = log(Yield_mm_mean_7), group = as.factor(year), color = as.factor(year))) +
  scale_colour_manual(values = mycols) + 
  geom_text(data = year_range, aes(x = -Inf, y = -Inf, label = yearrange), hjust = -0.1, vjust = -1) +
  facet_wrap(~ site_name, nrow = 3) + 
  xlab("Day of year") + ylab("ln 7-day mean yield") + labs(color = "Year") + theme_bw() + ylim(-5,3.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0))
dev.off()

# raw streamflow 
ggplot() + 
  geom_line(data = yield, aes(x = yday, y = (Yield_mm_mean_7), group = year), color = "grey70", size = 0.25) +
  geom_line(data = yield_recent, aes(x = yday, y = (Yield_mm_mean_7), group = as.factor(year), color = as.factor(year))) +
  scale_colour_manual(values = mycols) + 
  geom_text(data = year_range, aes(x = -Inf, y = -Inf, label = yearrange), hjust = -0.1, vjust = -1) +
  facet_wrap(~ site_name, nrow = 3) + 
  xlab("Day of year") + ylab("ln 7-day mean yield") + labs(color = "Year") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0))



####--------------------------------------------###
#### Drought Severity and Coverage Index ####
#### accessed: https://droughtmonitor.unl.edu/DmData/TimeSeries.aspx
#### HUC6 Basins
####--------------------------------------------###

# get huc 8 codes
huctib <- tibble(site_name = siteinfo$site_name, basin = siteinfo$basin, huc08 = NA)
for (i in 1:dim(siteinfo)[1]) {
  huctib$huc08[i] <- unlist(tibble(get_huc(siteinfo_sp[i,], type = "huc08"))[,11])
  print(i)
}
huctib <- huctib %>% group_by(basin) %>% summarize(huc08 = unique(huc08)) #%>% filter(basin != "Piney River")
unique(huctib$huc08)

# bring in drought indices
dsci <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/Raw data/dm_export_19800101_20241004.csv") %>%
 rename(huc08 = HUCID, name = Name, date = MapDate, dsci = DSCI) %>% mutate(date = ymd(date)) %>% left_join(huctib)
unique(dsci$basin)
unique(dsci$huc08)

# calculate monthly means
dsci_monthly <- dsci %>% 
  mutate(monthyear = as_date(paste(format_ISO8601(date, precision = "ym"), "-01", sep = ""))) %>% 
  mutate(year = year(monthyear), month = month(monthyear)) %>%
  group_by(huc08, basin, year, month, monthyear) %>% 
  summarize(dsci_monthly_1 = mean(dsci)) %>% 
  ungroup() %>%
  group_by(huc08, basin) %>%
  mutate(dsci_monthly_2 = rollapply(dsci_monthly_1, FUN = mean, width = 2, align = "right", fill = NA),
         dsci_monthly_3 = rollapply(dsci_monthly_1, FUN = mean, width = 3, align = "right", fill = NA),
         dsci_monthly_4 = rollapply(dsci_monthly_1, FUN = mean, width = 4, align = "right", fill = NA),
         dsci_monthly_5 = rollapply(dsci_monthly_1, FUN = mean, width = 5, align = "right", fill = NA),
         dsci_monthly_6 = rollapply(dsci_monthly_1, FUN = mean, width = 6, align = "right", fill = NA),
         dsci_monthly_7 = rollapply(dsci_monthly_1, FUN = mean, width = 7, align = "right", fill = NA),
         dsci_monthly_8 = rollapply(dsci_monthly_1, FUN = mean, width = 8, align = "right", fill = NA),
         dsci_monthly_9 = rollapply(dsci_monthly_1, FUN = mean, width = 9, align = "right", fill = NA),
         dsci_monthly_10 = rollapply(dsci_monthly_1, FUN = mean, width = 10, align = "right", fill = NA),
         dsci_monthly_11 = rollapply(dsci_monthly_1, FUN = mean, width = 11, align = "right", fill = NA),
         dsci_monthly_12 = rollapply(dsci_monthly_1, FUN = mean, width = 12, align = "right", fill = NA),
         dsci_monthly_13 = rollapply(dsci_monthly_1, FUN = mean, width = 13, align = "right", fill = NA),
         dsci_monthly_14 = rollapply(dsci_monthly_1, FUN = mean, width = 14, align = "right", fill = NA),
         dsci_monthly_15 = rollapply(dsci_monthly_1, FUN = mean, width = 15, align = "right", fill = NA),
         dsci_monthly_16 = rollapply(dsci_monthly_1, FUN = mean, width = 16, align = "right", fill = NA),
         dsci_monthly_17 = rollapply(dsci_monthly_1, FUN = mean, width = 17, align = "right", fill = NA),
         dsci_monthly_18 = rollapply(dsci_monthly_1, FUN = mean, width = 18, align = "right", fill = NA),
         dsci_monthly_19 = rollapply(dsci_monthly_1, FUN = mean, width = 19, align = "right", fill = NA),
         dsci_monthly_20 = rollapply(dsci_monthly_1, FUN = mean, width = 20, align = "right", fill = NA),
         dsci_monthly_21 = rollapply(dsci_monthly_1, FUN = mean, width = 21, align = "right", fill = NA),
         dsci_monthly_22 = rollapply(dsci_monthly_1, FUN = mean, width = 22, align = "right", fill = NA),
         dsci_monthly_23 = rollapply(dsci_monthly_1, FUN = mean, width = 23, align = "right", fill = NA),
         dsci_monthly_24 = rollapply(dsci_monthly_1, FUN = mean, width = 24, align = "right", fill = NA),) %>%
  ungroup()

# plot time series
dsci %>% ggplot(aes(x = date, y = dsci)) + geom_line() + facet_wrap(~ basin)

jpeg("./Explore Data/ClimaticContext_DSCI.jpg", height = 9, width = 11, units = "in", res = 500)
dsci_monthly %>% ggplot(aes(x = monthyear, y = dsci_monthly_1)) + geom_line() + facet_wrap(~ basin) + 
  xlab("") + ylab("Drought severity and coverage index (DSCI)") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()




####--------------------------------------------###
#### Low flow metrics by DSCI ####
####--------------------------------------------###

# check date range for each site
yield %>% group_by(site_name) %>% summarize(mindate = min(date), maxdate = max(date))

# convert to percentiles and plot
yield2 <- yield %>% filter(year(date) >= 1993) %>% 
  arrange(desc(Yield_mm_mean_7), .by_group = TRUE) %>% 
  mutate(yield_percentile = 100-(100/length(Yield_mm_mean_7)*1:length(Yield_mm_mean_7))) %>%
  ungroup()
ggplot() + 
  geom_line(data = yield2, aes(x = yday, y = yield_percentile, group = year), color = "grey70", size = 0.25) +
  facet_wrap(~ site_name, nrow = 3) + 
  xlab("Day of year") + ylab("7-day mean yield percentile") + labs(color = "Year") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0))

# set threshold percentile Yield for drought identification
thresh <- quantile(yield2$Yield_mm_mean_7, probs = 0.2, na.rm = TRUE) 

# visualize threshold on plot
jpeg("./Explore Data/ClimaticContext_StreamFlow_Recent_Yield_wThreshold.jpg", height = 9, width = 11, units = "in", res = 500)
ggplot() + 
  geom_line(data = yield, aes(x = yday, y = log(Yield_mm_mean_7), group = year), color = "grey70", size = 0.25) +
  geom_line(data = yield_recent, aes(x = yday, y = log(Yield_mm_mean_7), group = as.factor(year), color = as.factor(year))) +
  geom_hline(yintercept = log(thresh), linetype = "dashed") +
  scale_colour_manual(values = mycols) + 
  geom_text(data = year_range, aes(x = -Inf, y = -Inf, label = yearrange), hjust = -0.1, vjust = -1) +
  facet_wrap(~ site_name, nrow = 3) + 
  xlab("Day of year") + ylab("ln 7-day mean yield") + labs(color = "Year") + theme_bw() + ylim(-5,3.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0))
dev.off()

# calculate low flow metrics from yield percentiles
# See Hammond et al 2022 for use of normalized drought deficit and drought deficit fraction
yield3 <- yield2 %>% 
  mutate(isdrought = ifelse(Yield_mm_mean_7 <= thresh, 1, 0), 
         deficit = ifelse(isdrought == 1, thresh - Yield_mm_mean_7, 0),
         year = year(date),
         doy = yday(date)) %>%
  group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation, year) %>%
  summarize(duration = sum(isdrought), 
            deficit = sum(deficit),
            q7min = min(Yield_mm_mean_7, na.rm = TRUE),
            q7mindate = mean(yday[Yield_mm_mean_7 == q7min]),
            deficit_norm = sum(deficit) / sum(Yield_mm_mean_7)) 

# time series of low flow metrics by site
ggplot() + geom_line(data = yield3, aes(x = year, duration)) + facet_wrap(~ site_name, nrow = 3) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot() + geom_line(data = yield3, aes(x = year, deficit)) + facet_wrap(~ site_name, nrow = 3) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot() + geom_line(data = yield3, aes(x = year, q7min)) + facet_wrap(~ site_name, nrow = 3) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot() + geom_line(data = yield3, aes(x = year, q7mindate)) + facet_wrap(~ site_name, nrow = 3) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# get yearly mean DSCI and join to yield percentile dataset
dsci_yearly <- dsci %>% mutate(year = year(date)) %>% group_by(basin, year) %>% summarize(dsci_yr = mean(dsci)) %>% ungroup()
yield3 <- yield3 %>% left_join(dsci_yearly)

# PLOTS
# duration by dsci
jpeg("./Explore Data/ClimaticContext_BigG_DurationByDSCI.jpg", height = 9, width = 11, units = "in", res = 500)
yield3 %>% 
  ggplot(aes(x = dsci_yr, y = duration, color = year)) +
  geom_smooth(color = "black") + geom_point() + facet_wrap(~ site_name) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) +
  xlab("Drought severity and coverage index (DSCI)") + ylab("Drought duration (days)")
dev.off()
# deficit by dsci
jpeg("./Explore Data/ClimaticContext_BigG_DeficitnByDSCI.jpg", height = 9, width = 11, units = "in", res = 500)
yield3 %>%
  ggplot(aes(x = dsci_yr, y = deficit, color = year)) +
  geom_smooth(color = "black") + geom_point() + facet_wrap(~ site_name) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) + 
  xlab("Drought severity and coverage index (DSCI)") + ylab("Drought deficit (mm)")
dev.off()
# q7min by dsci
jpeg("./Explore Data/ClimaticContext_BigG_Q7MinByDSCI.jpg", height = 9, width = 11, units = "in", res = 500)
yield3 %>% 
  ggplot(aes(x = dsci_yr, y = log(q7min), color = year)) +
  geom_smooth(color = "black") + geom_point() + facet_wrap(~ site_name) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) +
  xlab("Drought severity and coverage index (DSCI)") + ylab("Minimum 7-day mean yield (mm)")
dev.off()


# unique scales
yield3 %>% 
  ggplot(aes(x = dsci_yr, y = duration, color = year)) +
  geom_smooth(color = "black") + geom_point() + facet_wrap(~ site_name, scales = "free_y") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) +
  xlab("Drought severity and coverage index (DSCI)") + ylab("Drought duration (days)")
# deficit by dsci
yield3 %>%
  ggplot(aes(x = dsci_yr, y = deficit, color = year)) +
  geom_smooth(color = "black") + geom_point() + facet_wrap(~ site_name, scales = "free_y") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) + 
  xlab("Drought severity and coverage index (DSCI)") + ylab("Drought deficit (mm)")
# q7min by dsci
yield3 %>% 
  ggplot(aes(x = dsci_yr, y = log(q7min), color = year)) +
  geom_smooth(color = "black") + geom_point() + facet_wrap(~ site_name, scales = "free_y") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) +
  xlab("Drought severity and coverage index (DSCI)") + ylab("Minimum 7-day mean yield (mm)")

#---------------------------------------------------------------------------------#

# alternatively, use site level thresholds 

# convert to percentiles and plot
yield2a <- yield %>% filter(year(date) >= 1993) %>% 
  mutate(year = year(date)) %>%
  group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation) %>%
  mutate(thresh = quantile(Yield_mm_mean_7, probs = 0.1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(isdrought = ifelse(Yield_mm_mean_7 <= thresh, 1, 0),
         deficit = ifelse(isdrought == 1, thresh - Yield_mm_mean_7, 0)) %>%
  group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation, year) %>%
  summarize(duration = sum(isdrought), 
            deficit = sum(deficit),
            q7min = min(Yield_mm_mean_7, na.rm = TRUE),
            thresh = unique(thresh)) %>%
  ungroup() %>%
  left_join(dsci_yearly)

# calculate (long-term) low flow thresholds from 7-day mean yield, by basin (Big G)
thresholds <- yield2 %>% filter(site_name != "West Brook 0", designation == "big") %>% 
  group_by(site_name) %>% summarise(thresh20 = quantile(Yield_mm_mean_7, probs = 0.20, na.rm = TRUE),
                                thresh10 = quantile(Yield_mm_mean_7, probs = 0.10, na.rm = TRUE),
                                thresh05 = quantile(Yield_mm_mean_7, probs = 0.05, na.rm = TRUE),
                                thresh02 = quantile(Yield_mm_mean_7, probs = 0.02, na.rm = TRUE)) 

# visualize threshold on plot
jpeg("./Explore Data/ClimaticContext_StreamFlow_Recent_Yield_wThreshold_sitesp.jpg", height = 9, width = 11, units = "in", res = 500)
ggplot() + 
  geom_line(data = yield2, aes(x = yday, y = log(Yield_mm_mean_7), group = year), color = "grey70", size = 0.25) +
  geom_line(data = yield_recent, aes(x = yday, y = log(Yield_mm_mean_7), group = as.factor(year), color = as.factor(year))) +
  geom_hline(data = thresholds, aes(yintercept = log(thresh10)), linetype = "dashed") +
  scale_colour_manual(values = mycols) + 
  geom_text(data = year_range, aes(x = -Inf, y = -Inf, label = yearrange), hjust = -0.1, vjust = -1) +
  facet_wrap(~ site_name, nrow = 3) + 
  xlab("Day of year") + ylab("ln 7-day mean yield") + labs(color = "Year") + theme_bw() + ylim(-5,3.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0))
dev.off()


# duration by dsci
jpeg("./Explore Data/ClimaticContext_BigG_DurationByDSCI_sitesp.jpg", height = 9, width = 11, units = "in", res = 500)
yield2a %>% 
  ggplot(aes(x = dsci_yr, y = duration, color = year)) +
  geom_smooth(color = "black") + geom_point() + facet_wrap(~ site_name, scales = "free_y") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) +
  xlab("Drought severity and coverage index (DSCI)") + ylab("Drought duration (days)")
dev.off()
# deficit by dsci
jpeg("./Explore Data/ClimaticContext_BigG_DeficitByDSCI_sitesp.jpg", height = 9, width = 11, units = "in", res = 500)
yield2a %>%
  ggplot(aes(x = dsci_yr, y = deficit, color = year)) +
  geom_smooth(color = "black") + geom_point() + facet_wrap(~ site_name, scales = "free_y") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) + 
  xlab("Drought severity and coverage index (DSCI)") + ylab("Drought deficit (mm)")
dev.off()
# q7min by dsci
jpeg("./Explore Data/ClimaticContext_BigG_Q7minByDSCI_sitesp.jpg", height = 9, width = 11, units = "in", res = 500)
yield2a %>%
  ggplot(aes(x = dsci_yr, y = log(q7min), color = year)) +
  geom_smooth(color = "black") + geom_point() + facet_wrap(~ site_name, scales = "free_y") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.9,0.1), legend.justification = c(1,0)) + 
  xlab("Drought severity and coverage index (DSCI)") + ylab("ln Minimum 7-day mean yield (mm)")
dev.off()




####--------------------------------------------###
#### Time scale of streamflow-DSCI Correlation ####
####--------------------------------------------###

dat2
dsci_monthly

# bind flow and dsci data
dat3 <- dat2 %>% 
  left_join(dsci_monthly %>% select(basin, year, month, dsci_monthly_1:dsci_monthly_24)) %>%
  left_join(spi_monthly %>% select(basin, year, month, spi_1:spi_24))

# calculate correlations
corlist_dsci_mean <- list()
corlist_dsci_min <- list()
corlist_spi_mean <- list()
corlist_spi_min <- list()
sites <- unique(dat3$site_name)
for(i in 1:length(sites)) {
  d <- dat3 %>% filter(site_name == sites[i])
  mymat_dsci_mean <- matrix(data = NA, nrow = 12, ncol = 24)
  mymat_dsci_min <- matrix(data = NA, nrow = 12, ncol = 24)
  mymat_spi_mean <- matrix(data = NA, nrow = 12, ncol = 24)
  mymat_spi_min <- matrix(data = NA, nrow = 12, ncol = 24)
  for(j in 1:12) {
    for(k in 1:24) {
      d2 <- d %>% filter(month == j) %>% select(z_flow_mean_monthly, paste("dsci_monthly_", k, sep = ""))
      mymat_dsci_mean[j,k] <- cor(d2[,1], d2[,2], method = "pearson", use = "complete.obs")
      d2 <- d %>% filter(month == j) %>% select(z_flow_min_monthly, paste("dsci_monthly_", k, sep = ""))
      mymat_dsci_min[j,k] <- cor(d2[,1], d2[,2], method = "pearson", use = "complete.obs")
      
      d2 <- d %>% filter(month == j) %>% select(z_flow_mean_monthly, paste("spi_", k, sep = ""))
      mymat_spi_mean[j,k] <- cor(d2[,1], d2[,2], method = "pearson", use = "complete.obs")
      d2 <- d %>% filter(month == j) %>% select(z_flow_min_monthly, paste("spi_", k, sep = ""))
      mymat_spi_min[j,k] <- cor(d2[,1], d2[,2], method = "pearson", use = "complete.obs")
    }
  }
  corlist_dsci_mean[[i]] <- mymat_dsci_mean
  corlist_dsci_min[[i]] <- mymat_dsci_min
  corlist_spi_mean[[i]] <- mymat_spi_mean
  corlist_spi_min[[i]] <- mymat_spi_min
  print(i)
}

# generate plots
n <- 100
for(i in 1:length(sites)) {
  # streamflow mean ~ DSCI
  jpeg(paste("./Explore Data/Dynamic drought sensitivity/Streamflow_DSCI_correlation_TimeVarying_", "site", i, "_mean.jpg", sep = ""), height = 6, width = 7, units = "in", res = 500)
  filled.contour(x = 1:24, y = 1:12, z = t(corlist_dsci_mean[[i]]), 
                 levels = seq(from = -1, to = 1, length = n+1), col = hcl.colors(n, "PRGn"),
                 plot.title = title(main = sites[i], 
                                    xlab = "DSCI - accumulation time scale (months)", 
                                    ylab = "Month"),
                 plot.axes = { 
                   axis(1, at = seq(from = 1, to = 23, by = 2))
                   axis(2, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 
                   contour(x = 1:24, y = 1:12, z = t(corlist_dsci_mean[[i]]), add = TRUE, labcex = 1, levels = seq(from = -1, to = 1, length = 11))
                 })
  dev.off()
  
  # streamflow mean ~ SPI
  jpeg(paste("./Explore Data/Dynamic drought sensitivity/Streamflow_SPI_correlation_TimeVarying_", "site", i, "_mean.jpg", sep = ""), height = 6, width = 7, units = "in", res = 500)
  filled.contour(x = 1:24, y = 1:12, z = t(corlist_spi_mean[[i]]), 
                 levels = seq(from = -1, to = 1, length = n+1), col = hcl.colors(n, "PRGn"),
                 plot.title = title(main = sites[i], 
                                    xlab = "SPI - accumulation time scale (months)", 
                                    ylab = "Month"),
                 plot.axes = { 
                   axis(1, at = seq(from = 1, to = 23, by = 2))
                   axis(2, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 
                   contour(x = 1:24, y = 1:12, z = t(corlist_spi_mean[[i]]), add = TRUE, labcex = 1, levels = seq(from = -1, to = 1, length = 11))
                 })
  dev.off()
  
  # streamflow minimum ~ DSCI
  jpeg(paste("./Explore Data/Dynamic drought sensitivity/Streamflow_DSCI_correlation_TimeVarying_", "site", i, "_min.jpg", sep = ""), height = 6, width = 7, units = "in", res = 500)
  filled.contour(x = 1:24, y = 1:12, z = t(corlist_dsci_min[[i]]), 
                 levels = seq(from = -1, to = 1, length = n+1), col = hcl.colors(n, "PRGn"),
                 plot.title = title(main = sites[i], 
                                    xlab = "DSCI - accumulation time scale (months)", 
                                    ylab = "Month"),
                 plot.axes = { 
                   axis(1, at = seq(from = 1, to = 23, by = 2))
                   axis(2, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 
                   contour(x = 1:24, y = 1:12, z = t(corlist_dsci_min[[i]]), add = TRUE, labcex = 1, levels = seq(from = -1, to = 1, length = 11))
                 })
  dev.off()
  
  # streamflow minimum ~ SPI
  jpeg(paste("./Explore Data/Dynamic drought sensitivity/Streamflow_SPI_correlation_TimeVarying_", "site", i, "_min.jpg", sep = ""), height = 6, width = 7, units = "in", res = 500)
  filled.contour(x = 1:24, y = 1:12, z = t(corlist_spi_min[[i]]), 
                 levels = seq(from = -1, to = 1, length = n+1), col = hcl.colors(n, "PRGn"),
                 plot.title = title(main = sites[i], 
                                    xlab = "SPI - accumulation time scale (months)", 
                                    ylab = "Month"),
                 plot.axes = { 
                   axis(1, at = seq(from = 1, to = 23, by = 2))
                   axis(2, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 
                   contour(x = 1:24, y = 1:12, z = t(corlist_spi_min[[i]]), add = TRUE, labcex = 1, levels = seq(from = -1, to = 1, length = 11))
                 })
  dev.off()
}


plot(z_flow_mean_monthly ~ dsci_monthly_24, dat3 %>% filter(site_name == "Donner Blitzen River nr Frenchglen NWIS", month == 10))
plot(z_flow_mean_monthly ~ spi_7, dat3 %>% filter(site_name == "Donner Blitzen River nr Frenchglen NWIS", month == 6))
