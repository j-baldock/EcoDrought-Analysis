############################################################################
#
# Project: EcoDrought
# Purpose: Quantify the senstivity of streamflow to climate at different timescales
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
siteinfo <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_SiteInformation.csv")
siteinfo_sp <- st_as_sf(siteinfo, coords = c("long", "lat"), crs = 4326)
mapview(siteinfo_sp, zcol = "designation")

# flow (and temp) data
dat <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_DailyWeekly.csv")

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



####--------------------------------------------###
#### Calculate SPI from modeled Daymet precipitation ####
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
# view(spi_monthly)

# view some time series of SPI
spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_1))
spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_3))
spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_6))
spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_12))
spi_monthly %>% filter(site_id == "SRL") %>% ggplot() + geom_line(aes(x = date, y = spi_24))



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
dsci <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Raw data/dm_export_19800101_20241004.csv") %>%
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

jpeg("./Explore Data/Long Term Plots/ClimaticContext_DSCI_1-6-12.jpg", height = 8, width = 9, units = "in", res = 500)
dsci_monthly %>% ggplot() + 
  geom_line(aes(x = monthyear, y = dsci_monthly_1)) + 
  geom_line(aes(x = monthyear, y = dsci_monthly_6), color = mycols[1]) + 
  geom_line(aes(x = monthyear, y = dsci_monthly_12), color = mycols[2]) + 
  facet_wrap(~ basin) + 
  xlab("") + ylab("Drought severity and coverage index (DSCI): 1-, 6-, and 12-month") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()



####--------------------------------------------###
#### Big G: Time scale of streamflow-DSCI/SPI Correlation ####
####--------------------------------------------###

# bind flow, dsci, and spi data
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

# generate plots/heatmaps of correlation
n <- 100
for(i in 1:length(sites)) {
  # streamflow mean ~ DSCI
  jpeg(paste("./Climate Sensitivity/Climate sensitivity plots/Streamflow_DSCI_correlation_TimeVarying_", "site", i, "_mean.jpg", sep = ""), height = 6, width = 7, units = "in", res = 500)
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
  jpeg(paste("./Climate Sensitivity/Climate sensitivity plots/Streamflow_SPI_correlation_TimeVarying_", "site", i, "_mean.jpg", sep = ""), height = 6, width = 7, units = "in", res = 500)
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
  jpeg(paste("./Climate Sensitivity/Climate sensitivity plots/Streamflow_DSCI_correlation_TimeVarying_", "site", i, "_min.jpg", sep = ""), height = 6, width = 7, units = "in", res = 500)
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
  jpeg(paste("./Climate Sensitivity/Climate sensitivity plots/Streamflow_SPI_correlation_TimeVarying_", "site", i, "_min.jpg", sep = ""), height = 6, width = 7, units = "in", res = 500)
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

# plot raw data
plot(z_flow_mean_monthly ~ dsci_monthly_24, dat3 %>% filter(site_name == "Donner Blitzen River nr Frenchglen NWIS", month == 10))
plot(z_flow_mean_monthly ~ spi_7, dat3 %>% filter(site_name == "Donner Blitzen River nr Frenchglen NWIS", month == 6))


