############################################################################
#
# Project: EcoDrought
# Purpose: Pilot Big G - Little g analysis
# Author: Jeff Baldock, jbaldock@usgs.gov
#
###########################################################################

# To Do
# - organize by either climate or water year
# - filter by data availability, especially when calculating low flow thresholds from Big G gages


library(tidyverse)
library(zoo)
library(RColorBrewer)
library(sf)
library(mapview)
library(fasstr)
library(ggpubr)
library(gganimate)
library(fmsb)
library(nhdplusTools)
library(terra)

####--------------------------------------------###
#### Site information and daily data ####
####--------------------------------------------###

# site information and locations
siteinfo <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_SiteInformation.csv")
siteinfo_sp <- st_as_sf(siteinfo, coords = c("long", "lat"), crs = 4326)
mapview(siteinfo_sp, zcol = "designation")

# flow (and temp) data 
dat <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_DailyWeekly.csv")

# add water/climate year variables
dat <- add_date_variables(dat, dates = date, water_year_start = 4)

# calculate completeness by site and water year
complete <- dat %>% group_by(site_name, designation, WaterYear) %>% summarize(completeness = sum(!is.na(flow_mean))/365)
dat <- dat %>% left_join(complete)

# grab long-term Big G data to calculate low flow thresholds
# dat_big <- read_csv("C:/Users/jbaldock/Desktop/Baldock-Temp/USGS ORISE/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_DailyWeekly.csv") %>%
#   filter(site_name %in% c("Mill River Northampton", "Staunton River 10"), designation == "big", year(date) >= 1993)

# view sites and data
unique(dat$site_name)
dat %>% filter(CalendarYear >= 2018) %>% ggplot() + geom_line(aes(x = date, y = log(flow_mean_7))) + facet_wrap(~site_name, scales = "free_y")


####--------------------------------------------###
#### Add yield and low flow thresholds ####
####--------------------------------------------###

# NOTE: yield is calculated on cubic meters per second rather than cfs

# convert cfs and basin area to metric
dat <- dat %>% mutate(flow_mean_cms = flow_mean*0.02831683199881, area_sqkm = area_sqmi*2.58999)

# sites
sites <- unique(dat$site_name)

# site-specific basin area in square km
basinarea <- dat %>% group_by(site_name) %>% summarize(area_sqkm = unique(area_sqkm))

# calculate yield
yield_list <- list()
for (i in 1:length(sites)) {
  d <- dat %>% filter(site_name == sites[i])
  ba <- unlist(basinarea %>% filter(site_name == sites[i]) %>% select(area_sqkm))
  yield_list[[i]] <-  add_daily_yield(data = d, values = flow_mean_cms, basin_area = as.numeric(ba))
}
dat <- do.call(rbind, yield_list)

# add missing dates
dat <- fill_missing_dates(dat, dates = date, groups = site_name)

# calculate 7-day rolling mean yield
dat <- dat %>%
  group_by(site_name) %>%
  mutate(Yield_mm_mean_7 = rollapply(Yield_mm, FUN = mean, width = 7, align = "center", fill = NA)) %>%
  ungroup() %>% filter(!is.na(site_id))

unique(dat$basin)
unique(dat$subbasin)

# write out data with yield
write_csv(dat, "C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_DailyWeekly_withYield.csv")
dat <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_DailyWeekly_withYield.csv") %>%
  filter(!site_name %in% c("Wounded Buck Creek"))

# # calculate exceedance proababilities and flow percentiles (1-exceedance, but calculated sensu Hammond et al 2022 WRR)
# dat <- dat %>% filter(!is.na(flow_mean_7)) %>% 
#   group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation) %>% 
#   arrange(desc(flow_mean_7), .by_group = TRUE) %>% 
#   mutate(exceedance = 100/length(flow_mean_7)*1:length(flow_mean_7),
#          weibull_perc = 100 * min_rank(flow_mean_7)/(length(flow_mean_7) + 1),
#          # site-level values needed for calulating the deficit volume of drought
#          thresh_30_site = stats::quantile(flow_mean_7, probs = 0.3, na.rm = TRUE),
#          thresh_20_site = stats::quantile(flow_mean_7, probs = 0.2, na.rm = TRUE),
#          thresh_10_site = stats::quantile(flow_mean_7, probs = 0.1, na.rm = TRUE),
#          thresh_5_site = stats::quantile(flow_mean_7, probs = 0.05, na.rm = TRUE),
#          thresh_2_site = stats::quantile(flow_mean_7, probs = 0.02, na.rm = TRUE)) %>%
#   ungroup()


####--------------------------------------------###
#### Filter by basin and/or completeness ####
####--------------------------------------------###

# # filter data by completeness and/or basin
# dat <- dat %>% filter(basin %in% c("West Brook"))
# # dat <- dat %>% filter(completeness >= 0.9) %>% filter(basin %in% c("West Brook", "Staunton River", "Paine Run", "Flathead"))
# 
# # set order of little g site_name factor...order by ~distance upstream
# unique(dat$site_name)
# mylevels <- c("West Brook Lower", "Mitchell Brook", "Jimmy Brook", 
#               "Obear Brook Lower", "West Brook Upper", "West Brook Reservoir", 
#               "Sanderson Brook", "Avery Brook", "West Whately Brook")


####--------------------------------------------###
#### basin plots of flow and yield by site/basin ####
####--------------------------------------------###

dat %>% filter(basin == "Flathead", designation == "little") %>%
  ggplot() + 
  geom_line(aes(x = date, y = log(flow_mean_7))) +
  facet_wrap(~site_name) + theme(legend.position = "none")

jpeg("Explore Data/Flathead_BigCreek_LittleG_lnYield.jpg", height = 3, width = 13, units = "in", res = 500)
dat %>% filter(subbasin == "Big Creek", designation == "little", !site_name %in% c("LangfordCreekLower", "SkookoleelCreek")) %>%
  ggplot() + geom_line(aes(x = date, y = log(Yield_mm_mean_7), group = site_name, color = site_name))
dev.off()

jpeg("Explore Data/Flathead_CoalCreek_LittleG_lnYield.jpg", height = 3, width = 13, units = "in", res = 500)
dat %>% filter(subbasin == "Coal Creek", designation == "little") %>%
  ggplot() + geom_line(aes(x = date, y = log(Yield_mm_mean_7), group = site_name, color = site_name))
dev.off()

jpeg("Explore Data/Flathead_McGeeCreek_LittleG_lnYield.jpg", height = 3, width = 13, units = "in", res = 500)
dat %>% filter(subbasin == "McGee Creek", designation == "little") %>%
  ggplot() + geom_line(aes(x = date, y = log(Yield_mm_mean_7), group = site_name, color = site_name))
dev.off()



# log(streamflow)
ggplot() + 
  geom_line(data = dat %>% filter(designation == "big", WaterYear %in% c(2019:2022)), aes(x = date, y = log(flow_mean_7)), color = "black", size = 2) +
  geom_line(data = dat %>% filter(designation == "little", WaterYear %in% c(2019:2022)), aes(x = date, y = log(flow_mean_7), group = site_name, color = site_name)) + 
  facet_wrap(~basin, nrow = 2, scales = "free_y") + theme(legend.position = "none")

# log(yield)
jpeg("Big G Little g/BigGLittleG_YieldByBasin2.jpg", height = 8, width = 8, units = "in", res = 500)
ggplot(data = dat %>% filter(designation == "little", site_name != "West Brook 0", WaterYear == 2021)) + 
  geom_line(aes(x = date, y = log(Yield_mm_mean_7), group = site_name, color = site_name)) + 
  # geom_hline(aes(yintercept = log(thresh20)), linetype = "dashed") +
  # geom_hline(aes(yintercept = log(thresh10)), linetype = "dashed") +
  # geom_hline(aes(yintercept = log(thresh05)), linetype = "dashed") +
  # geom_hline(aes(yintercept = log(thresh02)), linetype = "dashed") +
  geom_line(data = dat %>% filter(site_name == "West Brook NWIS", WaterYear == 2021), aes(x = date, y = log(Yield_mm_mean_7)), color = "black", size = 1.25) +
  # facet_wrap(~basin, nrow = 2) + 
  xlab("") + ylab("ln yield") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()

ggplot(data = dat %>% filter(designation == "little", WaterYear >= 2018)) + 
  geom_line(aes(x = date, y = log(Yield_mm_mean_7), group = site_name, color = site_name)) + 
  geom_line(data = dat %>% filter(designation == "big", WaterYear >= 2018), aes(x = date, y = log(Yield_mm_mean_7)), color = "black", size = 1.25) +
  facet_wrap(~WaterYear, scales = "free_x") + xlab("") + ylab("ln yield") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")

# percentiles
# ggplot() + 
#   geom_line(data = dat %>% filter(designation == "big"), aes(x = date, y = weibull_perc), color = "black", size = 2) +
#   geom_line(data = dat %>% filter(designation == "little"), aes(x = date, y = weibull_perc, group = site_name, color = site_name)) + 
#   facet_wrap(~basin, nrow = 2, scales = "free_y") + geom_hline(yintercept = 20)



####--------------------------------------------###
#### Create plotting functions ####
####--------------------------------------------###

# residual scatter plot function
residualplots <- function(mybasin, CY, little, big) {
  # essential elements
  dat_basin <- dat %>% filter(basin == mybasin)
  
  # create residual data frame
  delta_dat <- dat_basin %>% select(site_name, site_id, basin, region, designation, date, WaterYear, Yield_mm_mean_7) %>% filter(site_name %in% little, WaterYear == CY) %>% 
    left_join(dat_basin %>% select(basin, site_name, date, WaterYear, Yield_mm_mean_7) %>% filter(site_name == big, WaterYear == CY) %>% rename(bigyield = Yield_mm_mean_7) %>% select(-site_name)) %>% 
    mutate(delta_yield = log(Yield_mm_mean_7) - log(bigyield), site_name = factor(site_name, levels = little)) %>% 
    group_by(site_name) %>% mutate(cum_resid = cumsum(coalesce(delta_yield, 0)) + delta_yield*0) 
  
  # base plot
  p1 <- delta_dat %>% 
    filter(WaterYear == CY) %>%
    group_by(site_name) %>%
    mutate(month = month(date), doy = 1:n()) %>% 
    ungroup() %>%
    ggplot(aes(x = log(bigyield), y = log(Yield_mm_mean_7), color = doy)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_point(aes(group = doy)) +
    geom_path() +
    scale_color_gradient2(midpoint = 182, low = "orange", mid = "purple3", high = "orange") +
    facet_wrap(~site_name) +
    xlab("Big G ln(Yield)") + ylab("Little G ln(Yield)") + labs(color = "Days from April 1") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  
  # animated 
  # p1_anim <- p1 + transition_reveal(along = doy)
  
  # write out - static
  jpeg(paste("Big G Little g/Compare Time Series/BigGLittleG_ScatterByTime", mybasin, "_", CY, ".jpg", sep = ""), height = 10, width = 12, units = "in", res = 500)
  print(p1)
  dev.off()
  
  # write out - animated
  # animate(p1_anim, renderer = gifski_renderer(), height = 10, width = 12, units = "in", res = 500)
  # anim_save(paste("Big G Little g/Compare Time Series/BigGLittleG_ScatterByTime_Animated_", mybasin, "_", CY, ".gif", sep = ""))
  
  # residual time series
  jpeg(paste("Big G Little g/Compare Time Series/BigGLittleG_YieldResiduals_TimeSeries_", mybasin, "_", CY, ".jpg", sep = ""), height = 4, width = 6, units = "in", res = 500)
  print(ggplot(data = delta_dat) + 
    geom_line(aes(x = date, y = delta_yield, group = site_name, color = site_name)) + 
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    xlab("") + ylab("ln(Yield) residuals (difference from Big G)") + labs(color = "Little g") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
  dev.off()
  
  
  # plot little g cumulative residuals - difference from Big G by site
  jpeg(paste("Big G Little g/Compare Time Series/BigGLittleG_YieldResiduals_Cumulative_TimeSeries_", mybasin, "_", CY, ".jpg", sep = ""), height = 4, width = 6, units = "in", res = 500)
  print(delta_dat %>% 
    ggplot() + 
    geom_line(aes(x = date, y = cum_resid, group = site_name, color = site_name)) + 
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    xlab("") + ylab("ln(Yield) cumulative residuals ") + labs(color = "Little g") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
  dev.off()
}

# big plot function
bigplotfun <- function(mybasin, CY, little, big, super, supergyears, mymap) {
  #### essential elements
  dat_basin <- dat %>% filter(basin == mybasin)
  months <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
  
  #### MAP
  p1 <- mymap
  print("p1")
  
  #### HYDROGRAPHS IN YIELD
  p2 <- ggplot() + 
    geom_line(data = dat_basin %>% filter(WaterYear == CY, site_name %in% little) %>% mutate(site_name = factor(site_name, levels = little)), aes(x = date, y = log(Yield_mm_mean_7), group = site_name, color = site_name)) +
    geom_line(data = dat_basin %>% filter(WaterYear == CY, site_name == big), aes(x = date, y = log(Yield_mm_mean_7)), color = "black", size = 1.25) +
    xlab("Date") + ylab("ln(Yield, mm)") + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
  print("p2")
  
  
  #### TOTAL ANNUAL YIELD
  # get total yield per year and convert to percentiles
  yeartotals <- dat_basin %>% 
    filter(site_name == super, WaterYear %in% supergyears) %>% 
    group_by(WaterYear) %>% 
    summarize(totalyield = sum(Yield_mm, na.rm = TRUE)) %>%
    filter(!is.na(totalyield)) %>%
    mutate(percentile = percent_rank(totalyield))
  p3 <- ggplot() + 
    geom_line(data = yeartotals, aes(x = WaterYear, y = totalyield), color = "grey40") + 
    geom_point(data = yeartotals %>% filter(WaterYear == CY), aes(x = WaterYear, y = totalyield)) +
    xlab("Climate year") + ylab("Total annual yield (mm)") + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  print("p3")
  
  
  #### EXCEEDANCE PROBABILITY - SUPER G PERIOD OF RECORD
  exceedance <- dat_basin %>% 
    filter(site_name %in% c(little, big, super)) %>%
    filter(!is.na(Yield_mm_mean_7)) %>% 
    mutate(Yield_mm_mean_7_log = log(Yield_mm_mean_7)) %>%
    group_by(site_name, WaterYear) %>% 
    arrange(desc(Yield_mm_mean_7_log), .by_group = TRUE) %>% 
    mutate(exceedance = 100/length(Yield_mm_mean_7_log)*1:length(Yield_mm_mean_7_log)) %>%
    ungroup()
  p4 <- ggplot() + 
    geom_line(data = exceedance %>% filter(site_name == super, WaterYear %in% supergyears), aes(x = exceedance, y = Yield_mm_mean_7_log, group = WaterYear, color = WaterYear), size = 0.25) +
    geom_line(data = exceedance %>% filter(site_name == super, WaterYear == CY), aes(x = exceedance, y = Yield_mm_mean_7_log), color = "black", size = 1.25) +
    geom_text(aes(x = 50, y = Inf, label = paste("Super G: ", super, " (", min(supergyears), "-", max(supergyears), ")", "\nCurrent year: ", CY, " (", round(yeartotals$percentile[yeartotals$WaterYear == CY]*100), "th perc.)", sep = "")), vjust = 1.2) +
    scale_color_continuous(trans = "reverse") +
    xlab("Exceedance probability") + ylab("ln(Yield, mm)") + labs(color = "Climate year") + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.15,0.23), 
          legend.text = element_text(size = 9), legend.title = element_text(size = 9), legend.key.height = unit(0.4, "cm"))
  print("p4")
  
  
  #### CUMULATIVE YIELD RESIDUALS
  p5 <- dat_basin %>% select(site_name, site_id, basin, region, designation, date, WaterYear, Yield_mm_mean_7) %>% filter(site_name %in% little, WaterYear == CY) %>% 
    left_join(dat_basin %>% select(basin, site_name, date, WaterYear, Yield_mm_mean_7) %>% filter(site_name == big, WaterYear == CY) %>% rename(bigyield = Yield_mm_mean_7) %>% select(-site_name)) %>% 
    mutate(delta_yield = log(Yield_mm_mean_7) - log(bigyield), site_name = factor(site_name, levels = little)) %>% 
    group_by(site_name) %>% mutate(cum_resid = cumsum(coalesce(delta_yield, 0)) + delta_yield*0) %>%
    ggplot() + 
    geom_line(aes(x = date, y = cum_resid, group = site_name, color = site_name)) + 
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    xlab("Date") + ylab("ln(Yield) cumulative residuals ") + 
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
  print("p5")
  
  
  #### EXCEEDANCE PROBABILITY - BIG G/LITTLE G CURRENT YEAR
  p6 <- ggplot() + 
    geom_line(data = exceedance %>% filter(site_name %in% little, WaterYear == CY) %>% mutate(site_name = factor(site_name, levels = little)), aes(x = exceedance, y = Yield_mm_mean_7_log, group = site_name, color = site_name)) +
    geom_line(data = exceedance %>% filter(site_name == big, WaterYear == CY), aes(x = exceedance, y = Yield_mm_mean_7_log), color = "black", size = 1.25) +
    xlab("Exceedance probability") + ylab("ln(Yield, mm)") + labs(color = "Little g") + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
  print("p6")
  
  
  #### EXCEEDANCE PROBABILITY MONTHLY
  exceedance_monthly <- dat_basin %>% 
    filter(site_name %in% c(little, big)) %>%
    filter(!is.na(Yield_mm_mean_7), WaterYear == CY) %>% 
    mutate(Yield_mm_mean_7_log = log(Yield_mm_mean_7),
           site_name = factor(site_name, levels = c(little, big)),
           MonthName = factor(MonthName, levels = months)) %>%
    group_by(site_name, Month) %>% 
    arrange(desc(Yield_mm_mean_7_log), .by_group = TRUE) %>% 
    mutate(exceedance = 100/length(Yield_mm_mean_7_log)*1:length(Yield_mm_mean_7_log)) %>%
    ungroup()
  p7 <- ggplot() + 
    geom_line(data = exceedance_monthly %>% filter(site_name %in% little), aes(x = exceedance, y = Yield_mm_mean_7_log, group = site_name, color = site_name)) +
    geom_line(data = exceedance_monthly %>% filter(site_name == big), aes(x = exceedance, y = Yield_mm_mean_7_log), color = "black", size = 1.25) +
    facet_wrap(~ factor(MonthName), nrow = 2) + 
    xlab("Exceedance probability") + ylab("ln(Yield, mm)") + labs(color = "Little g") + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
  print("p7")
  
  
  #### ANNUAL BIG-LITTLE DIFFERENCE
  mypvals <- tibble(type =  rep(NA, times = length(little)), 
                    month = rep(NA, times = length(little)),
                    site_name = rep(NA, times = length(little)), 
                    stat = rep(NA, times = length(little)),
                    pval = rep(NA, times = length(little)))
  for (i in 1:length(little)) {
    mytest <- ks.test(exceedance$Yield_mm_mean_7_log[exceedance$site_name == big],
                      exceedance$Yield_mm_mean_7_log[exceedance$site_name == little[i]], exact = TRUE)
    mypvals$type <- "annual"
    mypvals$month <- 0
    mypvals$site_name[i] <- little[i]
    mypvals$stat[i] <- mytest$statistic
    mypvals$pval[i] <- mytest$p.value
  }
  p8 <- mypvals %>% 
    mutate(site_name = factor(site_name, levels = little)) %>% 
    ggplot() + 
    geom_boxplot(aes(x = month, y = pval, group = month), fill = "grey90", outlier.shape = NA) +
    geom_point(aes(x = jitter(month, factor = 10), y = pval, color = site_name), size = 2) +
    ylim(0,1) + geom_hline(yintercept = 0.05, linetype = "dashed") +
    xlab("") + ylab("Kolmogorov-Smirnov test p-value") + scale_x_continuous(labels = "Annual", breaks = 0) +
    theme_bw() + theme(axis.title.x = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
  print("p8")
  
  
  #### MONTHLY BIG-LITTLE DIFFERENCE
  mypvals_monthly_list <- list()
  for (i in 1:length(little)) {
    mypvals_monthly <- tibble(type =  rep(NA, times = 12), 
                              month = rep(NA, times = 12),
                              site_name = rep(NA, times = 12), 
                              stat = rep(NA, times = 12),
                              pval = rep(NA, times = 12))
    for (j in 1:12) {
      big_exc <- exceedance_monthly$Yield_mm_mean_7_log[exceedance_monthly$site_name == big & exceedance_monthly$MonthName == months[j]]
      lit_exc <- exceedance_monthly$Yield_mm_mean_7_log[exceedance_monthly$site_name == little[i] & exceedance_monthly$MonthName == months[j]]
      if(length(lit_exc) < 20) next
      mytest <- ks.test(big_exc, lit_exc, exact = TRUE)
      mypvals_monthly$site_name[j] <- little[i]
      mypvals_monthly$type <- "monthly"
      mypvals_monthly$month[j] <- months[j]
      mypvals_monthly$stat[j] <- mytest$statistic
      mypvals_monthly$pval[j] <- mytest$p.value
    }
    mypvals_monthly_list[[i]] <- mypvals_monthly
  }
  mypvals_monthly <- do.call(rbind, mypvals_monthly_list) %>% 
    mutate(site_name = factor(site_name, levels = little),
           month = factor(month, levels = months), 
           month_num = as.numeric(month)) %>% 
    filter(!is.na(pval))
  # compute the loess
  # dum <- rbind(mypvals_monthly %>% mutate(month_num = month_num-12),
  #              mypvals_monthly,
  #              mypvals_monthly %>% mutate(month_num = month_num+12)) 
  # mylo <- loess(pval ~ month_num, dum, span = 0.25)
  # plot(pval ~ month_num, dum)
  # j <- order(dum$month_num)[(dim(mypvals_monthly)[1]+1):(dim(mypvals_monthly)[1]+dim(mypvals_monthly)[1])]
  # lines(dum$month_num[j], mylo$fitted[j], col = "red")
  # the plot
  p9 <- mypvals_monthly %>% 
    ggplot() + 
    geom_boxplot(aes(x = month, y = pval, group = month), fill = "grey90", outlier.shape = NA) +
    # geom_line(aes(x = dum$month_num[j], y = mylo$fitted[j]), linewidth = 1.25) +
    geom_smooth(aes(x = month_num, y = pval), linewidth = 1.25, color = "black", se = FALSE) +
    geom_point(aes(x = jitter(month_num), y = pval, color = site_name), size = 2) +
    ylim(0,1) + geom_hline(yintercept = (0.05), linetype = "dashed") +
    xlab("") + ylab("") + 
    labs(color = "") + theme_bw() + 
    theme(axis.title.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank())
  print("p9")
  
  
  #### BIG PLOT
  bigp <- ggarrange(ggarrange(mymap, ggarrange(NA, p2, nrow = 2, heights = c(0.2,1))),
                    ggarrange(p3, p4), 
                    ggarrange(p5, p6),
                    p7, 
                    ggarrange(p8, p9, widths = c(0.13, 0.85)), nrow = 5, heights = c(1.2,0.9,0.9,1.2,0.9))
  print("big plot!")
  # write out
  jpeg(paste("Big G Little g/Compare Distributions/BigGLittleG_BigPlot_", mybasin, "_", CY, ".jpg", sep = ""), height = 18, width = 10, units = "in", res = 500)
  # annotate_figure(bigp, fig.lab = "The West Brook, CY 2021", fig.lab.pos = "top.right", fig.lab.size = 24)
  print(annotate_figure(bigp, top = text_grob(paste(mybasin, ", CY ", CY, sep = ""), x = 0.75, y = -0.5, just = "centre", size = 24)))
  dev.off()
}



####--------------------------------------------###
#### Create Map Objects ####
####--------------------------------------------###

#### WEST BROOK
# NHD
# xycomid <- discover_nhdplus_id(siteinfo_sp %>% filter(site_name == "West Brook NWIS"))
# mynet <- navigate_nldi(nldi_feature = list(featureSource = "comid", featureID = xycomid), mode = "UT", distance_km = 50)
# plot(mynet$UT_flowlines, col = "blue")
# delineated
mysheds <- vect("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Watersheds/Mass_Watersheds.shp")
mysheds <- mysheds[mysheds$site_id == "WBR",]
mynet <- vect("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Streams/Mass_Streams.shp")
crs(mynet) <- crs(mysheds)
mynet <- crop(mynet, mysheds)

# hillshade
myrast <- rast("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/WestBrook_DEM_10m_nc.tif")
myrast <- mask(crop(myrast, mysheds), mysheds)
slo <- terrain(myrast, "slope", unit = "radians") 
asp <- terrain(myrast, "aspect", unit = "radians")
hill <- shade(slope = slo, aspect = asp, angle = 40, direction = 270)
hilldf <- as.data.frame(hill, xy = TRUE)

# get lakes
lakes <- get_waterbodies(AOI = siteinfo_sp %>% filter(site_name == big), buffer = 10000)
lakes <- lakes %>% filter(gnis_name %in% c("Northampton Reservoir Upper", "Northampton Reservoir"))

# little g points
mylittleg <- siteinfo_sp %>% filter(site_name %in% mylevels) %>% mutate(site_name = factor(site_name, levels = mylevels))
mybigg <- siteinfo_sp %>% filter(site_name == big)

# create map
# mapview(mynet$UT_flowlines) + mapview((lakes)) + mapview(siteinfo_sp %>% filter(basin == "West Brook"))
p1_WB <- ggplot() +
  geom_raster(data = hilldf, aes(x = x, y = y, fill = hillshade), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  geom_sf(data = st_as_sf(mysheds), color = "black", fill = NA, linewidth = 0.4) + 
  geom_sf(data = st_as_sf(mynet), color = "royalblue4", linewidth = 1) +
  geom_sf(data = lakes, color = "royalblue4", fill = "lightskyblue1", linewidth = 0.5) +
  geom_sf(data = mylittleg, aes(color = site_name), size = 3) +
  geom_sf(data = mylittleg, shape = 1, size = 3) +
  geom_sf(data = mybigg, size = 4) + geom_sf(data = mybigg, color = "white", size = 2.5) +
  labs(color = "") +
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")


#### STAUNTON RIVER
mysheds <- vect("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Watersheds/Shen_Watersheds.shp")
mysheds <- mysheds[mysheds$site_id == "PA_10FL",]
mynet <- vect("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Streams/Shen_Streams.shp")
crs(mynet) <- crs(mysheds)
mynet <- crop(mynet, mysheds)

# hillshade
myrast <- rast("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/Shenandoah_DEM_10m_nc.tif")
myrast <- mask(crop(myrast, mysheds), mysheds)
slo <- terrain(myrast, "slope", unit = "radians") 
asp <- terrain(myrast, "aspect", unit = "radians")
hill <- shade(slope = slo, aspect = asp, angle = 40, direction = 270)
hilldf <- as.data.frame(hill, xy = TRUE)

# little g points
mylittleg <- siteinfo_sp %>% filter(site_name %in% c("Staunton River 09", "Staunton River 07", "Staunton River 06", "Staunton River 03", "Staunton River 02")) %>% mutate(site_name = factor(site_name, levels = c("Staunton River 09", "Staunton River 07", "Staunton River 06", "Staunton River 03", "Staunton River 02")))
mybigg <- siteinfo_sp %>% filter(site_name == "Staunton River 10")

# create map
# mapview(mynet$UT_flowlines) + mapview((lakes)) + mapview(siteinfo_sp %>% filter(basin == "West Brook"))
p1_ST <- ggplot() +
  geom_raster(data = hilldf, aes(x = x, y = y, fill = hillshade), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  geom_sf(data = st_as_sf(mysheds), color = "black", fill = NA, linewidth = 0.4) + 
  geom_sf(data = st_as_sf(mynet), color = "royalblue4", linewidth = 1) +
  # geom_sf(data = lakes, color = "royalblue4", fill = "lightskyblue1", linewidth = 0.5) +
  geom_sf(data = mylittleg, aes(color = site_name), size = 3) +
  geom_sf(data = mylittleg, shape = 1, size = 3) +
  geom_sf(data = mybigg, size = 4) + geom_sf(data = mybigg, color = "white", size = 2.5) +
  labs(color = "") +
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")


#### PAINE RUN
mysheds <- vect("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Watersheds/Shen_Watersheds.shp")
mysheds <- mysheds[mysheds$site_id == "PR_10FL",]
mynet <- vect("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Streams/Shen_Streams.shp")
crs(mynet) <- crs(mysheds)
mynet <- crop(mynet, mysheds)

# hillshade
myrast <- rast("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/Shenandoah_DEM_10m_nc.tif")
myrast <- mask(crop(myrast, mysheds), mysheds)
slo <- terrain(myrast, "slope", unit = "radians") 
asp <- terrain(myrast, "aspect", unit = "radians")
hill <- shade(slope = slo, aspect = asp, angle = 40, direction = 270)
hilldf <- as.data.frame(hill, xy = TRUE)

# little g points
mylittleg <- siteinfo_sp %>% filter(site_name %in% c("Paine Run 08", "Paine Run 07", "Paine Run 06", "Paine Run 02", "Paine Run 01")) %>% mutate(site_name = factor(site_name, levels = c("Paine Run 08", "Paine Run 07", "Paine Run 06", "Paine Run 02", "Paine Run 01")))
mybigg <- siteinfo_sp %>% filter(site_name == "Paine Run 10")

# create map
# mapview(mynet$UT_flowlines) + mapview((lakes)) + mapview(siteinfo_sp %>% filter(basin == "West Brook"))
p1_PA <- ggplot() +
  geom_raster(data = hilldf, aes(x = x, y = y, fill = hillshade), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  geom_sf(data = st_as_sf(mysheds), color = "black", fill = NA, linewidth = 0.4) + 
  geom_sf(data = st_as_sf(mynet), color = "royalblue4", linewidth = 1) +
  # geom_sf(data = lakes, color = "royalblue4", fill = "lightskyblue1", linewidth = 0.5) +
  geom_sf(data = mylittleg, aes(color = site_name), size = 3) +
  geom_sf(data = mylittleg, shape = 1, size = 3) +
  geom_sf(data = mybigg, size = 4) + geom_sf(data = mybigg, color = "white", size = 2.5) +
  labs(color = "") +
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")



#### FLATHEAD
mysheds <- vect("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Watersheds/Flat_Watersheds.shp")
mysheds <- mysheds[mysheds$site_id == "NFF",]
mynet <- vect("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/EcoDrought-Analysis/Watershed Delineation/Streams/Flat_Streams.shp")
crs(mynet) <- crs(mysheds)
mynet <- crop(mynet, mysheds)

# hillshade
myrast <- rast("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/Spatial data/Elevation/NorthForkFlathead_DEM_10m_nc.tif")
myrast <- mask(crop(myrast, mysheds), mysheds)
slo <- terrain(myrast, "slope", unit = "radians") 
asp <- terrain(myrast, "aspect", unit = "radians")
hill <- shade(slope = slo, aspect = asp, angle = 40, direction = 270)
hilldf <- as.data.frame(hill, xy = TRUE)

# get lakes
lakes <- get_waterbodies(AOI = siteinfo_sp %>% filter(site_name == "North Fork Flathead River NWIS"), buffer = 100000)
lakes <- st_transform(lakes, crs(mysheds))
lakes <- st_intersection(lakes, st_as_sf(mysheds))
lakes <- lakes %>% filter(gnis_name %in% c("Moose Lake", "Mud Lake", "Cyclone Lake", "Winona Lake", "Logging Lake", "Quartz Lake",
                                  "Rogers Lake", "Trout Lake", "Middle Quartz Lake", "Lower Quartz Lake", "Kintla Lake", "Bowman Lake"))
mapview(lakes)

# little g points
mylevels <- c("BigCreekLower", "LangfordCreekLower", "LangfordCreekUpper", "BigCreekMiddle", "BigCreekUpper", "HallowattCreekLower", "SkookoleelCreek", "NicolaCreek", "WernerCreek", 
              "McGeeCreekTrib", "McGeeCreekLower", "McGeeCreekUpper",
              "CoalCreekLower", "MeadowCreek", "CycloneCreekLower", "CycloneCreekMiddle", "CycloneCreekUpper", "CoalCreekMiddle", "CoalCreekNorth", "CoalCreekHeadwaters")
mylittleg <- siteinfo_sp %>% filter(site_name %in% mylevels) %>% mutate(site_name = factor(site_name, levels = mylevels))
mybigg <- siteinfo_sp %>% filter(site_name == "North Fork Flathead River NWIS")

# create map
# mapview(mynet$UT_flowlines) + mapview((lakes)) + mapview(siteinfo_sp %>% filter(basin == "West Brook"))
p1_FL <- ggplot() +
  geom_raster(data = hilldf, aes(x = x, y = y, fill = hillshade), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  geom_sf(data = st_as_sf(mysheds), color = "black", fill = NA, linewidth = 0.4) + 
  geom_sf(data = st_as_sf(mynet), color = "royalblue4", linewidth = 1) +
  geom_sf(data = lakes, color = "royalblue4", fill = "lightskyblue1", linewidth = 0.5) +
  geom_sf(data = mylittleg, aes(color = site_name), size = 3) +
  geom_sf(data = mylittleg, shape = 1, size = 3) +
  geom_sf(data = mybigg, size = 4) + geom_sf(data = mybigg, color = "white", size = 2.5) +
  labs(color = "") +
  scale_x_continuous(limits = c(-114.525,-114), expand = c(0,0)) + scale_y_continuous(limits = c(48.475,48.75), expand = c(0,0)) +
  theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")


ggplot() +
  geom_raster(data = hilldf, aes(x = x, y = y, fill = hillshade), show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys") +
  xlim(114.6,114) + ylim(48.5,48.75)


####--------------------------------------------###
#### Generate plots with functions ####
####--------------------------------------------###

unique(dat$basin)
unique(dat$site_name)


# West Brook 2021
residualplots(mybasin = "West Brook",
              CY = 2021,
              little = c("West Brook Lower", "Mitchell Brook", "Jimmy Brook", "Obear Brook Lower", "West Brook Upper", "West Brook Reservoir", "Sanderson Brook", "Avery Brook", "West Whately Brook"),
              big = "West Brook NWIS")

bigplotfun(mybasin = "West Brook",
           CY = 2021,
           little = c("West Brook Lower", "Mitchell Brook", "Jimmy Brook", "Obear Brook Lower", "West Brook Upper", "West Brook Reservoir", "Sanderson Brook", "Avery Brook", "West Whately Brook"),
           big = "West Brook NWIS",
           super = "South River Conway NWIS",
           mymap = p1_WB,
           supergyears = c(1981:2024))

bigplotfun(mybasin = "West Brook",
           CY = 2022,
           little = c("West Brook Lower", "Mitchell Brook", "Jimmy Brook", "Obear Brook Lower", "West Brook Upper", "West Brook Reservoir", "Sanderson Brook", "Avery Brook", "West Whately Brook"),
           big = "West Brook NWIS",
           super = "South River Conway NWIS",
           mymap = p1_WB,
           supergyears = c(1981:2024))


# Staunton River 2021
residualplots(mybasin = "Staunton River",
              CY = 2021,
              little = c("Staunton River 09", "Staunton River 07", "Staunton River 06", "Staunton River 03", "Staunton River 02"),
              big = "Staunton River 10")

bigplotfun(mybasin = "Staunton River",
           CY = 2021,
           little = c("Staunton River 09", "Staunton River 07", "Staunton River 06", "Staunton River 03", "Staunton River 02"),
           big = "Staunton River 10",
           super = "Staunton River 10",
           mymap = p1_ST,
           supergyears = c(1994:2023))


bigplotfun(mybasin = "Staunton River",
           CY = 2022,
           little = c("Staunton River 09", "Staunton River 07", "Staunton River 06", "Staunton River 03", "Staunton River 02"),
           big = "Staunton River 10",
           super = "Staunton River 10",
           mymap = p1_ST,
           supergyears = c(1994:2023))

bigplotfun(mybasin = "Staunton River",
           CY = 2023,
           little = c("Staunton River 09", "Staunton River 07", "Staunton River 06", "Staunton River 03", "Staunton River 02"),
           big = "Staunton River 10",
           super = "Staunton River 10",
           mymap = p1_ST,
           supergyears = c(1994:2023))


# Paine Run
residualplots(mybasin = "Paine Run",
              CY = 2021,
              little = c("Paine Run 08", "Paine Run 07", "Paine Run 06", "Paine Run 02", "Paine Run 01"),
              big = "Paine Run 10")

bigplotfun(mybasin = "Paine Run",
           CY = 2021,
           little = c("Paine Run 08", "Paine Run 07", "Paine Run 06", "Paine Run 02", "Paine Run 01"),
           big = "Paine Run 10",
           super = "Paine Run 10",
           mymap = p1_PA,
           supergyears = c(1994:2023))

bigplotfun(mybasin = "Paine Run",
           CY = 2022,
           little = c("Paine Run 08", "Paine Run 07", "Paine Run 06", "Paine Run 02", "Paine Run 01"),
           big = "Paine Run 10",
           super = "Paine Run 10",
           mymap = p1_PA,
           supergyears = c(1994:2023))





























############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################

mybasin <- "Staunton River"
CY <- 2021
little <- c("Staunton River 09", "Staunton River 07", "Staunton River 06", "Staunton River 03", "Staunton River 02")
big <- "Staunton River 10"
super <- "Staunton River 10"
mymap <- "p1_ST"
supergyears <- c(1994:2023)


#### essential elements
dat_basin <- dat %>% filter(basin == mybasin)
months <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")

#### MAP
p1 <- mymap


#### HYDROGRAPHS IN YIELD
p2 <- ggplot() + 
  geom_line(data = dat_basin %>% filter(WaterYear == CY, site_name %in% little) %>% mutate(site_name = factor(site_name, levels = little)), aes(x = date, y = log(Yield_mm_mean_7), group = site_name, color = site_name)) +
  geom_line(data = dat_basin %>% filter(WaterYear == CY, site_name == big), aes(x = date, y = log(Yield_mm_mean_7)), color = "black", size = 1.25) +
  xlab("Date") + ylab("ln(Yield, mm)") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")


#### TOTAL ANNUAL YIELD
# get total yield per year and convert to percentiles
yeartotals <- dat_basin %>% 
  filter(site_name == super, WaterYear %in% supergyears) %>% 
  group_by(WaterYear) %>% 
  summarize(totalyield = sum(Yield_mm, na.rm = TRUE)) %>%
  filter(!is.na(totalyield)) %>%
  mutate(percentile = percent_rank(totalyield))
p3 <- ggplot() + 
  geom_line(data = yeartotals, aes(x = WaterYear, y = totalyield), color = "grey40") + 
  geom_point(data = yeartotals %>% filter(WaterYear == CY), aes(x = WaterYear, y = totalyield)) +
  xlab("Climate year") + ylab("Total annual yield (mm)") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#### EXCEEDANCE PROBABILITY - SUPER G PERIOD OF RECORD
exceedance <- dat_basin %>% 
  filter(site_name %in% c(little, big, super)) %>%
  filter(!is.na(Yield_mm_mean_7)) %>% 
  mutate(Yield_mm_mean_7_log = log(Yield_mm_mean_7)) %>%
  group_by(site_name, WaterYear) %>% 
  arrange(desc(Yield_mm_mean_7_log), .by_group = TRUE) %>% 
  mutate(exceedance = 100/length(Yield_mm_mean_7_log)*1:length(Yield_mm_mean_7_log)) %>%
  ungroup()
p4 <- ggplot() + 
  geom_line(data = exceedance %>% filter(site_name == super, WaterYear %in% supergyears), aes(x = exceedance, y = Yield_mm_mean_7_log, group = WaterYear, color = WaterYear), size = 0.25) +
  geom_line(data = exceedance %>% filter(site_name == super, WaterYear == CY), aes(x = exceedance, y = Yield_mm_mean_7_log), color = "black", size = 1.25) +
  geom_text(aes(x = 50, y = Inf, label = paste("Super G: ", super, "\nPeriod of record: ", min(supergyears), "-", max(supergyears), "\nCurrent year: ", CY, " (", round(yeartotals$percentile[yeartotals$WaterYear == CY]*100), "th perc.)", sep = "")), vjust = 1.2) +
  scale_color_continuous(trans = "reverse") +
  xlab("Exceedance probability") + ylab("ln(Yield, mm)") + labs(color = "Climate year") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.15,0.23), 
        legend.text = element_text(size = 9), legend.title = element_text(size = 9), legend.key.height = unit(0.4, "cm"))


#### CUMULATIVE YIELD RESIDUALS
p5 <- dat_basin %>% select(site_name, site_id, basin, region, designation, date, WaterYear, Yield_mm_mean_7) %>% filter(site_name %in% little, WaterYear == CY) %>% 
  left_join(dat_basin %>% select(basin, site_name, date, WaterYear, Yield_mm_mean_7) %>% filter(site_name == big, WaterYear == CY) %>% rename(bigyield = Yield_mm_mean_7) %>% select(-site_name)) %>% 
  mutate(delta_yield = log(Yield_mm_mean_7) - log(bigyield), site_name = factor(site_name, levels = little)) %>% 
  group_by(site_name) %>% mutate(cum_resid = cumsum(coalesce(delta_yield, 0)) + delta_yield*0) %>%
  ggplot() + 
  geom_line(aes(x = date, y = cum_resid, group = site_name, color = site_name)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  xlab("Date") + ylab("ln(Yield) cumulative residuals ") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")


#### EXCEEDANCE PROBABILITY - BIG G/LITTLE G CURRENT YEAR
p6 <- ggplot() + 
  geom_line(data = exceedance %>% filter(site_name %in% little, WaterYear == CY) %>% mutate(site_name = factor(site_name, levels = little)), aes(x = exceedance, y = Yield_mm_mean_7_log, group = site_name, color = site_name)) +
  geom_line(data = exceedance %>% filter(site_name == big, WaterYear == CY), aes(x = exceedance, y = Yield_mm_mean_7_log), color = "black", size = 1.25) +
  xlab("Exceedance probability") + ylab("ln(Yield, mm)") + labs(color = "Little g") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")


#### EXCEEDANCE PROBABILITY MONTHLY
exceedance_monthly <- dat_basin %>% 
  filter(site_name %in% c(little, big)) %>%
  filter(!is.na(Yield_mm_mean_7), WaterYear == CY) %>% 
  mutate(Yield_mm_mean_7_log = log(Yield_mm_mean_7),
         site_name = factor(site_name, levels = c(little, big)),
         MonthName = factor(MonthName, levels = months)) %>%
  group_by(site_name, Month) %>% 
  arrange(desc(Yield_mm_mean_7_log), .by_group = TRUE) %>% 
  mutate(exceedance = 100/length(Yield_mm_mean_7_log)*1:length(Yield_mm_mean_7_log)) %>%
  ungroup()
p7 <- ggplot() + 
  geom_line(data = exceedance_monthly %>% filter(site_name %in% little), aes(x = exceedance, y = Yield_mm_mean_7_log, group = site_name, color = site_name)) +
  geom_line(data = exceedance_monthly %>% filter(site_name == big), aes(x = exceedance, y = Yield_mm_mean_7_log), color = "black", size = 1.25) +
  facet_wrap(~ factor(MonthName), nrow = 2) + 
  xlab("Exceedance probability") + ylab("ln(Yield, mm)") + labs(color = "Little g") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")


#### ANNUAL BIG-LITTLE DIFFERENCE
mypvals <- tibble(type =  rep(NA, times = length(little)), 
                  month = rep(NA, times = length(little)),
                  site_name = rep(NA, times = length(little)), 
                  stat = rep(NA, times = length(little)),
                  pval = rep(NA, times = length(little)))
for (i in 1:length(little)) {
  mytest <- ks.test(exceedance$Yield_mm_mean_7_log[exceedance$site_name == big],
                    exceedance$Yield_mm_mean_7_log[exceedance$site_name == little[i]], exact = TRUE)
  mypvals$type <- "annual"
  mypvals$month <- 0
  mypvals$site_name[i] <- little[i]
  mypvals$stat[i] <- mytest$statistic
  mypvals$pval[i] <- mytest$p.value
}
p8 <- mypvals %>% 
  mutate(site_name = factor(site_name, levels = little)) %>% 
  ggplot() + 
  geom_boxplot(aes(x = month, y = pval, group = month), fill = "grey90", outlier.shape = NA) +
  geom_point(aes(x = jitter(month, factor = 10), y = pval, color = site_name), size = 2) +
  ylim(0,1) + geom_hline(yintercept = 0.05, linetype = "dashed") +
  xlab("") + ylab("Kolmogorov-Smirnov test p-value") + scale_x_continuous(labels = "Annual", breaks = 0) +
  theme_bw() + theme(axis.title.x = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")


#### MONTHLY BIG-LITTLE DIFFERENCE
mypvals_monthly_list <- list()
for (i in 1:length(little)) {
  mypvals_monthly <- tibble(type =  rep(NA, times = 12), 
                            month = rep(NA, times = 12),
                            site_name = rep(NA, times = 12), 
                            stat = rep(NA, times = 12),
                            pval = rep(NA, times = 12))
  for (j in 1:12) {
    big_exc <- exceedance_monthly$Yield_mm_mean_7_log[exceedance_monthly$site_name == big & exceedance_monthly$MonthName == months[j]]
    lit_exc <- exceedance_monthly$Yield_mm_mean_7_log[exceedance_monthly$site_name == little[i] & exceedance_monthly$MonthName == months[j]]
    if(length(lit_exc) < 20) next
    mytest <- ks.test(big_exc, lit_exc, exact = TRUE)
    mypvals_monthly$site_name[j] <- little[i]
    mypvals_monthly$type <- "monthly"
    mypvals_monthly$month[j] <- months[j]
    mypvals_monthly$stat[j] <- mytest$statistic
    mypvals_monthly$pval[j] <- mytest$p.value
  }
  mypvals_monthly_list[[i]] <- mypvals_monthly
}
mypvals_monthly <- do.call(rbind, mypvals_monthly_list) %>% 
  mutate(site_name = factor(site_name, levels = little),
         month = factor(month, levels = months), 
         month_num = as.numeric(month)) %>% 
  filter(!is.na(pval))
# compute the loess
# dum <- rbind(mypvals_monthly %>% mutate(month_num = month_num-12),
#              mypvals_monthly,
#              mypvals_monthly %>% mutate(month_num = month_num+12)) 
# mylo <- loess(pval ~ month_num, dum, span = 0.25)
# plot(pval ~ month_num, dum)
# j <- order(dum$month_num)[(dim(mypvals_monthly)[1]+1):(dim(mypvals_monthly)[1]+dim(mypvals_monthly)[1])]
# lines(dum$month_num[j], mylo$fitted[j], col = "red")
# the plot
p9 <- mypvals_monthly %>% 
  ggplot() + 
  geom_boxplot(aes(x = month, y = pval, group = month), fill = "grey90", outlier.shape = NA) +
  # geom_line(aes(x = dum$month_num[j], y = mylo$fitted[j]), linewidth = 1.25) +
  geom_smooth(aes(x = month_num, y = pval), linewidth = 1.25, color = "black", se = FALSE) +
  geom_point(aes(x = jitter(month_num), y = pval, color = site_name), size = 2) +
  ylim(0,1) + geom_hline(yintercept = (0.05), linetype = "dashed") +
  xlab("") + ylab("") + 
  labs(color = "") + theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank())


#### BIG PLOT
bigp <- ggarrange(ggarrange(p1, ggarrange(NA, p2, nrow = 2, heights = c(0.2,1))),
                  ggarrange(p3, p4), 
                  ggarrange(p5, p6),
                  p7, 
                  ggarrange(p8, p9, widths = c(0.13, 0.85)), nrow = 5, heights = c(1.2,0.9,0.9,1.2,0.9))
# write out
jpeg(paste("Big G Little g/Compare Distributions/BigGLittleG_BigPlot2_", mybasin, "_", CY, ".jpg", sep = ""), height = 18, width = 10, units = "in", res = 500)
# annotate_figure(bigp, fig.lab = "The West Brook, CY 2021", fig.lab.pos = "top.right", fig.lab.size = 24)
annotate_figure(bigp, top = text_grob(paste(mybasin, ", CY ", CY, sep = ""), x = 0.75, y = -0.5, just = "centre", size = 24))
dev.off()





############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################


####--------------------------------------------###
#### Big G/little g comparison of drought metrics ####
####--------------------------------------------###

#### see Hammond scripts for more robust methods to calculate low flow metrics

# threshold drought percentile
dat$thresh <- dat$thresh20

# define drought periods, calculate deficit
dat_summ <- dat %>% filter(site_name != "West Brook 0", WaterYear == 2021) %>%
  mutate(designation = factor(designation, levels = c("big", "little")),
         isdrought = ifelse(Yield_mm_mean_7 <= thresh, 1, 0), 
         deficit = ifelse(isdrought == 1, thresh - Yield_mm_mean_7, 0)) %>%
  filter(!is.na(isdrought)) %>%
  group_by(station_no, site_name, site_id, basin, region, lat, long, elev_ft, area_sqmi, designation, WaterYear) %>%
  summarize(duration = sum(isdrought, na.rm = TRUE), 
            deficit = sum(deficit, na.rm = TRUE),
            q7min = min(Yield_mm_mean_7, na.rm = TRUE),
            q7mindate = doy[Yield_mm_mean_7 == min(Yield_mm_mean_7, na.rm = TRUE)]) %>%
  ungroup() %>%
  group_by(basin) %>%
  mutate(area_perc = percent_rank(area_sqmi)) %>%
  ungroup()

# convert to wide format to calculate difference in metrics
dat_summ_little <- dat_summ %>% filter(designation == "little")
dat_summ_big <- dat_summ %>% filter(designation == "big")
dat_summ_diff <- dat_summ_little %>% 
  left_join(dat_summ_big %>% 
              select(basin, WaterYear, duration, deficit, q7min, q7mindate) %>% 
              rename(duration_big = duration, deficit_big = deficit, q7min_big = q7min, q7mindate_big = q7mindate)) %>%
  mutate(duration_diff = duration - duration_big,
         deficit_diff = deficit - deficit_big,
         q7min_diff = q7min - q7min_big,
         q7mindate_diff = q7mindate - q7mindate_big)


# raw comparison
# draft plot objects
p1 <- dat_summ %>% 
  ggplot(aes(x = designation, y = duration, color = area_perc)) + geom_jitter(width = 0.1, height = 0) + 
  xlab("") + ylab("Duration (days)") + scale_x_discrete(labels = c("Big G", "Little G")) +
  theme_bw() + scale_color_continuous(trans = "reverse") + 
  facet_wrap(~ basin, scales = "free_y", nrow = 2) 

p2 <- dat_summ %>% 
  ggplot(aes(x = designation, y = deficit, color = area_perc)) + geom_jitter(width = 0.1, height = 0) + 
  xlab("") + ylab("Deficit (mm)") + scale_x_discrete(labels = c("Big G", "Little G")) +
  theme_bw() + scale_color_continuous(trans = "reverse") + 
  facet_wrap(~ basin, scales = "free_y", nrow = 2) 

p3 <- dat_summ %>% 
  ggplot(aes(x = designation, y = q7min, color = area_perc)) + geom_jitter(width = 0.1, height = 0) + 
  xlab("") + ylab("7-day minimum yield (mm)") + scale_x_discrete(labels = c("Big G", "Little G")) +
  theme_bw() + scale_color_continuous(trans = "reverse") + 
  facet_wrap(~ basin, scales = "free_y", nrow = 2) 

p4 <- dat_summ %>% 
  ggplot(aes(x = designation, y = q7mindate, color = area_perc)) + geom_jitter(width = 0.1, height = 0) + 
  xlab("") + ylab("Date of 7-day minimum yield (day of year)") + scale_x_discrete(labels = c("Big G", "Little G")) +
  theme_bw() + scale_color_continuous(trans = "reverse") + 
  facet_wrap(~ basin, scales = "free_y", nrow = 2) 

jpeg("Big G Little g/BigGLittleG_DroughtMetrics.jpg", height = 7, width = 11, units = "in", res = 500)
ggarrange(p1, p2, p3, p4, nrow = 1, common.legend = TRUE)
dev.off()



# differences
# draft plot objects
p1 <- dat_summ_diff %>% 
  ggplot(aes(x = basin, y = duration_diff)) + 
  geom_boxplot(fill = "grey80") + geom_jitter(aes(color = area_perc), width = 0.1, height = 0.1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  xlab("") + ylab("Duration (days), difference from Big G") + 
  theme_bw() + scale_color_continuous(trans = "reverse") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <- dat_summ_diff %>% 
  ggplot(aes(x = basin, y = deficit_diff)) + 
  geom_boxplot(fill = "grey80") + geom_jitter(aes(color = area_perc), width = 0.1, height = 0.1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  xlab("") + ylab("Deficit (mm), difference from Big G") + 
  theme_bw() + scale_color_continuous(trans = "reverse") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p3 <- dat_summ_diff %>% 
  ggplot(aes(x = basin, y = q7min_diff)) + 
  geom_boxplot(fill = "grey80") + geom_jitter(aes(color = area_perc), width = 0.1, height = 0.1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  xlab("") + ylab("7-day minimum yield (mm), difference from Big G") + 
  theme_bw() + scale_color_continuous(trans = "reverse") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p4 <- dat_summ_diff %>% 
  ggplot(aes(x = basin, y = q7mindate_diff)) + 
  geom_boxplot(fill = "grey80") + geom_jitter(aes(color = area_perc), width = 0.1, height = 0.1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  xlab("") + ylab("Date of 7-day minimum yield (mm), difference from Big G") + 
  theme_bw() + scale_color_continuous(trans = "reverse") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


jpeg("Big G Little g/BigGLittleG_DroughtMetrics_Difference2.jpg", height = 6, width = 11, units = "in", res = 500)
ggarrange(p1, p2, p3, p4, nrow = 1, common.legend = TRUE)
dev.off()







