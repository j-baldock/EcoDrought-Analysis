############################################################################
#
# Project: EcoDrought
# Purpose: Explore effects of monthly/annual water availability on Big-little difference
# Author: Jeff Baldock, jbaldock@usgs.gov
#
###########################################################################


library(tidyverse)
library(sf)
library(mapview)
library(fasstr)
library(ggpubr)
library(dygraphs)


####--------------------------------------------###
#### Site information and daily data ####
####--------------------------------------------###

# site information and locations
siteinfo <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_SiteInformation.csv")
siteinfo_sp <- st_as_sf(siteinfo, coords = c("long", "lat"), crs = 4326)
mapview(siteinfo_sp, zcol = "designation")

# flow/yield (and temp) data 
dat <- read_csv("C:/Users/jbaldock/OneDrive - DOI/Documents/USGS/EcoDrought/EcoDrought Working/Data/EcoDrought_FlowTempData_DailyWeekly.csv") %>%
  filter(!site_name %in% c("WoundedBuckCreek"))

# add water/climate year variables
dat <- add_date_variables(dat, dates = date, water_year_start = 4)


####--------------------------------------------###
#### Separate/join big-little data ####
####--------------------------------------------###

# pull out big G data
dat_big <- dat %>% filter(subbasin != "Duck Creek", site_name %in% c("West Brook NWIS", "Paine Run 10", "Piney River 10", "Staunton River 10", "Spread Creek Dam", "North Fork Flathead River NWIS", "Shields River ab Smith NWIS", "Donner Blitzen River nr Frenchglen NWIS")) 

# pull out little G data, assign big G site name
dat_little <- dat %>% 
  filter(designation %in% c("little", "medium"), basin != "Duck Creek",
         !site_name %in% c("West Brook NWIS", "West Brook 0", "Paine Run 10", "Piney River 10", "Staunton River 10", "Spread Creek Dam", "Shields River ab Smith NWIS", "Brackett Creek" )) %>%
  mutate(site_name_big = ifelse(basin == "Flathead", "North Fork Flathead River NWIS",
                                ifelse(basin == "West Brook", "West Brook NWIS", 
                                       ifelse(basin == "Paine Run", "Paine Run 10", 
                                              ifelse(basin == "Piney River", "Piney River 10",
                                                     ifelse(basin == "Staunton River", "Staunton River 10",
                                                            ifelse(basin == "Shields River", "Shields River ab Smith NWIS",
                                                                   ifelse(basin == "Snake River", "Spread Creek Dam", "Donner Blitzen River nr Frenchglen NWIS"))))))))

dat_little %>% group_by(basin) %>% summarize(site_name_big = unique(site_name_big))

# Join big-little discharge data
dat_Gg <- dat_little %>%
  select(site_name, site_name_big, basin, subbasin, date, Month, MonthName, WaterYear, Yield_filled_mm_7) %>% rename(yield_little = Yield_filled_mm_7) %>%
  left_join(dat_big %>% select(site_name, date, Yield_filled_mm_7) %>% rename(site_name_big = site_name, yield_big = Yield_filled_mm_7)) %>%
  drop_na() %>% mutate(MonthName = as.character(MonthName))
sort(unique(dat_Gg$site_name))


####--------------------------------------------###
#### Compute monthly big-little difference ####
####--------------------------------------------###

mysites <- unique(dat_Gg$site_name)
kscompare_list <- list()
for (i in 1:length(mysites)) {
  d <- dat_Gg %>% filter(site_name == mysites[i])
  yrs <- unique(d$WaterYear)
  kscompare_list_yr <- list()
  for(j in 1:length(yrs)) {
    dy <- d %>% filter(WaterYear == yrs[j])
    mypvals_monthly <- tibble(site_name = rep(NA, times = 12), 
                              site_name_big = rep(NA, times = 12), 
                              WaterYear = rep(NA, times = 12),
                              Month = rep(NA, times = 12),
                              MonthName = rep(NA, times = 12),
                              days = rep(NA, times = 12),
                              stat = rep(NA, times = 12),
                              pval = rep(NA, times = 12))
    for(k in 1:12) {
      dym <- dy %>% filter(Month == k)
      if(dim(dym)[1] < 25) next
      mytest <- ks.test(dym$yield_little, dym$yield_big, exact = TRUE)
      mypvals_monthly$site_name[k] <- mysites[i]
      mypvals_monthly$site_name_big[k] <- unique(dym$site_name_big)
      mypvals_monthly$WaterYear[k] <- yrs[j]
      mypvals_monthly$Month[k] <- k
      mypvals_monthly$MonthName[k] <- unique(dym$MonthName)
      mypvals_monthly$days[k] <- dim(dym)[1]
      mypvals_monthly$stat[k] <- mytest$statistic
      mypvals_monthly$pval[k] <- mytest$p.value
      }
    kscompare_list_yr[[j]] <- mypvals_monthly
    }
  kscompare_list[[i]] <- do.call(rbind, kscompare_list_yr)
}
kscompare <- do.call(rbind, kscompare_list) %>% drop_na()


####--------------------------------------------###
#### Get total Big G yield and join to KS diffs ####
####--------------------------------------------###

# get total annual yield by big G site
totyield_annual <- dat_big %>% 
  group_by(site_name, WaterYear, basin, subbasin) %>% 
  summarize(days = n(), yield_annual = sum(Yield_filled_mm, na.rm = TRUE)) %>%
  filter(!is.na(yield_annual), days >= 360) %>%
  ungroup() %>%
  rename(site_name_big = site_name) %>%
  select(site_name_big, WaterYear, yield_annual)

# get total monthly yield by big G site
totyield_monthly <- dat_big %>% 
  group_by(site_name, WaterYear, basin, subbasin, Month) %>% 
  summarize(days = n(), yield_monthly = sum(Yield_filled_mm, na.rm = TRUE)) %>%
  filter(!is.na(yield_monthly), days >= 28) %>%
  ungroup() %>%
  rename(site_name_big = site_name) %>%
  select(site_name_big, WaterYear, Month, yield_monthly)

# join KS comparison df with annual/monthly water availability
kscompare <- kscompare %>% left_join(totyield_annual) %>% left_join(totyield_monthly) %>%
  mutate(MonthName = factor(MonthName, levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))) %>%
  left_join(siteinfo %>% select(site_name, basin))
range(kscompare$pval)
range(kscompare$stat)


####--------------------------------------------###
#### Explore effects of water availability on big-little differences ####
####--------------------------------------------###

# basic plots
plot(pval ~ stat, kscompare)
plot(qlogis(pval) ~ qlogis(stat), kscompare)
hist(kscompare$pval)
hist(kscompare$stat)
hist(log(kscompare$pval))
hist(log(kscompare$stat))

# everything together
kscompare %>% ggplot(aes(x = log(yield_monthly), y = log(pval))) + 
  geom_point(aes(group = MonthName, color = MonthName)) + 
  geom_smooth(method = "lm") + geom_abline(slope = 0, intercept = log(0.05))

# kscompare %>% ggplot() + geom_point(aes(x = log(yield_annual), y = (pval))) + facet_wrap(~ basin + MonthName)
# kscompare %>% ggplot(aes(x = log(yield_monthly), y = (pval))) + 
#   geom_point(aes(group = MonthName, color = MonthName)) + facet_wrap(~ basin) + 
#   geom_smooth(method = "glm", method.args = list(family = "binomial")) + geom_abline(slope = 0, intercept = (0.05))

# Plot all sites, facet by basin
jpeg("./Big G Little g/Test for Differences/KS_pval-by-monthlyyield.jpg", units = "in", res = 500, width = 8, height = 6.5)
kscompare %>% ggplot(aes(x = log(yield_monthly), y = log(pval))) + 
  geom_point(aes(group = MonthName, color = MonthName)) + facet_wrap(~ basin) + 
  geom_smooth(method = "lm") + geom_abline(slope = 0, intercept = log(0.05), linetype = "dashed")
dev.off()

# West Brook only, facet by year
jpeg("./Big G Little g/Test for Differences/KS_pval-by-monthlyyield_WB_byyear.jpg", units = "in", res = 500, width = 8, height = 6.5)
kscompare %>% filter(basin == "West Brook") %>%
  ggplot(aes(x = log(yield_monthly), y = log(pval))) + 
  geom_point(aes(group = MonthName, color = MonthName)) + facet_wrap(~ basin + WaterYear) + 
  geom_smooth(method = "lm") + geom_abline(slope = 0, intercept = log(0.05), linetype = "dashed")
dev.off()

# West Brook only, facet by month
jpeg("./Big G Little g/Test for Differences/KS_pval-by-monthlyyield_WB_bymonth.jpg", units = "in", res = 500, width = 8, height = 6.5)
kscompare %>% filter(site_name_big == "West Brook NWIS") %>%
  ggplot(aes(x = log(yield_monthly), y = log(pval))) + 
  geom_point(aes(group = MonthName, color = MonthName)) + facet_wrap(~ basin + MonthName) + 
  geom_smooth(method = "lm") + geom_abline(slope = 0, intercept = log(0.05), linetype = "dashed")
dev.off()

# West Brook only, facet by site
jpeg("./Big G Little g/Test for Differences/KS_pval-by-monthlyyield_WB_bysite.jpg", units = "in", res = 500, width = 8, height = 6.5)
kscompare %>% filter(site_name_big == "West Brook NWIS") %>%
  ggplot(aes(x = log(yield_monthly), y = log(pval))) + 
  geom_point(aes(group = MonthName, color = MonthName)) + facet_wrap(~ basin + site_name) + 
  geom_smooth(method = "lm") + geom_abline(slope = 0, intercept = log(0.05), linetype = "dashed")
dev.off()







# kscompare %>% ggplot(aes(x = log(yield_monthly), y = car::logit(pval))) + geom_point(aes(group = MonthName, color = MonthName)) + facet_wrap(~ basin, scales = "free") + geom_smooth(method = "lm")
# kscompare %>% ggplot(aes(x = log(yield_monthly), y = qlogis(pval))) + geom_point(aes(group = MonthName, color = MonthName)) + facet_wrap(~ basin) + geom_smooth(method = "lm")







