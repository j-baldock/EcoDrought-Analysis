


library(tidyverse)
library(sf)
library(mapview)
library(fasstr)
library(ggpubr)
library(dygraphs)
library(GGally)
library(moments)
library(fmsb)


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


# filter to West Brook, sites of interest
dat_wb <- dat %>% filter(basin == "West Brook", WaterYear == 2021, 
                         site_name %in% c("West Brook Lower", "Mitchell Brook", "Jimmy Brook", "Obear Brook Lower", "West Brook Upper", "West Brook Reservoir", "Sanderson Brook", "Avery Brook", "West Whately Brook", "West Brook NWIS"))

mydf <- data.frame(dat %>% filter(site_name == "South River Conway NWIS") %>% drop_na(flow_mean))
names(mydf) <- c("date", "discharge")
calc_magnifSeven(mydf[c("date", "flow_mean")])
calc_magnifSeven(sampleData2[c("date", "discharge")])


sampleData
summary(sampleData)
sampleData2 <- sampleData
sampleData2$discharge <- as.numeric(sampleData2$discharge)

dat_wb2 <- dat_wb %>% 
  select(date, site_name, Yield_filled_mm_7) %>%
  spread(key = site_name, value = Yield_filled_mm_7) %>% 
  drop_na() %>%
  gather(key = site_name, value = Yield_filled_mm_7, 2:ncol(.))

dat_wb_clean <- dat_wb2 %>% 
  left_join(dat_wb %>% select(site_name, date, site_id, basin, subbasin, region, lat, long, elev_ft, area_sqmi, designation)) %>%
  group_by(site_name, site_id, basin, subbasin, region, lat, long, elev_ft, area_sqmi, designation) %>%
  mutate(yield_var = log(Yield_filled_mm_7) - lag(log(Yield_filled_mm_7))) %>%
  drop_na() %>% ungroup()

dat_wb_clean %>% ggplot() + geom_histogram(aes(x = Yield_filled_mm_7)) + facet_wrap(~site_name)

dat_wb_clean_sum <- dat_wb_clean %>% 
  group_by(site_name, site_id, basin, subbasin, region, lat, long, elev_ft, area_sqmi, designation) %>%
  summarize(yield_mean = mean(Yield_filled_mm_7),
            yield_min = min(Yield_filled_mm_7),
            yield_max = max(Yield_filled_mm_7),
            yield_cv = sd(Yield_filled_mm_7)/mean(Yield_filled_mm_7),
            yield_skew = skewness(Yield_filled_mm_7),
            yield_kurt = kurtosis(Yield_filled_mm_7),
            yield_ar1c = cor(Yield_filled_mm_7, lag(Yield_filled_mm_7), method = "pearson", use = "complete.obs")) %>%
  ungroup()

ggpairs(dat_wb_clean_sum[,c(11:17)])

jpeg("./Explore Data/Radial Plots/WB_radial_test.jpeg", units = "in", res = 500, width = 7, height = 7)
par(mfrow = c(4,3), mar = c(1,1,1,1))
for (i in 1:dim(dat_wb_clean_sum)[1]) {
  sum_minmax <- rbind(dat_wb_clean_sum %>% select(c(11:17)) %>% summarize_all(min),
                      dat_wb_clean_sum %>% select(c(11:17)) %>% summarize_all(max),
                      dat_wb_clean_sum[i, c(11:17)])
  radarchart(sum_minmax, title = dat_wb_clean_sum$site_name[i], 
             axistype = 1, pcol = rgb(0.2,0.5,0.5,0.9), pfcol = rgb(0.2,0.5,0.5,0.5), plwd = 4, cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.8, vlcex = 0.8)
}
dev.off()


sum_minmax <- t(tibble(sapply(sum_minmax, max), sapply(sum_minmax, min)))
row.names(sum_minmax) <- NULL
sum_minmax <- tibble(sum_minmax)
colnames(sum_minmax) <- names(dat_wb_clean_sum[,c(11:17)])

rbind(sum_minmax, dat_wb_clean_sum[1,c(11:17)])


# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 20 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)

# Check your data, it has to look like this!
# head(data)

# Custom the radarChart !
radarchart( data  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)



