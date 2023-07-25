# This code extracts environmental variables at the plot level
library(raster)
library(rgdal)
library(plyr)
library(sf)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

setwd("C:/Hawaii_project/Hawaii/pub_code_data/data")

# DATA
datt <- read.csv("HawIslandsAbundance_2SizeClasses_100plus_v2_2.csv" ) # includes cookies info. saved 31.01.2022. The csv was edited in 0__Dylan_MakeAbundance_2sizeclasses.R

prj4string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
my.projection <- st_crs(prj4string)

points <- st_as_sf(datt, coords = c("Long_Dec", "Lat_Dec"), crs = my.projection)
st_crs(points)
plot(points)
class(points)

# st_write(points, "C:/Hawaii_project/additional_studies/datt.shp")

# Climatic, geological and elevation data
setwd("C:/Hawaii_project/Hawaii_Atlas_Data/")
prec_ras       <- raster("staterf_mmann.tif") # climate data from http://climate.geography.hawaii.edu/downloads.html
geology        <- raster("Haw_St_geo.tif") # Geological data from https://pubs.usgs.gov/of/2007/1089/

crs(geology)
crs(prec_ras)
crs(points)

# Extracting values per plot
prec            <- extract(prec_ras, points)                                                             
geo             <- extract(geology, points)

datt_geology <- as.data.frame(cbind(points$PlotID, geo))

# Geology classes coding  https://pubs.usgs.gov/of/2007/1089/Hawaii_metadata_for_users.pdf   Sherrod et al 2007 
# 0 -- Sedimentary rocks and deposits that span several age ranges 
# 1 -- 0-200 yr ago, the so-called "historical lavas of some authors
# 2 -- 200-750 yr
# 3 -- 750-1,500 yr
# 4 -- 1,500-3,000 yr
# 5 -- 3,000-5,000 yr
# 6 -- 5,000-10,000 yr
# 7 -- 10-30 ka
# 8 -- 30-50 ka
# 9 -- 50-140 ka
# 10 -- 140-780 ka
# 11 -- 780-1,000 ka
# 12 -- 1-2 Ma
# 13 -- 2-4 Ma
# 14 -- 4-6 Ma

datt_geology <- dplyr::summarize(group_by(datt_geology, V1), geology   = max(geo))  # plot level information 
colnames(datt_geology)[1] <- "PlotID"

datt_geology$geology
# Because zero O is a mix of geologies, I changed the class to the closest geology in the map using GIS. 
datt_geology$geology[datt_geology$PlotID == 338] <- 13
datt_geology$geology[datt_geology$PlotID == 342] <- 13
datt_geology$geology[datt_geology$PlotID == 464] <- 13
datt_geology$geology[datt_geology$PlotID == 539] <- 13
datt_geology$geology[datt_geology$PlotID == 297] <- 13
datt_geology$geology[datt_geology$PlotID == 222] <- 13
datt_geology$geology[datt_geology$PlotID == 339] <- 14
datt_geology$geology[datt_geology$PlotID == 341] <- 14
datt_geology$geology[datt_geology$PlotID == 462] <- 14
datt_geology$geology[is.na(datt_geology$geology)] <- 13

sum(is.na(datt_geology$geology))

datt_geology <- read.csv("datt_geology_20230712.csv")

##########################
# Geology classes coding 
datt_geology <- read.csv("datt_geology_20230712.csv")
# 
load("results_native_plot_20230601.RData")
load("results_all_plot_20230601.RData")  

a <- dplyr::left_join(results_all_plot, datt_geology, by = "PlotID")
a <- dplyr::filter(a, geo_entity2 == "Hawai'i Island" & MAP > 5000)
a$geology <- as.factor(a$geology)

a <- a %>%  
  mutate(geology = fct_recode(geology,
                              "2" = "1",   # 0     - 750    yr
                              "4" = "3",   # 750   - 3,000  yr
                              "6" = "5",   # 3,000 - 10,000 yr
                              "8" = "7",   # 10,000- 50  ka
                              "10" = "9")) # 50    - 780 ka


a %>% count(geology)

# I want to see if plots on wet areas on Big Island with low diversity are located in young substrates
#  tax_q0  phy_q0   FD_q0
tax <- a %>%
  ggplot( aes(x=geology, y=tax_q0, fill=geology)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,  direction = -1, option = "A") +
  geom_jitter(color="grey", size=.5, alpha=0.5) + xlab("Geology")+
  theme_minimal()
phy <- a %>%
  ggplot( aes(x=geology, y=phy_q0, fill=geology)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,  direction = -1, option = "A") +
  geom_jitter(color="grey", size=.5, alpha=0.5) +
  theme_minimal()
fun <- a %>%
  ggplot( aes(x=geology, y=FD_q0, fill=geology)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,  direction = -1, option = "A") +
  geom_jitter(color="grey", size=.5, alpha=0.5) +
  theme_minimal()

ggpubr::ggarrange(tax, phy, fun, ncol = 3)

pdf(file = "C:/Hawaii_project/Figures/Sup_Fig_8_subtrateage_20230719.pdf",   # 
    width=12, # 89 mm (single column) is 3.5 inches.
    height=2,
    paper = "a4")
ggpubr::ggarrange(tax, phy, fun, ncol = 3)

dev.off()

