# This code extracts environmental variables at the plot level
library(terra)
library(sf)
library(tidyverse)

# 
# library(rgdal)
# library(plyr)
# library(hrbrthemes)
# library(viridis)
# library(forcats)

#    setwd("C:/Hawaii_project/Hawaii/pub_code_data")    # Update here to you directory

# DATA
datt <- read.csv("data/HawIslandsAbundance_2SizeClasses_100plus_v2_2.csv" ) # includes cookies info. saved 31.01.2022. The csv was edited in 0__Dylan_MakeAbundance_2sizeclasses.R

tmp <- datt %>%
  group_by(PlotID) %>%
  summarise(Lat_Dec = max(Lat_Dec), Long_Dec= max (Long_Dec), MAT = max(MAT), MAP = max(MAP))

prj4string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no=defs"
my.projection <- st_crs(prj4string)

points <- st_as_sf(tmp, coords = c("Long_Dec", "Lat_Dec"), crs = my.projection)
st_crs(points)
plot(points)

points <- vect(points)
class(points)

# writeVector(points, "C:/Hawaii_project/additional_studies/plots_20240604.shp") # check in GIS the plots
# st_write(points, "C:/Hawaii_project/additional_studies/datt.shp") # check in GIS the data

# Climatic, geological and elevation data
# setwd("C:/Hawaii_project/Hawaii_Atlas_Data")
prec_ras       <- rast("staterf_mmann.tif") # Download climate data from http://climate.geography.hawaii.edu/downloads.html
temp_ras       <- rast("tair_ann.tif")      # Download climate data from http://climate.geography.hawaii.edu/downloads.html
geology        <- rast("Haw_St_geo.tif")    # Download Geological data from https://pubs.usgs.gov/of/2007/1089/
geology        <- terra::project(geology, prj4string)

crs(geology)
crs(prec_ras)
crs(temp_ras)
crs(points)

plot(geology)
plot(points, bg="transparent", add=TRUE)

terra::freq (geology)

# Extracting values per plot
prec            <- terra::extract(prec_ras, points)   
temp            <- terra::extract(temp_ras, points)  
geo             <- terra::extract(geology, points)

datt_env <- as.data.frame(cbind(points$PlotID, prec$staterf_mmann, temp$tair_ann, geo$Haw_St_geo)) # add PlotID to extracted values from raster
colnames(datt_env) <- c("PlotID", "staterf_mmann", "tair_ann", "Haw_St_geo")
datt_env$Haw_St_geo <- as.integer(datt_env$Haw_St_geo)

unique(datt_env$Haw_St_geo)

# Geology classes coding 
# https://pubs.usgs.gov/of/2007/1089/Hawaii_metadata_for_users.pdf  Sherrod et al 2007 
# https://www.soest.hawaii.edu/GG/FACULTY/KJOHNSON/GG103/HawaiiMap=expl=pamphlet.pdf
# 0 -- Sedimentary rocks and deposits that span several age ranges 
# 1 -- 0-200 yr          = 100      = 0.0001 Ma        Years ago, the so-called "historical lavas of some authors"
# 2 -- 200-750 yr        - 275      = 0.000275 Ma      equidistant/mean age value 
# 3 -- 750-1,500 yr      - 1125     = 0.00112 Ma
# 4 -- 1,500-3,000 yr    - 2250     = 0.0022 Ma
# 5 -- 3,000-5,000 yr    - 4000     = 0.004 Ma
# 6 -- 5,000-10,000 yr   - 7500     = 0.0075 Ma
# 7 -- 10-30 ka          - 20000    = 0.02 Ma
# 8 -- 30-50 ka          - 40000    = 0.04 Ma
# 9 -- 50-140 ka         - 95000    = 0.09 Ma
# 10 -- 140-780 ka       - 460000   = 0.46 Ma
# 11 -- 780-1,000 ka     - 890000   = 0.89 Ma
# 12 -- 1-2 Ma                       - 1.5 Ma
# 13 -- 2-4 Ma                       - 3 Ma
# 14 -- 4-6 Ma                       - 5 Ma

# Because zero O is a mix of geologies, I changed the class to the closest geology in the map using GIS. 
datt_env$Haw_St_geo[datt_env$PlotID == 222] <- 12
datt_env$Haw_St_geo[datt_env$PlotID == 297] <- 13
datt_env$Haw_St_geo[datt_env$PlotID == 339] <- 12
datt_env$Haw_St_geo[datt_env$PlotID == 341] <- 14
datt_env$Haw_St_geo[datt_env$PlotID == 342] <- 13
datt_env$Haw_St_geo[datt_env$PlotID == 462] <- 12
datt_env$Haw_St_geo[datt_env$PlotID == 464] <- 13
datt_env$Haw_St_geo[datt_env$PlotID == 539] <- 13
datt_env$Haw_St_geo[datt_env$PlotID == 461] <- 13

sort(unique(datt_env$Haw_St_geo))

write.csv(datt_env, file = "C:/Hawaii_project/Hawaii/pub=code=data/data/datt_env=20240604.csv")

# # # 
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

# Geology classes coding 
setwd ("C:/Hawaii_project/Hawaii/pub_code_data/data")
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

# Checking if plots on wet areas on Big Island with low diversity are located in young substrates
#  tax=q0  phy=q0   FD=q0
tax <- a %>%
  ggplot( aes(x=geology, y=tax=q0, fill=geology)) +
  geom=boxplot() +
  scale_fill_viridis(discrete = TRUE,  direction = -1, option = "A") +
  geom_jitter(color="grey", size=.5, alpha=0.5) + xlab("Geology")+
  theme_minimal()
phy <- a %>%
  ggplot( aes(x=geology, y=phy=q0, fill=geology)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,  direction = -1, option = "A") +
  geom_jitter(color="grey", size=.5, alpha=0.5) +
  theme_minimal()
fun <- a %>%
  ggplot( aes(x=geology, y=FD=q0, fill=geology)) +
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

# Supplementary Figure 9 
# Assembling Temperature and Geology into data frames to be added in the models. 

setwd ("C:/Hawaii_project/Hawaii/pub_code_data/data")

datt_geology <- read.csv("datt_geology_20230712.csv")
load("results_native_plot_20230601.RData")
load("results_all_plot_20230601.RData")

tmp <- datt_geology
tmp <- tmp %>%  
  mutate(Substrate_age_Ma = dplyr::recode(geology,
                                          "1"  = 0.0001  , "2"  = 0.000275 ,     
                                          "3"  = 0.00112 , "4"  = 0.0022   ,
                                          "5"  = 0.004   , "6"  = 0.0075   , 
                                          "7"  = 0.02 , "8"  = 0.04 ,
                                          "9"  = 0.09 , "10" = 0.46 ,
                                          "11" = 0.89 , "12" = 1.5  ,
                                          "13" = 3    , "14" = 5 )) 

a <- tmp %>%
  left_join(results_native_plot, by = "PlotID") %>%
  filter(!is.na(tax_q0)) %>%
  select(PlotID, geology, geo_entity2, Substrate_age_Ma, MAP, 
         tax_q0, tax_q1, tax_q2, 
         phy_q0, phy_q1, phy_q2, 
         FD_q0, FD_q1, FD_q2)

b <- tmp %>%
  left_join(results_all_plot, by = "PlotID") %>%
  filter(!is.na(tax_q0)) %>%
  select(PlotID, geology, geo_entity2,Substrate_age_Ma, MAP, 
         tax_q0, tax_q1, tax_q2, 
         phy_q0, phy_q1, phy_q2, 
         FD_q0, FD_q1, FD_q2)

# # 
coco = c(
  "#00AFBB", 
  "gray60", 
  "#E7B800")

s9_a <- ggplot(a) +   
  geom_boxplot(aes(x=Substrate_age_Ma, y=geo_entity2)) +
  geom_jitter(aes(x=Substrate_age_Ma, y=geo_entity2, group = geo_entity2, colour = geo_entity2), size = .8 , alpha = .6) +
  scale_colour_manual(values=coco) +
  theme_set(theme_minimal(base_size = 8))
s9_a

s9_b <- ggplot(b) +   
  geom_boxplot(aes(x=Substrate_age_Ma, y=geo_entity2)) +
  geom_jitter(aes(x=Substrate_age_Ma, y=geo_entity2, group = geo_entity2, colour = geo_entity2), size = .8 , alpha = .6) +
  scale_colour_manual(values=coco) +
  theme_set(theme_minimal(base_size = 8))
s9_b

# ggpubr::ggarrange(s9_a, s9_b)
# pdf(file = "C:/Hawaii_project/Figures/Suppl_Fig_S9_20240606.pdf",
#     width=8, # 89 mm (single column) is 3.5 inches.
#     height=2,
#     paper = "a4")
# ggpubr::ggarrange(s9_a, s9_b)
# dev.off()

ggplot(a, aes(x = Substrate_age_Ma, y = FD_q2)) +  
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = TRUE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3)+
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+
  theme_set(theme_minimal(base_size = 8))




