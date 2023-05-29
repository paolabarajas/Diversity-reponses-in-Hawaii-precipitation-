rm(list = ls())

require(dplyr)
require(mobr)
require(ggplot2)
library(sp)
library(rgdal)
library(sf)
library(rgeos)
require(maptools)

# OpenNahele data is at tree level
# datt is at species level. Dylan et al summarized OpenNahele per species summing abundances. 

setwd("C:/Hawaii_project/Hawaii_diversity-master") #  Complete data set

datt <- read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv", header=T)

unique(datt$Island)
datt$Island[datt$Island == "Lana'i Island"]                        <- "Maui Island"
datt$Island[datt$Island == "Moloka'i Island"]                      <- "Maui Island"
datt$Island[datt$Island == "O'ahu Island (incl. Mokoli'i Islet)"]  <- "O'ahu Island"
unique(datt$Island)

length(unique(datt$SPP_CODE3A))  # 166 species


# The approach: 
# 1. split the plots into areas with similar precipitation regimes - Data binning 
# 2. select plots are spatially clustered  - clustering analysis 
# 3. Plots were edited in GIS to conform the 'cookies'- plots aggregation 

plots_hawaii        <-  datt[,1:10] 
plots_hawaii$PlotID <- as.factor(plots_hawaii$PlotID)

plots_hawaii <- aggregate(. ~ PlotID, data = plots_hawaii, max) 
plots_hawaii <- plots_hawaii %>% mutate_at(vars(Plot_Area,Lat_Dec, Long_Dec, Elev_m, MAT, MAP),
                                           ~ as.numeric(as.character(.))) # changes to numeric the variables I selected and that were characters. 

binned       <- mltools::bin_data(plots_hawaii$MAP,
                                   returnDT = T,
                                   bins=c(361,         # < 361        Dry
                                          1300,        # < 1300       Mesic
                                          2000,        # 1000 - 2000  wet
                                          5000, 9815)) # > 5000       Super Wet

unique(binned$Bin)
t0 <- binned[order(binned$Bin, binned$BinVal),]
t1 <- plots_hawaii[order(plots_hawaii$MAP, plots_hawaii$PlotID),]
#    t1$MAP[410:420]   # I checked here if the values are really in same order to join them
#    t0$BinVal[410:420]
t0$ID <- row.names(t0)
t1$ID <- 1:nrow(t1)

plots_hawaii_bin  <- plyr::join(t0, t1, by = "ID") # joined plots with binned data
plots_hawaii_bin %>% group_by(Bin, Island) %>%  count(Bin)  # Check how many plots I have per bin per area.

plots_hawaii_bin1 <- plots_hawaii_bin[,c("Bin","PlotID" )]
datt0             <- plyr::join(plots_hawaii_bin1, datt, by = "PlotID") # add  bin to datt

#___________________________________________________________   Exporting Plots into a shapefile

coordinates(plots_hawaii_bin) <- c("Long_Dec", "Lat_Dec") # Hawaii UTM zones 4 and 5 (big Hawaii island is in zone 5 the rest islands are in 4)
class(plots_hawaii_bin)
is.projected(plots_hawaii_bin) # check if a projection is defined 
proj4string(plots_hawaii_bin) <- CRS("+proj=utm +zone=4 +datum=WGS84 +units=km") # defining projection
is.projected(plots_hawaii_bin)

# writeOGR(OpenNahele_plots, "shapefiles/OpenNahele_plots",      # Save shapefile using writeOGR from rgdal
#          "OpenNahele_plots", driver = "ESRI Shapefile")

# NOTES
# From this point I exported the hawaian plots shapefile into ArcGIS and I manually checked the clusters (aka cookies)
# I created a shapefile with circles of 5 km radius, so I can control the area of the cookies
# cookies_04_ClustManu shapefile is the one I edited in ArcGIS
# Manual_cookies.shp shapefile contains the 5 km radius cluster (cookies)

cookies_clustmanu <- readOGR(dsn="C:/Hawaii_project/shapefiles",layer="cookies_04_ClustManu")   # LOAD plot with ID of cookies. Plot were already filtered with max. 40 % aliens
Manual_cookies <- readOGR(dsn="C:/Hawaii_project/shapefiles",layer="Manual_cookies")




