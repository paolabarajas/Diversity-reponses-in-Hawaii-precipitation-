TODO
# 1. extract TEMP and Soil values per plot

NOTES
# climate data from http://climate.geography.hawaii.edu/downloads.html
# Geological data from https://pubs.usgs.gov/of/2007/1089/
# Digital elevation model from:  https://srtm.csi.cgiar.org/download
# Soil data from http://gis.ctahr.hawaii.edu/SoilAtlas#map

library(raster)
library(rgdal)
library(plyr)
library(sf)
library(dplyr)

rm(list=ls())

# DATA
datt <- read.csv("C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2_1.csv" ) # includes cookies info. saved 31.01.2022. The csv was edited in 0__Dylan_MakeAbundance_2sizeclasses.R

prj4string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
my.projection <- st_crs(prj4string)

points <- st_as_sf(datt, coords = c("Long_Dec", "Lat_Dec"), crs = my.projection)
st_crs(points)
plot(points)
class(points)

# st_write(points, "C:/Hawaii_project/additional_studies/datt.shp")

# Climatic, geological and elevation data
setwd("C:/Hawaii_project/Hawaii_Atlas_Data/")

temp_ras     <- raster("tair_ann.tif")    # Air Temperature: Temperature of the near-surface air
prec_ras     <- raster("staterf_mmann.tif") 
evapo_ras    <- raster("aet_mm_ann.tif")  # The rate of transfer of water from the surface to the atmosphere through evaporation of water from wet leaves, branches, and stems of plants
solrad_ras   <- raster("cl_sw_ann.tif")   # Incident solar radiation on a horizontal surface.
soil_fer_ras   <- raster("C:/Hawaii_project/Hawaii_Atlas_Data/Soil/SoilFertility.tif")
soil_Org_ras   <- raster("C:/Hawaii_project/Hawaii_Atlas_Data/Soil/SoilOrganic.tif")
geology  <- raster("C:/Hawaii_project/Hawaii_Atlas_Data/Haw_St_geo.tif")
# st_crs(geology)  

# Extracting values per point / plot

temp    <- extract(temp_ras, points)  
prec    <- extract(prec_ras, points)                                                             
evapo   <- extract(evapo_ras, points) 
solarad <- extract(solrad_ras, points) 
soil_fertility  <- extract(soil_fer_ras, points)
soil_OrgMatter  <- extract(soil_Org_ras, points)
geo             <- extract(geology, points)

tmp <- as.data.frame(cbind (temp, prec, evapo, solarad, soil_fertility, soil_OrgMatter, geo))

# Soil_fertility classes coding
# 1 Infertile Andisols
# 2 fertile Andisols
# 3 Other
# 4 Infertile
# 5 Fertile
# 6 organic soils
unique(tmp$soil_fertility)
tmp$soil_fertility <- gsub(1 , "Infertile", tmp$soil_fertility) 
tmp$soil_fertility <- gsub(2 , "fertile"  , tmp$soil_fertility) 
tmp$soil_fertility <- gsub(3 , NA       , tmp$soil_fertility) 
tmp$soil_fertility <- gsub(4 , "Infertile", tmp$soil_fertility) 
tmp$soil_fertility <- gsub(5 , "fertile"  , tmp$soil_fertility) 
tmp$soil_fertility <- gsub(6 , "organicsoil", tmp$soil_fertility) 

# Soil_OrgMatter classes coding
# 1 Moderate 
# 2 high 
# 3 Very high
# 4 Low
# 5 NA
unique(tmp$soil_OrgMatter)
tmp$soil_OrgMatter <- gsub(1 , "Moderate" , tmp$soil_OrgMatter) 
tmp$soil_OrgMatter <- gsub(2 , "high"     , tmp$soil_OrgMatter) 
tmp$soil_OrgMatter <- gsub(3 , "Very high", tmp$soil_OrgMatter) 
tmp$soil_OrgMatter <- gsub(4 , "Low" , tmp$soil_OrgMatter) 
tmp$soil_OrgMatter <- gsub(5 , NA    , tmp$soil_OrgMatter) 

# Geology classes coding
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

datt_env <- cbind(datt, tmp)
datt_env <- dplyr::summarize(group_by(datt_env, PlotID, geo_entity2, Study, soil_fertility, soil_OrgMatter),
                             MAT   = max(temp),
                             MAP   = max(prec),
                             EVAPO = max(evapo),
                             SOLR  = max(solarad),
                             GEO   = max(geo))  # plot level information 

unique(datt_env$GEO)
# Because zero O is a mix of geologies, I changed the class to the closest geology in the map using GIS. 
datt_env$GEO[datt_env$PlotID == 338] <- 13
datt_env$GEO[datt_env$PlotID == 342] <- 13
datt_env$GEO[datt_env$PlotID == 464] <- 13
datt_env$GEO[datt_env$PlotID == 539] <- 13
datt_env$GEO[datt_env$PlotID == 297] <- 13
datt_env$GEO[datt_env$PlotID == 222] <- 13
datt_env$GEO[datt_env$PlotID == 339] <- 14
datt_env$GEO[datt_env$PlotID == 341] <- 14
datt_env$GEO[datt_env$PlotID == 462] <- 14
datt_env$GEO[is.na(datt_env$GEO)] <- 13

sum(is.na(datt_env$GEO))

write.csv(datt_env, "C:/Hawaii_project/additional_studies/datt_env_11042020.csv", row.names = FALSE) # abiotic variables at plot level



#### New MAP data
# https://journals.ametsoc.org/view/journals/hydr/23/4/JHM-D-21-0171.1.xml
# high spatial resolution (~250m) gridded prediction of cumulative rainfall in millimeters from 01 Jan 1990 to 31 Jan 1990.

a_1990 <- list.files(path = "C:/Hawaii_project/Hawaii_Atlas_Data/HCDP_data/data/production/rainfall/new/month/statewide/data_map/1990",
                     pattern='.TIF$',  all.files=TRUE, full.names=FALSE)






























