
# This code I characterize each species to a precipitation range where they exist
# 
tabla <- read.csv("C:/Hawaii_project/shapefiles/species_maps/Species_shapes_appendix-table.csv")
# names(tabla)[1] <- paste("Scientific_name")

length(unique(tabla$NAME)) # 1231

names <- tabla$NAME # 1231 species

resolution <- TNRS::TNRS(names, min_score = 0.8, source = "wcvp")
accepted_names <- resolution[, c("Name_submitted","Accepted_name")]

tabla$Scientific_name <- accepted_names$Accepted_name
# tabla$Name_submitted <- accepted_names$Name_submitted

load("C:/Hawaii_project/Hawaii/data_datt_nat_40_20220913.RData") 

datt_40 = datt_40_new

length(unique(datt_40$Scientific_name))  # 118
length(unique(datt_nat$Scientific_name)) # 79

# 49 species in datt_40 that are not in the shapefiles
# length(setdiff(unique(datt_40$Scientific_name), unique(tabla$Scientific_name))) 
# # 12 species in datt_nat that are not in the shapefiles
# length(setdiff(unique(datt_nat$Scientific_name), unique(tabla$NAME))) 

# Mask Tabla with species in datt_40
species <- as.data.frame(unique(datt_nat$Scientific_name))
names(species)[1] <- paste ("Scientific_name")

z <- dplyr::left_join(species, tabla, by = "Scientific_name" , keep = TRUE)
z$Scientific_name.x # are the species from datt_nat, that I need to download

write.csv(z, file = "C:/Hawaii_project/shapefiles/species_maps/Species_shapes_dowload.csv")

###############################################################################

tmp <- read.csv("C:/Hawaii_project/shapefiles/species_maps/Species_shapes_dowload.csv")
names (tmp)
tmp <- tmp [, c( "Scientific_name.x" ,"NAME", "DOWNLOAD.GIS.FILES..shapefiles..in.zip.format.", "Scientific_name.y" )]

names(tmp)[1] <- paste("Scientific_name") # Scientific_name from datt_nat
names(tmp)[2]                             # Names from the shapefile 
names(tmp)[4] <- paste("NAME_standardized") # names  from the from the shapefile after TNRS standarization

tmp <- na.omit(tmp)

###############################################################################

library(maptools)
library(sp)
library(sf)
library(rgdal)
require(raster)
require(rgdal)
require(plyr)

# Load environmental heterogeneity components

setwd('H:/Manuscripts/Hawaii_project/shapefiles/species_maps') 

acakoa   <- readOGR("acakoa.shp")   
acakoai  <- readOGR("acakoai.shp")    
antplat  <- readOGR("antplat.shp")    
antpulv  <- readOGR("antpulv.shp")    
bobsand  <- readOGR("bobsand.shp")

chaovat <- readOGR("chaovat.shp")
cheforb <- readOGR("cheforb.shp")
cheoahu <- readOGR("cheoahu.shp")
chetrig <- readOGR("chetrig.shp")
clearbo <- readOGR("clearbo.shp")
clekake <- readOGR("clekake.shp")
cleparv <- readOGR("cleparv.shp")
coloppo <- readOGR("coloppo.shp")
copfoli <- readOGR("copfoli.shp")
coplong <- readOGR("coplong.shp")
copochr <- readOGR("copochr.shp")
coprhyn <- readOGR("coprhyn.shp")
cyaangu <- readOGR("cyaangu.shp")

diosand <- readOGR("diosand.shp")
dodvisc <- readOGR("dodvisc.shp")
dubplan <- readOGR("dubplan.shp")
erysand <- readOGR("erysand.shp")
frearbo <- readOGR("frearbo.shp")
hibarno <- readOGR("hibarno.shp")
ileanom <- readOGR("ileanom.shp")
kadaffi <- readOGR("kadaffi.shp")
kadaxil <- readOGR("kadaxil.shp")
leptame <- readOGR("leptame.shp")

melclus <- readOGR("melclus.shp")
melhale <- readOGR("melhale.shp")
melvolc <- readOGR("melvolc.shp")
metpoly <- readOGR("metpoly.shp")
mettrem <- readOGR("mettrem.shp")
myosand <- readOGR("myosand.shp")
myremar <- readOGR("myremar.shp")
myrlana <- readOGR("myrlana.shp")
myrless <- readOGR("myrless.shp")
myrsand <- readOGR("myrsand.shp")
nessand <- readOGR("nessand.shp")

ostanth <- readOGR("ostanth.shp")
pantect <- readOGR("pantect.shp")
persand <- readOGR("persand.shp")
pipalbi <- readOGR("pipalbi.shp")
pitglab <- readOGR("pitglab.shp")
pithosm <- readOGR("pithosm.shp")
pleauwa <- readOGR("pleauwa.shp")
plehawa <- readOGR("plehawa.shp")
pousand <- readOGR("pousand.shp")
pribecc <- readOGR("pribecc.shp")
primunr <- readOGR("primunr.shp")
psyhawa <- readOGR("psyhawa.shp")
psyhexa <- readOGR("psyhexa.shp")
psymari <- readOGR("psymari.shp")

rausand <- readOGR("rausand.shp")
reysand <- readOGR("reysand.shp")
rubmacr <- readOGR("rubmacr.shp")
sanfrey <- readOGR("sanfrey.shp")
sanpani <- readOGR("sanpani.shp")
sapoahu <- readOGR("sapoahu.shp")
scagaud1 <- readOGR("scagaud1.shp")
sopchry <- readOGR("sopchry.shp")
syzsand <- readOGR("syzsand.shp")

tethawa <- readOGR("tethawa.shp")
tetoahu <- readOGR("tetoahu.shp")
toulati <- readOGR("toulati.shp")
vacdent <- readOGR("vacdent.shp")
vacreti <- readOGR("vacreti.shp")
wikmont <- readOGR("wikmont.shp")
wikoahu <- readOGR("wikoahu.shp")
wiksand <- readOGR("wiksand.shp")

MAP_raster <- raster("C:/Hawaii_project/Hawaii_Atlas_Data/staterf_mmann.tif") 

# get all files with the .shp extension from working directory 
setwd("H:/Manuscripts/Hawaii_project/shapefiles/species_maps")
shps <- dir(getwd(), "*.shp")

# the assign function will take the string representing shp and turn it into a variable
# which holds the spatial points data
for (shp in shps) assign(shp, rgdal::readOGR(shp))
plot(get(shp[1])) # i.e.
# ...done


###############################################################################


species_map_stats <- read.csv("C:/Hawaii_project/shapefiles/species_maps/MAP_stats/species_MAP_stats.csv")

























