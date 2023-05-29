# This is how I arranged HawIslandsAbundance_2SizeClasses_100plus.csv to "rescue" species without epithet. 
# this short code is also part of 0_data_prep_Phylo_Trees.R. 

rm(list = ls())

# 1. DATA
datt     <- read.csv( # Dylan summarized OpenNahele per species summing abundances.
  'C:/Hawaii_project/Hawaii_diversity-master/Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv', header=T)

cookies_clustmanu <- rgdal::readOGR(dsn="C:/Hawaii_project/shapefiles",layer="cookies_04_ClustManu")   # Plots on cookies were already filtered 40 % aliens
cookies           <- data.frame(cookies_clustmanu@data)
cookies           <- dplyr::select(cookies, PlotID, Clust_manu)
length(unique(cookies$Clust_manu)) # 59 cookies in total
length(unique(cookies$PlotID))     # 393 plots in total
rm(cookies_clustmanu)

datt        <- plyr::join(datt, cookies, by = "PlotID") # Adding cookies to the data

datt_40      <- na.omit(datt)   # Discarding plot with  more than 40 % aliens. I use datt because it was joint with cookies which were filtered already (for max 40%)
datt_nat     <- dplyr::filter(datt_40, Native_Status == "native")  # filtering for only natives

spp          <- dplyr::summarize(dplyr::group_by(datt, Scientific_name, Native_Status), Abundance=sum(Abundance))
spp$Genus    <- gsub(" .+$", "", spp$Scientific_name)
spp$Species  <- gsub("^.+ ", "", spp$Scientific_name)
spp$Abundance<- NULL
lookup = dplyr::filter(spp, Species == "spp.") # 15 spp have spp. in all OpenNahele as epithet.

spp          <- dplyr::summarize(dplyr::group_by(datt_nat, Scientific_name, Native_Status), Abundance=sum(Abundance))
spp$Genus    <- gsub(" .+$", "", spp$Scientific_name)
spp$Species  <- gsub("^.+ ", "", spp$Scientific_name)
spp$Abundance<- NULL
lookup = dplyr::filter(spp, Species == "spp.") # 10 spp NATIVE have don't have epithet.

# write.csv(datt_nat, file = "C:/Hawaii_project/New_code/datt_nat_recue.csv")


####################################################################################

# In that csv file I edited two things of OpenNahele database
# 1. I edited  datt_nat  to rescue information, i.e, NATIVE species epithet. I added a new column with old names, so I can track what names did I change
# 2. I added hathaway study to it. It includes two alien species.

datt_nat_rescued = read.csv("C:/Hawaii_project/New_code/datt_nat_rescue_spp29112021.csv") # 1442 observations
datt_nat_rescued = filter(datt_nat_rescued, Native_Status == "native") # 1435 native only, as I am working only with native species for now.


