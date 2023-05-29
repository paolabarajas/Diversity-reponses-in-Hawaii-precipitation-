
# The code comes from Dylan Craven et al 2018. PNAS  OpenNahele data arrangement 
# It produces HawIslandsAbundance_2SizeClasses_100plus.csv. The csv file contains
# 1. Standardized spp names and cleaning of species with uncertain floristic status (i.e., naturalized status was uncertain)
# 2. climatic data from Atlas of Hawaii, Giambelluca et al 2013
# 3. Filtered huge plots, Mosly from "HIPPNET" study


######################################################
# convert tree-level OpenNahele data to abundance data   
######################################################
######################################################

len_un <- function (x){length(unique(x))}

replace_na_with_last<-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

require(dplyr)
require(tidyr)
require(stringr)
require(reshape2)

########################
# load data ############
########################

setwd("C:/Hawaii_project/Hawaii_diversity-master")

spp<-read.delim("Data/Hawaii_fullSPP_clean.csv",sep=",",header=T)
spp<-select(spp, Scientific_name=Accepted_name_species, SPP_CODE3A)
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp1", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp2", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp3", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp4", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp5", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp6", "spp.")

spp<-unique(spp)

alienz<-read.csv("Data/Hawaii_aliens.csv")
nat_alienz<-filter(alienz, Naturalized_Introduced=="Not_Naturalized")
nat_alienz$Naturalized_Introduced<-as.character(nat_alienz$Naturalized_Introduced)
nat_alienzz<-as.character(nat_alienz$SPP_CODE3A)

tog<-read.delim("Data/OpenNahele_Tree_Data.csv",sep=",",header=T)
togg<-select(tog, Island, Study, PlotID, Plot_Area,Scientific_name, Native_Status, Tree_ID, DBH_cm,Abundance, Abundance_ha)

togg$geo_entity2<-togg$Island
togg$geo_entity2<-as.character(togg$geo_entity2)
togg$geo_entity2<-ifelse(togg$geo_entity2=="Maui Island"|togg$geo_entity2=="Lana'i Island"|togg$geo_entity2=="Moloka'i Island","Maui Nui",togg$geo_entity2)

togg$geo_entity2<-ifelse(togg$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu Island",togg$geo_entity2)

togg$Scientific_name<-as.character(togg$Scientific_name)

togg<-left_join(togg, spp, by="Scientific_name")

togg<-select(togg, PlotID, Study, Island, geo_entity2, Plot_Area, Tree_ID, Scientific_name, SPP_CODE3A, Native_Status, DBH_cm, Abundance,  Abundance_ha)

##############################
# Part I: Filter data        #
##############################

# Criteria 1: plot size > 100 m2 & eliminate HIPPNET (too big) 

togg1<- filter(togg, Plot_Area>=100)%>%
        filter(., Study!="HIPPNET") 

# Criteria 2: remove spp. that are not naturalized or uncertain 

togg1<- filter(togg1, !SPP_CODE3A %in% nat_alienzz)%>%
        filter(., Native_Status=="native"|Native_Status=="alien")

#########
## qc ###
#########

min(togg1$DBH_cm,na.rm=T) # 5

max(togg1$DBH_cm,na.rm=T) # 214

#################
# trees > 5 cm  #
#################

all<-dplyr::summarize(group_by(togg, Study, Island, geo_entity2, PlotID,Plot_Area, Scientific_name,SPP_CODE3A, Native_Status), Abundance=sum(Abundance),
                Abundance_ha=sum(Abundance_ha))

all$Abundance_ha<-round(all$Abundance_ha)

all$SizeClass<-"5"

#########################################
# Part II: Identify highly invaded plots  #
#########################################

tog_tog<- dcast(all, PlotID +SizeClass~Native_Status,value.var="Abundance_ha",sum)
tog_tog<-tog_tog %>%
  mutate(Tot_Abund= alien+native) 

tog_tog<-tog_tog %>%
  mutate(PropInvaded= alien/Tot_Abund) 

tog_tog<-select(tog_tog, PlotID, SizeClass,PropInvaded)

all_bigg<-merge(all, tog_tog,by.y=c("PlotID", "SizeClass"))

########################
# add in climate data ##
########################

env<-read.csv("Data/Hawaiian_Env_Soil_1km_v2.csv",sep=",",header=T)
colnames(env)[1]<-"PlotID"

all_bigg<-left_join(all_bigg, env,by.y=c("PlotID"))

########################
# arrange ##############
########################

all_bigg2<-select(all_bigg, PlotID, Study, Island, geo_entity2, Plot_Area, Lat_Dec, Long_Dec, Elev_m,MAT, MAP, PET,AridInd,
                  SubstrateAge_range, SubstrateAge_code, HFP, HII, Plot_Prop_Invaded=PropInvaded, SizeClass, Scientific_name, SPP_CODE3A, Native_Status, Abundance, Abundance_ha)

all_bigg2<-arrange(all_bigg2, PlotID, SizeClass)

# write.table(all_bigg2,"Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",sep=",",row.names=F)


################################################################################################

# PAOLA BARAJAS EDITED OPENAHELE DATA (HawIslandsAbundance_2SizeClasses_100plus)
# last edits 23.12.2021: two major edits
# 1. I included Hathaway study (in Oahu) 
# 2. I manually changed certain epithet names. I assigned to species with spp, 
  # based on distribution maps of the species and floristic status of the species. 

#################################################################################################

# This is how I arranged HawIslandsAbundance_2SizeClasses_100plus.csv to "rescue" species without epithet. 
# this short code is also part of 0_data_prep_Phylo_Trees.R. 

# New edits (from 23.12.2021) are in HawIslandsAbundance_2SizeClasses_100plus_v2.csv
datt = read.csv("C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2.csv",sep="," ) 
# datt$PlotID = as.factor(datt$PlotID) # 2160

unique(datt$Study)

length(unique(datt$PlotID))     # 524 plots in total

cookies_clustmanu <- rgdal::readOGR(dsn="C:/Hawaii_project/shapefiles",layer="cookies_04_ClustManu")   # Plots on cookies were already filtered. So plots with > 40 % aliens were removed, i.e., highly invaded plots
cookies           <- data.frame(cookies_clustmanu@data)
cookies           <- dplyr::select(cookies, PlotID, Clust_manu)
length(unique(cookies$Clust_manu)) # 59 cookies in total
length(unique(cookies$PlotID))     # 393 plots in total
rm(cookies_clustmanu)

datt        <- plyr::join(datt, cookies, by = "PlotID") # Adding cookies to the data
datt$X = NULL

write.csv(datt, "C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2_1.csv" , row.names = FALSE ) # includes cookies info. saved 31.01.2022
















