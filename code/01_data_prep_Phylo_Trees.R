# R.version

library(TNRS)
library(V.PhyloMaker)
library(tidyverse)
library(dplyr)
library(phylobase)
library(ape)
library(reshape)
library(stringr)

# NEW Edited DATA 21 March 2022
datt <-read.csv("C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2_1.csv" ) # includes cookies info. saved 31.01.2022. The csv was edited in 0__Dylan_MakeAbundance_2sizeclasses.R
datt$PlotID <- as.factor(datt$PlotID) # 2272. added new data Chase & Knight
datt$Clust_manu <- datt$Clust_manu %>% replace_na('anone')

length(unique(datt$PlotID)) # 552 total amount of plots OpenNahele ("hyppnet" were removed)
length((unique(datt$Family))) # 56
length(unique(datt$Scientific_name)) # 181 species

#------------ Adding new standard names 2022
datt  <- filter(datt, Scientific_name != "unknown_ilexlike")
names <- unique(datt$Scientific_name) # need to turn the df into a character string before the next step
resolution     <- TNRS(names, min_score = 0.8, source = "wcvp")
accepted_names <- resolution[, c("Name_submitted","Accepted_name", "Accepted_family")] # standard names and family

names(accepted_names)[1] <- "Scientific_name"
accepted_names$Accepted_name[accepted_names$Scientific_name == "Schefflera actinophylla"] <- "Schefflera actinophylla" #
accepted_names$Accepted_family[accepted_names$Scientific_name == "Schefflera actinophylla"] <- "Araliaceae"
accepted_names$Accepted_name[accepted_names$Scientific_name == "Heptapleurum actinophyllum"] <- "Heptapleurum actinophyllum" #
accepted_names$Accepted_name[accepted_names$Scientific_name == "Nototrichium sandwicense"] <- "Nototrichium sandwicense" #

accepted_names[143,1]
accepted_names[143,2] <- "Schizostachyum glaucifolium"
accepted_names[143,3] <- "Poaceae"

anyNA(accepted_names)

names(accepted_names)[2] <- "Accepted_name_TNRS2022"
names(accepted_names)[3] <- "Accepted_family_TNRS2022"

datt1 <- left_join(datt, accepted_names, by = "Scientific_name")

length(unique(datt1$Scientific_name))
length(unique(datt1$Accepted_name_TNRS))

# write.csv(datt1, file = "C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2_2.csv" ,  row.names = FALSE) # saved 26.10.2022

# setwd("H:/Manuscripts/Hawaii_project/additional_studies")
setwd("C:/Hawaii_project/additional_studies")

datt <-read.csv("HawIslandsAbundance_2SizeClasses_100plus_v2_2.csv" ) # saved 01.11.2022

# Cleaning data for analysis
datt <- filter(datt, Study != "Hatheway_1952") # removing "Hatheway_1952"  2219
datt <- filter(datt, Family != "Cibotiaceae" & Family != "Blechnaceae" & Family != "Cyatheaceae") # 1860
datt <- filter(datt, Scientific_name != "unknown_ilexlike") 
datt <- filter(datt, geo_entity2 != "Kaua'i Island")
datt <- filter(datt, Scientific_name != "Melicope kaalaensis" & Scientific_name != "Psychotria kaduana") # 2 natives in highly invaded plots, lost in native analysis. 

datt <- datt %>% separate(Clust_manu, c("Map_class"), remove = FALSE)
datt$Map_class <- paste(datt$Map_class, "", datt$PlotID)

# # # 
### Improving # individual per plot. merge some plots in Oahu and Maui nui
datt$PlotID[which(datt$PlotID == "555")] <- 554 # Oahu
datt$PlotID[which(datt$PlotID == "556")] <- 553
datt$PlotID[which(datt$PlotID == "568")] <- 567
datt$PlotID[which(datt$PlotID == "565")] <- 564
datt$PlotID[which(datt$PlotID == "314")] <- 300 # find 314 convert it into 300

datt$PlotID[which(datt$PlotID == "388")] <- 349 # Mau Nui
datt$PlotID[which(datt$PlotID == "19")]  <- 365
datt$PlotID[which(datt$PlotID == "364")] <- 348
datt$PlotID[which(datt$PlotID == "407")] <- 348

tmp2 = filter(datt,
              PlotID == "554" | PlotID == "553" | PlotID == "567" | PlotID == "564" |
                PlotID == "300" | PlotID == "349" | PlotID == "365" | PlotID == "348" )

tmp3 = filter(tmp2,
              PlotID == "554" | PlotID == "553" | PlotID == "567" | PlotID == "564" |
                PlotID == "300" | PlotID == "349" | PlotID == "365" | PlotID == "348"  ) %>%
  
  mutate(MAP = ifelse(PlotID == "554", 919.4246, MAP))%>%
  mutate(Map_class = ifelse(PlotID == "554", "dry  554", Map_class)) %>%
  
  mutate(MAP = ifelse(PlotID == "553",  921.1044, MAP))%>%
  mutate(Map_class = ifelse(PlotID == "553", "dry  553", Map_class)) %>%
  
  mutate(MAP = ifelse(PlotID == "567",  3586.6475, MAP))%>%
  mutate(Map_class = ifelse(PlotID == "567", "wet  567", Map_class)) %>%
  
  mutate(MAP = ifelse(PlotID == "564",  3892.3423, MAP))%>%
  mutate(Map_class = ifelse(PlotID == "564", "wet  564 ", Map_class)) %>%
  
  mutate(MAP = ifelse(PlotID == "300",  3271.8630, MAP))%>%
  mutate(Map_class = ifelse(PlotID == "300", "wet  300", Map_class)) %>%
  
  mutate(MAP = ifelse(PlotID == "349",  7492.5718, MAP))%>%
  mutate(Map_class = ifelse(PlotID == "349", "superwet  349", Map_class)) %>%
  
  mutate(MAP = ifelse(PlotID == "365",  3851.2681, MAP))%>%
  mutate(Map_class = ifelse(PlotID == "365", "wet  365", Map_class)) %>%
  
  mutate(MAP = ifelse(PlotID == "348",  5277.5991, MAP))%>%
  mutate(Map_class = ifelse(PlotID == "348", "superwet  348", Map_class))

datt_tmp = filter(datt, PlotID != "554" & PlotID != "553" & PlotID != "567" & PlotID != "564" &
                       PlotID != "300" & PlotID != "349" & PlotID != "365" & PlotID != "348" )

datt <- rbind(datt_tmp, tmp3)

# # # 

length(unique(datt$Scientific_name)) # 142 species  

###--- NATIVE PATTERNS
datt_40  <- filter(datt, Plot_Prop_Invaded < 0.41) # 1305

spp_40 <- dplyr::summarize(dplyr::group_by(datt_40, Scientific_name, Family))
spp_40 <- spp_40 %>% separate(Scientific_name, c("genus","species"), sep = " ", remove = FALSE)
spp_40 <- data.frame (species = spp_40$Scientific_name, genus = spp_40$genus, family = spp_40$Family)

datt_nat <- filter(datt_40, Native_Status == "native") # 1092

spp_nat <- dplyr::summarize(dplyr::group_by(datt_nat, Scientific_name, Family))
spp_nat <- spp_nat %>% separate(Scientific_name, c("genus","species"), sep = " ", remove = FALSE)
spp_nat <- data.frame (species = spp_nat$Scientific_name, genus = spp_nat$genus, family = spp_nat$Family)

###--- PATTERNS Native + aliens
spp_all <- dplyr::summarize(dplyr::group_by(datt, Scientific_name, Family))
spp_all <- spp_all %>% separate(Scientific_name, c("genus","species"), sep = " ", remove = FALSE)
spp_all <- data.frame (species = spp_all$Scientific_name, genus = spp_all$genus, family = spp_all$Family)
spp_all[129, 2] <- "Schizostachyum" # for some reason I have to do this. 

###--- BUILDING THE TREES

hawaii.tree_native <- phylo.maker(spp_nat, scenarios="S2") # 79 native species. With new added species from Chase & Knight study
# plot.phylo(hawaii.tree_native$scenario.1, cex = .4, main = "Hawaiian Islands native")

hawaii.tree_40 <- phylo.maker(spp_nat, scenarios="S2") # 118 native species. With new added species from Chase & Knight study

hawaii.tree_all <- phylo.maker(spp_all, scenarios="S2") # 146 native species. With new added species from Chase & Knight study

save (hawaii.tree_native, hawaii.tree_40, hawaii.tree_all,
      file = "C:/Hawaii_project/Hawaii/data_phylo_trees_20230419.RData") # 

##
length(unique(datt$Scientific_name)) # 144 species  
length(unique(datt_40$Scientific_name)) # 118 species  
length(unique(datt_nat$Scientific_name)) # 79 species  

datt
datt_40
datt_nat

save (datt, datt_40, datt_nat,
      file = "C:/Hawaii_project/Hawaii/datt_20230419.RData")


#####################
# Plot Supplementary Figure 2
load("C:/Hawaii_project/Hawaii/data_phylo_trees_20230419.RData")

load("C:/Hawaii_project/Hawaii/datt_20221101.RData")
tmp <- dplyr::summarize(group_by(datt, Scientific_name, Native_Status))
tmp$Scientific_name <- sub(" ", "_", tmp$Scientific_name)

tree <- hawaii.tree_all$scenario.2
tree <- as(tree$run.1, "phylo")
class(tree)
length(tree$tip.label)

##
# converts abundance matrix into the order of species in the tree
t    <- as.data.frame(tree$tip.label)  
setdiff( t$`tree$tip.label` , tmp$Scientific_name) # should be zero
colnames(t) <- c('Scientific_name')

t <- left_join(t, tmp,  by = "Scientific_name")

##
flortistic <- factor(t$Native_Status)
levels(flortistic)
(mycol <- c("violetred3", "gray32")[flortistic])

#
pdf(file = "C:/Hawaii_project/Figures/Sup_Fig_2_phylotree_all.pdf",   # Figure_1_native   Figure_3_native_alien
    width=8, # 89 mm (single column) is 3.5 inches.
    height=12,
    paper = "a4")

plot(tree, tip.color = mycol, type= "fan", cex=.7)

dev.off()



