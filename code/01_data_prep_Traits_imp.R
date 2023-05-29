# R.version

library(tidyr)
library(visdat)
library(dplyr)
library(tidyverse)
library(reshape2)
library(missForest)
library(adephylo)
library(phytools)
library(ape)
library(pez)

# 552 total amount of plots and 181 species in OpenNahele 
# 2272 observations added new data Chase & Knight


# ----
# datt <- read.csv("C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2_1.csv" ) # includes cookies info. saved 31.01.2022. The csv was edited in 0__Dylan_MakeAbundance_2sizeclasses.R
# datt$PlotID <- as.factor(datt$PlotID) # 2272. added new data Chase & Knight
# datt$Clust_manu <- datt$Clust_manu %>% replace_na('anone')
# datt <- filter(datt, Study != "Hatheway_1952") # removing "Hatheway_1952"  2219
# datt <- filter(datt, Family != "Cibotiaceae" & Family != "Blechnaceae" & Family != "Cyatheaceae") # 1860
# datt <- filter(datt, Scientific_name != "unknown_ilexlike") 
# datt <- filter(datt, geo_entity2 != "Kaua'i Island")
# 
# length(unique(datt$Scientific_name))
# 
# datt_40 <- dplyr::filter(datt, Plot_Prop_Invaded < 0.41) # 1305
# 
# datt_40 <- datt_40 %>% separate(Clust_manu, c("Map_class"), remove = FALSE)
# datt_40$Map_class <- paste(datt_40$Map_class, "", datt_40$PlotID)
# 
# datt_nat = filter(datt_40, Native_Status == "native") # 1092
# datt_nat$PlotID = as.factor(datt_nat$PlotID)
# 
# length(unique(datt$Scientific_name)) # 146 species  
# spp_all <- dplyr::summarize(dplyr::group_by(datt, Scientific_name, Accepted_name_TNRS))
# 
# # ORGANAZING TRAITS  
# 
# setwd("C:/Hawaii_project/Functional_data/Hawaii_traits/28Nov2021_csv_individual_trait")
# 
# t          = read.csv("SLA.csv")
# t          = t[, c("AccSpeciesName", "StdValue")]
# length(unique(t$AccSpeciesName))
# SLA = aggregate(StdValue ~  AccSpeciesName, data = t, mean)
# 
# t          = read.csv("WD.csv")
# t          = t[, c("AccSpeciesName", "StdValue")]
# length(unique(t$AccSpeciesName))
# wood_density = aggregate(StdValue ~  AccSpeciesName, data = t, mean)
# 
# t   = read.csv("MaxHeight.csv")
# t          = t[, c("AccSpeciesName", "StdValue")]
# length(unique(t$AccSpeciesName))
# max_height = aggregate(StdValue ~  AccSpeciesName, data = t, mean)
# 
# t        = read.csv("NitrogenMass.csv")    # UNIT mg.g-1
# t        = t[, c("AccSpeciesName", "StdValue")]
# length(unique(t$AccSpeciesName))
# Nmass = aggregate(StdValue ~  AccSpeciesName, data = t, mean)
# 
# t        = read.csv("Leaf_th.csv")
# t        = t[, c("AccSpeciesName", "StdValue")]
# length(unique(t$AccSpeciesName))
# Leaf_th = aggregate(StdValue ~  AccSpeciesName, data = t, mean)
# 
# # ##---
# # control <- read.csv("C:/Hawaii_project/Functional_data/Hawaii_traits/control.csv")
# # length(unique(control$Scientific_name)) # 185 species
# # 
# # # Check trait coverage native species 
# # traits_cover <- dplyr::filter(control, Native_Status == "native" ) # 110 species 
# # names(traits_cover)
# # traits_cover <- traits_cover[, c("Scientific_name", "SLA_mm2.mg.1", "WD_g.cm.3", "Height_m", 
# #                                  "LeafThickness_mm", "Nmass_mg.g.1", "SeedMass_mg")]
# # vis_dat(traits_cover)
# # vis_miss(traits_cover)
# 
# ### Adding traits to master species list 
# spp_all$AccSpeciesName <- spp_all$Scientific_name
# 
# trait = plyr::join(spp_all, SLA, by = "AccSpeciesName")
# trait$SLA_mm2mg1 = trait$StdValue
# trait$StdValue = NULL
# 
# trait = plyr::join(trait, wood_density, by = "AccSpeciesName")
# trait$wood_density_gcm3 = trait$StdValue
# trait$StdValue = NULL
# 
# trait = plyr::join(trait, max_height, by = "AccSpeciesName")
# trait$max_height_m = trait$StdValue
# trait$StdValue = NULL
# 
# trait = plyr::join(trait, Nmass, by = "AccSpeciesName")
# trait$Nmass = trait$StdValue
# trait$StdValue = NULL
# 
# trait = plyr::join(trait, Leaf_th, by = "AccSpeciesName")
# trait$Leaf_th = trait$StdValue
# trait$StdValue = NULL
# 
# is.num <- sapply(trait, is.numeric)
# trait[is.num] <- lapply(trait[is.num], round, 4)
# 
# vis_miss(trait[,4:7])
# trait$AccSpeciesName <- NULL # because it is not actually the most accepted names
# 
# ####################### Fill trait gaps using last TRY data retrieved in May 2022
# # NOTES: in TRY there are not many Hawaian species (i.e.,118 in our case missing) 
# load("C:/Hawaii_project/Hawaii/traits_2022_TRY.RData")
# traits_TRY = join
# names(traits_TRY)[2] <- "Accepted_name_TNRS"
# 
# trait1 <- left_join(trait, traits_TRY, by = "Accepted_name_TNRS")
# sum(is.na(trait1$AccSpeciesID)) # 118 NA. So there is only 46 species from TRY. 
# names(trait1)
# 
# trait2 <- trait1 %>%  # replacing NA in my trait data assembled 2021 with new TRY data
#   mutate(SLA_mm2mg1 = coalesce(SLA_mm2mg1, SLA_mean))
# 
# trait2 <- trait1 %>% 
#   mutate(wood_density_gcm3 = coalesce(wood_density_gcm3, SSD_mean))
# 
# trait2 <- trait1 %>% 
#   mutate(max_height_m = coalesce(max_height_m, H_mean))
# 
# trait2 <- trait1 %>% 
#   mutate(Nmass = coalesce(Nmass, N_mean))
# 
# vis_miss(trait2[, 3:6])
# 
# ####################### Fill trait gaps using Diaz et al data to complement Hawaii trait data 
# diaz = read.csv("C:/2022_Assemply_ms_PhDII/pub_R_code/submission_2/diaz_etal_new_gf_23052022_Joswig.csv")
# names(diaz)[11] <- "Accepted_name_TNRS"
# diaz = diaz[, c ("Accepted_name_TNRS",
#                  "LMA..g.m2.", "SSD.combined..mg.mm3.", "Plant.Height..m.","Nmass..mg.g.")]
# diaz$SLA = 1000/diaz$LMA..g.m2. # (SLA units mm2 /mg) this should be the units to add to Hawaii
# diaz$LMA..g.m2. = NULL
# diaz = diaz %>% filter(!is.na(Accepted_name_TNRS))
# 
# trait22 = trait2[, 1:6]
# trait3 <- left_join(trait22, diaz, by = "Accepted_name_TNRS")
# 
# names(trait3)  # filling NA in Hawaii trait with Diaz. the coverage in Diaz is poor
# trait3 <- trait3 %>% 
#   mutate(SLA_mm2mg1 = coalesce(SLA_mm2mg1, SLA))
# 
# trait3 <- trait3 %>% 
#   mutate(wood_density_gcm3 = coalesce(wood_density_gcm3, SSD.combined..mg.mm3.))
# 
# trait3 <- trait3 %>% 
#   mutate(max_height_m = coalesce(max_height_m, Plant.Height..m.))
# 
# trait3 <- trait3 %>% 
#   mutate(Nmass = coalesce(Nmass, Nmass..mg.g.))
# 
# trait3 = trait3[, 1:6]
# vis_miss(trait3[, 3:6])
# 
# ####################### Fill trait gaps using JOSWIG
# load("C:/2022_Assemply_ms_PhDII/Data/data_jjoswig2021/X0.RData") # TRY_Env   256.132 X 213
# names(TRY_Env)
# joswig <- TRY_Env[, c ("AccSpeciesID.x_obs", 
#                        "SLA_obs","SLA_gf",  # "SLA_pred",
#                        "LeN_obs","LeN_gf", 
#                        "PlantHeight_obs","PlantHeight_gf" , # "PlantHeight_pred", 
#                        "SSD_obs","SSD_gf")]  # "SSD_pred"
# 
# names(joswig)[1] <- "AccSpeciesID"
# 
# tmp_TRY <- traits_TRY[, c ("AccSpeciesID","Accepted_name_TNRS")]
# 
# joswig <- left_join(joswig, tmp_TRY, by = "AccSpeciesID") # add to Joswig data species names from TRY
# joswig <- joswig %>% filter(!is.na(Accepted_name_TNRS))
# 
# length(unique(joswig$Accepted_name_TNRS)) # 4934 species
# names(joswig)
# 
# tmp_joswig <- dplyr::summarize(group_by(joswig, Accepted_name_TNRS),
#                                SLA_gf = mean(SLA_gf),
#                                LeN_gf = mean(LeN_gf),
#                                PlantHeight_gf = mean(PlantHeight_gf),
#                                SSD_gf = mean(SSD_gf))
# 
# 
# trait4 <- left_join(trait3, tmp_joswig, by = "Accepted_name_TNRS")
# 
# trait4 <- trait4 %>%  # # filling NA in Hawaii trait with Joswig data. Coverage is poor
#   mutate(SLA_mm2mg1 = coalesce(SLA_mm2mg1, SLA_gf))   
# 
# trait4 <- trait4 %>% 
#   mutate(wood_density_gcm3 = coalesce(wood_density_gcm3, SSD_gf))
# 
# trait4 <- trait4 %>% 
#   mutate(max_height_m = coalesce(max_height_m, PlantHeight_gf))
# 
# trait4 <- trait4 %>% 
#   mutate(Nmass = coalesce(Nmass, LeN_gf))
# 
# trait4 = trait4[, 1:6]
# vis_miss(trait4[, 3:6])
# 
# trait4
# 
# write.csv(trait4, file = "C:/Hawaii_project/Hawaii/trait4_filled.csv", row.names = FALSE)
# 
# # ####################### Fill trait gaps using BARAJAS et al ONLY ONE SPECIES IN BARAJAS ET AL
# # Barajasetal <- read.csv("C:/2022_Assemply_ms_PhDII/pub_R_code/submission_2/Tenerife_names_TNRS.csv")
# # names(Barajasetal)
# # Barajasetal <- Barajasetal[, c ("Accepted_name_TNRS", "LMA","Nmass","SSD" , "H")]
# # trait5 <- left_join(trait4, Barajasetal, by = "Accepted_name_TNRS")
# 
# save(trait, file = "C:/Hawaii_project/Hawaii/data_Trait.Rdata") # 15 March 2022
# 
# #### ALL code before won be published


#_______________________________________________________________________________

# TRAIT IMPUTATION ----

# LOADS TRAITS
# trait4 <- read.csv("C:/Hawaii_project/Hawaii/trait4_filled.csv") # traits assembled 6.June 2022 gap filled with Diaz, Barajas and Joswig data
# load("C:/Hawaii_project/Hawaii/datt_20221101.RData")
# trait4 <- trait4 %>% semi_join(datt, by = "Scientific_name") # keep species that match with species in datt 
# save(trait4, file = "C:/Hawaii_project/Hawaii/data_Trait_144sp_20230418.RData")

load("C:/Hawaii_project/Hawaii/data_Trait_144sp_20230418.RData")

vis_miss(trait4[, 3:6]) # the new data I assembled (6/06/2022) has 10 more traits than the previous one. 32 % missing data

traits <- trait4
traits$Accepted_name_TNRS = NULL
traits$Scientific_name <- str_replace_all(traits$Scientific_name," ","_")
traits                 <- tibble::column_to_rownames(traits, var = "Scientific_name")

# LOADS TREE
load("H:/Manuscripts/Hawaii_project/Hawaii/data_phylo_trees_20221101.RData")

tree <- hawaii.tree_all$scenario.2 # tree for native species only
tree <- as(tree$run.1, "phylo")
length(tree$tip.label) # 144 species

# Calculate Moran eigenvetors to use phylogenetic correlation structure to predict traits
# Eigenvectors eliminate features that have a strong correlation 
# between them and also help in reducing over-fitting.
# create phylogenetic proximity table. 
prox.Ab.all <- proxTips(tree, method = "Abouheif", normalize="none")
dim(prox.Ab.all) # 144

prox <- prop.table(prox.Ab.all, 1) # standardize by row
prox <- 0.5 * (prox + t(prox))     # make matrix symetric

ME <- me.phylo(prox = prox) # create Moran's eigenvectors based on phylogenetic distance matrix. closely related species will have similar ME values 
ME <- ME[rownames(traits),]

trait.imp <- cbind(traits, ME[,1:30]) # This is Morans Eigenvectors plus species by traits matrix (with column_to_rownames). 

#  ---- Phylogenetic imputation using missForest ----
set.seed(42) 
dfk <- data.frame(matrix(NA, nrow = 30, ncol = 5)) # ncol is = 4 traits + k = 5 (i.e., # traits + 1)
names(trait.imp)
colnames(dfk) <- c("k", "OOB_SLA","OOB_WD","OOB_H","OOB_Nmass")  

for (n in 1:30) {
  dfimp <- trait.imp[, 1: ( 4 +n)] # per trait (i.e., 4) a loop is run for each phylogenetic eigenvector number (all number > 10 (i.e., traits number) untill 30) 
  o <- missForest(dfimp, maxiter = 25, ntree = 100 , variablewise = TRUE) # One can put higher number of iterations
  dfk[n,1] <- n
  dfk[n,2] <- o$OOBerror[1] #save OOBerror for target traits only. OOB, is out-of-bag imputation error
  dfk[n,3] <- o$OOBerror[2]    
  dfk[n,4] <- o$OOBerror[3]
  dfk[n,5] <- o$OOBerror[4]
}

dfk2<-dfk %>%                                                                          # imputation errors per trait
  summarize(min_SLA = min(OOB_SLA), k_min_SLA = k[which.min(OOB_SLA)], 
            min_WD = min(OOB_WD), k_min_WD = k[which.min(OOB_WD)],
            min_H = min(OOB_H), k_min_H = k[which.min(OOB_H)],
            min_Nmass = min(OOB_Nmass), k_min_Nmass = k[which.min(OOB_Nmass)],
  )

# phylogenetically-informed imputed has generally similar/lower error rates
# Chose the number of eigenvectors that minimizes the imputation error (i.e., OOB_trait).

SLA_ideal  <-missForest(trait.imp[, 1: (10+dfk2$k_min_SLA)], maxiter = 25, ntree = 100 ,   variablewise = TRUE)  
WD_ideal   <-missForest(trait.imp[, 1: (10+dfk2$k_min_WD)], maxiter = 25, ntree = 100 ,   variablewise = TRUE) 
H_ideal    <-missForest(trait.imp[, 1: (10+dfk2$k_min_H)], maxiter = 25, ntree = 100 ,   variablewise = TRUE)  ## I had to chang this to 34. 
Nmass_ideal<-missForest(trait.imp[, 1: (10+dfk2$k_min_Nmass)], maxiter = 25, ntree = 100 ,   variablewise = TRUE) 

best_SLA    <-tibble(SLA= SLA_ideal$ximp$SLA_mm2mg1, species=rownames(SLA_ideal$ximp))
best_WD     <-tibble(WD = WD_ideal$ximp$wood_density_gcm3, species=rownames(WD_ideal$ximp))
best_H      <-tibble(H  = H_ideal$ximp$max_height_m, species=rownames(H_ideal$ximp))
best_Nmass  <-tibble(Nmass = Nmass_ideal$ximp$Nmass, species=rownames(Nmass_ideal$ximp))

impute_out <-left_join(best_SLA, best_WD, by="species")%>%   # traits imputed using phylogeny and OOBerror
  left_join(.,best_H,by="species")%>%
  left_join(.,best_Nmass, by="species")%>%
  select(., species, SLA, WD, H, Nmass)

names(impute_out)[1] <- "Scientific_name"
impute_out$Scientific_name <- str_replace_all(impute_out$Scientific_name,"_"," ")

# save(impute_out, file = "C:/Hawaii_project/Hawaii/data_Trait_imputed_20230418.RData")

# Phylogenetically informed errors
errors_phylo_info <- dfk2
errors_phylo_info <- errors_phylo_info[c ("min_SLA", "min_WD", "min_H", "min_Nmass" )]
errors_phylo_info <- data.frame(OOB_SLA  = errors_phylo_info$min_SLA, 
                                OOB_WD= errors_phylo_info$min_WD, 
                                OOB_Nmass =errors_phylo_info$min_Nmass, 
                                OOB_H   =errors_phylo_info$min_H )

errors_phylo_info

# write.csv(errors_phylo_info, file = "C:/Hawaii_project/Hawaii/Imputation_errors.csv" )  

# Plot supplementary material Figure 1 

load("C:/Hawaii_project/Hawaii/data_Trait_144sp_20230418.RData")
load("C:/Hawaii_project/Hawaii/data_Trait_imputed_20230418.RData")

trait = trait4
trait_imp = impute_out

vis_miss(trait[,2:5]) # 42.7 % data missing

trait_missing <- vis_miss(trait[, 2:5])# + 
  theme_set(theme_minimal(base_size = 8))

names(trait_imp); names(trait)

trait <- data.frame(Scientific_name = trait$Scientific_name,
                    SLA = trait$SLA_mm2mg1 ,  
                    WD  = trait$wood_density_gcm3 , 
                    H   = trait$max_height_m,
                    Nmass = trait$Nmass)

trait$id <- "raw trait"
trait_imp$id <- "imputed trait"

trait_all = rbind(trait, trait_imp)


coco = c( "mediumslateblue", "gray40") 
  
SLA = ggplot(data= trait_all, aes(x = SLA, group = id, fill= id)) + 
  geom_density(adjust=1.5, alpha=.5)+ 
  scale_color_manual(values = coco) + scale_fill_manual(values = coco) + 
  theme_set(theme_minimal(base_size = 8))
WD = ggplot(data= trait_all, aes(x = WD, group = id, fill= id)) + 
  geom_density(adjust=1.5, alpha=.5)+ 
  scale_color_manual(values = coco) + scale_fill_manual(values = coco) + 
  theme_set(theme_minimal(base_size = 8))

H = ggplot(data= trait_all, aes(x = H, group = id, fill= id)) + 
  geom_density(adjust=1.5, alpha=.5)+ 
  scale_color_manual(values = coco) + scale_fill_manual(values = coco) + 
  theme_set(theme_minimal(base_size = 8))

N = ggplot(data= trait_all, aes(x = Nmass, group = id, fill= id)) + 
  geom_density(adjust=1.5, alpha=.5)+ 
  scale_color_manual(values = coco) + scale_fill_manual(values = coco) + 
  theme_set(theme_minimal(base_size = 8))

traits_density <- ggpubr::ggarrange(SLA, WD, H, N, ncol = 2, nrow = 2)

opq = ggpubr::ggarrange(trait_missing,traits_density, ncol = 2, widths = c(1, 2) )
opq

pdf(file = "C:/Hawaii_project/Figures/Supp_Fig_1_traits_assesment.pdf",
    width=10, # 89 mm (single column) is 3.5 inches.
    height=4,
    paper = "a4")
opq
dev.off()

krusl_sla = agricolae::kruskal(trait_all$SLA, trt = trait_all$id)
krusl_sla$groups

krusl_wd = agricolae::kruskal(trait_all$WD, trt = trait_all$id)
krusl_wd$groups

krusl_h = agricolae::kruskal(trait_all$H, trt = trait_all$id)
krusl_h$groups

krusl_n = agricolae::kruskal(trait_all$Nmass, trt = trait_all$id)
krusl_n$groups


