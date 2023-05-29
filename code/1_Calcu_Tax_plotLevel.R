
# CODE LAST EDITS by PAOLA 23.12.2021

library(dplyr)
library(iNEXT.3D)
library(mobr)
library(reshape2)

library(ggplot2)
library(ggpubr)
library(gg.gap)
library(devtools)
library(tidyr)

# ========================================================================================================== #

rm(list=ls())

# EDITED OPENAHELE DATA
# Analysis full data. 

# HawIslandsAbundance_2SizeClasses_100plus is updated OpenNahele, i.e., included Hathaway (Oahu) and certain epithet names were assigned to species with spp.
# datt = read.csv("C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2.csv",sep="," ) 

datt = read.csv("C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2_1.csv" ) # includes cookies info. saved 31.01.2022. csv edited in 0__Dylan_MakeAbundance_2sizeclasses.R

datt$PlotID = as.factor(datt$PlotID) # 2160

unique(datt$Study)

length(unique(datt$PlotID)) # 524
datt_pl_a = aggregate(cbind(Study, Island, 
                             Plot_Area,  Lat_Dec, Long_Dec, 
                             MAP, Plot_Prop_Invaded) ~ PlotID, data = datt, FUN = max)

datt_nat = dplyr::filter(datt, Native_Status == "native") # 1661 
datt_nat$PlotID = as.factor(datt_nat$PlotID)

length(unique(datt_nat$PlotID)) # 480

# OpenNahele were filtered (for plots with max 40 % aliens, huge plots "hyppnet" were removed, native species only)

# ========================================================================================================== #
# USING MOBR METRICS

#_____________________ native species

tmp           <- dplyr::summarize(group_by(datt_nat, PlotID, Scientific_name),
                                  Abundance=sum(Abundance))
tmp$Abundance <- as.numeric(tmp$Abundance)
tmp           <- pivot_wider(tmp, id_cols = Scientific_name, names_from = PlotID, 
                             values_from = Abundance, values_fill = list(Abundance=0))
Abund_datt_nat_plot <- tibble::column_to_rownames(tmp, var = "Scientific_name") # species x abundance matrix

mx = Abund_datt_nat_plot
mx  <- t(as.matrix(mx))
summary((rowSums(mx))) # min 1 ind. per plot, max 270. Average of 51 ind. per plot. 

N     <- calc_biodiv (mx, groups = row.names(mx), index = c("N"))  # Number of individuals
S     <- calc_biodiv (mx, groups = row.names(mx), index = c("S"))  # Number of species
Spie  <- calc_biodiv (mx, groups = row.names(mx), index = c("S_PIE"),  extrapolate = TRUE) # Effective number of species based on PIE
Sn    <- calc_biodiv (mx, groups = row.names(mx), index = c("S_n"), effort = c(5, 10, 20, 50, 60, 70) , extrapolate = TRUE, return_NA = FALSE) # Rarefied or extrapolated number of species for n individuals
Sn    <- pivot_wider(Sn,   # "widens" data, increasing the number of columns and decreasing the number of rows
                 id_cols = group, names_from = effort, values_from = value) 

anyNA(Spie$value)

N$N_native = N$value
S$S_native = S$value
Spie$Spie_native = Spie$value # there is NAs being produced, WHY? 23.12.2021
Sn$Sn_5_native  = Sn$`5`
Sn$Sn_10_native = Sn$`10`
Sn$Sn_20_native = Sn$`20`
Sn$Sn_50_native = Sn$`50`
Sn$Sn_60_native = Sn$`60`
Sn$Sn_70_native = Sn$`70`

results_native = cbind(N[c("group", "N_native")], S[c ("S_native")],  
                       Sn[ c("Sn_5_native","Sn_10_native","Sn_20_native","Sn_50_native", "Sn_60_native","Sn_70_native")],  
                       Spie[c ("Spie_native")]) 

colnames(results_native)[1]  = "PlotID"

results_native =   plyr::join(datt_pl_a, results_native, by = "PlotID")  # 524 plots (plots with 100 aliens will have NAs)


#_____________________ All species

tmp           <- dplyr::summarize(group_by(datt, PlotID, Scientific_name),
                                  Abundance=sum(Abundance))
tmp$Abundance <- as.numeric(tmp$Abundance)
tmp           <- pivot_wider(tmp, id_cols = Scientific_name, names_from = PlotID, 
                             values_from = Abundance, values_fill = list(Abundance=0))
Abund_datt_all_plot            <- tibble::column_to_rownames(tmp, var = "Scientific_name") # species x abundance matrix

mx = Abund_datt_all_plot
mx  <- t(as.matrix(mx))
summary((rowSums(mx)))

N     <- calc_biodiv (mx, groups = row.names(mx), index = c("N"))  # Number of individuals
S     <- calc_biodiv (mx, groups = row.names(mx), index = c("S"))  # Number of species
Spie  <- calc_biodiv (mx, groups = row.names(mx), index = c("S_PIE"),  extrapolate = TRUE) # Effective number of species based on PIE
Sn    <- calc_biodiv (mx, groups = row.names(mx), index = c("S_n"), effort = c(5, 10, 20, 50, 60, 80) , extrapolate = TRUE, return_NA = FALSE) # Rarefied or extrapolated number of species for n individuals
Sn    <- pivot_wider(Sn, id_cols = group, names_from = effort, values_from = value) 

N$N_all = N$value
S$S_all = S$value
Spie$Spie_all = Spie$value
Sn$Sn_5_all  = Sn$`5`
Sn$Sn_10_all = Sn$`10`
Sn$Sn_20_all = Sn$`20`
Sn$Sn_50_all = Sn$`50`
Sn$Sn_60_all = Sn$`60`
Sn$Sn_80_all = Sn$`80`

results_all = cbind(N[c("group", "N_all")], S[c ("S_all")],  
                    Sn[ c("Sn_5_all","Sn_10_all","Sn_20_all","Sn_50_all", "Sn_60_all","Sn_80_all")],  
                    Spie[c ("Spie_all")]) 

colnames(results_all)[1]  = "PlotID"

results_all =   plyr::join(datt_pl_a, results_all, by = "PlotID")

length(unique(results_all$PlotID))
length(unique(results_native$PlotID))

results = cbind(results_native, results_all[, c ( "N_all", "S_all", 
                                                  "Sn_5_all","Sn_10_all","Sn_20_all","Sn_50_all", "Sn_60_all","Sn_80_all", 
                                                  "Spie_all" ) ])

write.csv (results, file = "C:/Hawaii_project/New_code/results_OpenNa_metrics_plotlevel_23122021.csv")



# ========================================================================================================== #
# Using Anne Chao et al 2021 metrics

Abun     <- Abund_datt_nat_plot # my data: 111 species and 480 sites (plots)  
Abun$tmp <- row.names(Abun)
row.names(Abun) <- Abun$tmp
Abun$tmp <- NULL

# ========================================================================================================== #
# Taxonomic diversity

TD_est <- estimate3D(data = Abun, diversity = 'TD', q = c(0), datatype = 'abundance', base = 'size',
                     level = 50, nboot = 0)




