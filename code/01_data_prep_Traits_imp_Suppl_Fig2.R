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

setwd("C:/Hawaii_project/Hawaii/pub_code_data/data")
trait4 <- read.csv("data_Trait_144sp_20230418.csv")

vis_miss(trait4[, 3:6]) # the new data I assembled (6/06/2022) has 10 more traits than the previous one. 32 % missing data

traits <- trait4
traits$Accepted_name_TNRS <- NULL
traits$Scientific_name <- str_replace_all(traits$Scientific_name," ","_")
traits                 <- tibble::column_to_rownames(traits, var = "Scientific_name")

# LOADS TREE
load("data_phylo_trees_20230419.RData")

tree <- hawaii.tree_all$scenario.2 # tree for native species only
tree <- as(tree$run.1, "phylo")
length(tree$tip.label) # 142 species

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

# Plot supplementary material Figure 2
trait4 <- read.csv("data_Trait_144sp_20230418.csv")
load("data_Trait_imputed_20230418.RData")

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

pdf(file = "C:/Hawaii_project/Figures/Supp_Fig_2_traits_assesment.pdf",
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


