library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(devtools)
library(tidyr)
library("betaC") # devtools::install_github("T-Engel/betaC")
library(purrr)
options(dplyr.summarise.inform = FALSE)

##################
# BETA DIVERSITY 
##################
setwd("C:/Hawaii_project/Hawaii/pub_code_data/data")

load("datt_20230419.RData")

data <- datt_nat[, c ("PlotID", "Scientific_name", "Abundance", "MAP", "geo_entity2", "Map_class")] # NATIVE ONLY (relatively uninvaded plots)
# data <- datt[, c ("PlotID", "Scientific_name", "Abundance", "MAP", "geo_entity2")] # NATIVE + ALIENS (highly invaded plots)

####
tmp0 <- dplyr::summarize(group_by(data, PlotID), ind_amount = sum(Abundance))
data <- left_join(data, tmp0, by = "PlotID" )
data <- filter(data, ind_amount > 15) # include plots with minimum 15 stems 
data$ind_amount <- NULL

# Replicate rows by count - https://mgimond.github.io/ES218/Week03b.html
data_1 <- uncount(data, Abundance) # converts data into long format, i.e., at stem level
unique(data_1$geo_entity2)
length(unique(data_1$PlotID)) # 281 plots with min 15 stems

length(unique(filter(data_1, geo_entity2 == "O'ahu Island")$PlotID))    # 18
length(unique(filter(data_1, geo_entity2 == "Hawai'i Island")$PlotID))  # 190
length(unique(filter(data_1, geo_entity2 == "Maui Nui")$PlotID))        # 73

isl_dat <- filter (data_1, geo_entity2 ==  "Hawai'i Island") # "O'ahu Island" "Hawai'i Island" "Maui Nui"
plots = unique(isl_dat$PlotID) ; length(unique(isl_dat$PlotID)) 

# # ################################################################################
Beta_MAP  <- c()

for (i in 1:length(plots)) {
  for (j in i:length(plots)) {
    if (i!=j) {
      
      plot1 = filter(isl_dat, PlotID == plots[i])
      plot2 = filter(isl_dat, PlotID == plots[j])
      isl_2plots0 <- rbind(plot1, plot2)

      nbperm  <- 100 # number of permutations

      for (perm in 1:nbperm){

        isl_2plots <- isl_2plots0 %>%
          group_by(PlotID) %>%
          sample_n(13)  # Takes randomly 10 individuals.

        isl_2plots$n <- 1

        # Transforming data. Species in columns (as colnames) sites in rows (as rownames)
        Abun_p <- dplyr::summarize(group_by(isl_2plots, PlotID, Scientific_name),
                                   Abundance = sum(n))
        Abun_p <- pivot_wider(Abun_p, id_cols = Scientific_name, names_from = PlotID,
                              values_from = Abundance, values_fill = list(Abundance=0))
        Abun_p <- tibble::column_to_rownames(Abun_p, var = "Scientific_name")
        Abun_p <- as.data.frame(as.matrix(t(Abun_p)))

        beta   <- beta_C(Abun_p, C = 0.60, extrapolation = TRUE) # change to 0.3 for the sensitivity analysis

        # organizing data
        PlotID   <- t(row.names(Abun_p)) 
        map      <- as.data.frame(unique(isl_2plots$MAP))
        MAP_diff <- map[1,] - map[2,]

        Map_class <-  as.data.frame(t (unique(isl_2plots0$Map_class)))

        x        <- cbind(beta, PlotID, MAP_diff, perm)  # Creates Beta diversity versus MAP difference of plot pairs
        colnames(x) <- c("beta_c", "plot_1", "plot_2", "MAP_diff", "perm") 

        Beta_MAP <-  rbind(Beta_MAP, x)
        # Beta_MAP <- dplyr::bind_rows(Beta_MAP, x)

      }
    }
  }
}

# # ################################################################################

# save(Beta_MAP, file = "Beta_MAP_NAT_hawaii_03_20230620.RData") # Save the rest of the runs