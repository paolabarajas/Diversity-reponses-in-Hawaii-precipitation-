## WHAT THIS CODE DOES
# 1. Control for the difference in plot sizes I randomly sample a minimum of different individuals from plots
# randomly select two plots 
# sample 10 individuals from each plot 
# I compute Beta C between this two randomly selected plots
# I compute the difference of PREC between the two plots
###
# PROTOCOL that makes use of interpolation and extrapolation to compare across across multiple assemblages
# 1. Determine the appropriate target coverage value Ctarget for the standardization:
# 1.1 For each assemblage j, determine the smallest number of individuals observed at the alpha-scale and call it  Nminj.
c_t <- C_target(Abun_p) # target coverage value

# 2. Calculate ??C
# Returns expected sample coverage of a sample x for a smaller than observed sample size m 
beta_C(Abun_p, C = c_t) # What is beta_C for a coverage value of 60%?

#  ??C does not have an analytical variance estimator but
# we recommend calculating a distribution of ??C for repeated random subsets of the samples.
# Using, e.g., use pairwise comparisons of samples, or comparisons among larger subsets of samples. 
# While such variance can help to contextualize the observed values of ??-diversity it has to be interpreted with caution

# formula of possible unique pairs (168*(168-1))/2. n(n-1)/2 where n is the number of items in the set


##################
 # BETA DIVERSITY 
 ##################
 
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

load("C:/Hawaii_project/Hawaii/datt_20221101.RData")

data <- datt_nat[, c ("PlotID", "Scientific_name", "Abundance", "MAP", "geo_entity2", "Map_class")]

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

isl_dat <- filter (data_1, geo_entity2 ==  "Maui Nui") # "O'ahu Island"   "Hawai'i Island" "Maui Nui"

plots = unique(isl_dat$PlotID) ; length(unique(isl_dat$PlotID)) 

# # ################################################################################
Beta_MAP  <- c()

for (i in 1:length(plots)) {
  for (j in i:length(plots)) {
    if (i!=j) {
      # print(c(plots[i],plots[j]))

      plot1 = filter(isl_dat, PlotID == plots[i])
      plot2 = filter(isl_dat, PlotID == plots[j])
      isl_2plots0 <- rbind(plot1, plot2)

      nbperm  <- 100 # number of permutations

      for (perm in 1:nbperm){

        isl_2plots <- isl_2plots0 %>%
          group_by(PlotID) %>%
          sample_n(13)  # Takes randomly 10 rows per plot, i.e., 10 individuals.

        isl_2plots$n <- 1

        # Transforming data. Species in columns (as colnames) sites in rows (as rownames)

        Abun_p <- dplyr::summarize(group_by(isl_2plots, PlotID, Scientific_name),
                                   Abundance = sum(n))
        Abun_p <- pivot_wider(Abun_p, id_cols = Scientific_name, names_from = PlotID,
                              values_from = Abundance, values_fill = list(Abundance=0))
        Abun_p <- tibble::column_to_rownames(Abun_p, var = "Scientific_name")
        Abun_p <- as.data.frame(as.matrix(t(Abun_p)))

        beta   <- beta_C(Abun_p, C = 0.61, extrapolation = TRUE)

        # organizing data
        PlotID   <- t(row.names(Abun_p)) # the second one I have to subtract 1
        map      <- as.data.frame(unique(isl_2plots$MAP))
        MAP_diff <- map[1,] - map[2,]

        Map_class <-  as.data.frame(t (unique(isl_2plots0$Map_class)))

        x        <- cbind(beta, PlotID, MAP_diff, perm) #  Map_class)  ## Creates Beta diversity versus MAP difference of plot pairs
        colnames(x) <- c("beta_c", "plot_1", "plot_2", "MAP_diff", "perm") #  ' "map_1","map_2")

        Beta_MAP <-  rbind(Beta_MAP, x)
        # Beta_MAP <- dplyr::bind_rows(Beta_MAP, x)

      }
    }
  }
}
# # ################################################################################

save(Beta_MAP, file = "H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_mauinui_20221102_2.RData") # started at 6.08pm 2 Nov


################################################################################
# Plots ----
load("C:/Hawaii_project/Hawaii/Beta_c_calculated_20220916.RData") # native species only beta results 

## Three islands
Beta_MAP_o_sum$island = "Oahu" 
Beta_MAP_h_sum$island = "Hawaii" 
Beta_MAP_m_sum$island = "MauiNui" 
Beta_MAP_nat_sum = rbind(Beta_MAP_h_sum, Beta_MAP_m_sum, Beta_MAP_o_sum)

unique(sort(Beta_MAP_nat_sum$island))
coco2 = c(
  "#00AFBB", # blue
  "gray75 ", 
  "#E7B800") # yellow


############ ************************* work 2 nov 
beta_map = ggplot(Beta_MAP_nat_sum, aes(x = abs(MAP_diff), y = BetaC_mean)) +   
  geom_point(aes(color = island), size = .3, show.legend = FALSE, alpha = .2) + 
  
  geom_smooth(aes(color = island, fill = island), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  
  scale_color_manual(values = coco2) + scale_fill_manual(values = coco2)+
    ylab("mean Beta C - Native")+
  theme_set(theme_minimal(base_size = 8))
beta_map

pdf(file = "C:/Hawaii_project/Figures/Figure_3_Beta_native.pdf",   
    width=2.3, # 89 mm (single column) is 3.5 inches.
    height=2,
    paper = "a4")
beta_map
dev.off()

## 
z <- Beta_MAP_nat_sum

z <- z %>%
  mutate(island = factor(island, levels = c("MauiNui", "Hawaii" , "Oahu")))

glm_t_0 <- glm(BetaC_mean ~ log(abs(MAP_diff)) * island, family  = Gamma(link = "log"), data = z)
glm_t_0_noi <- glm(BetaC_mean ~ abs(MAP_diff) , family  = Gamma(link = "log"), data = z)

lmtest::lrtest(glm_t_0, glm_t_0_noi)

q_colors =  1 
v_colors =  viridis::viridis(q_colors) 

beta_map2 <- jtools ::plot_summs(glm_t_0, scale = TRUE,  
                                 ci_level = 0.95, inner_ci_level = .85, 
                                 colors = v_colors) + 
  theme_set(theme_minimal(base_size = 8))

beta_map1

beta_map2
#

# 
pdf(file = "C:/Hawaii_project/Figures/Suppl_Fig_3_Beta_native_2.pdf",   
    width=6, # 89 mm (single column) is 3.5 inches.
    height=1.5,
    paper = "a4")
beta_map2
dev.off()
############ ************************* work 2 nov 



