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

# # # 
# load("C:/Hawaii_project/Hawaii/datt_20221101.RData")
# 
# data <- datt[, c ("PlotID", "Scientific_name", "Abundance", "MAP", "geo_entity2")]
# 
# tmp0 <- dplyr::summarize(group_by(data, PlotID), ind_amount = sum(Abundance))
# data <- left_join(data, tmp0, by = "PlotID" )
# data <- filter(data, ind_amount > 15) # include plots with minimum 15 stems 
# data$ind_amount <- NULL
# 
# # Replicate rows by count - https://mgimond.github.io/ES218/Week03b.html
# data_1 <- uncount(data, Abundance) # converts data into long format, i.e., at stem level
# unique(data_1$geo_entity2)
# length(unique(data_1$PlotID)) # 374 plots with min 15 stems
# 
# length(unique(filter(data_1, geo_entity2 == "O'ahu Island")$PlotID))    # 35
# # length(unique(filter(data_1, geo_entity2 == "Hawai'i Island")$PlotID))  # 254
# # length(unique(filter(data_1, geo_entity2 == "Maui Nui")$PlotID))        # 85
# 
# isl_dat <- filter (data_1, geo_entity2 ==  "O'ahu Island") # "O'ahu Island"   "Hawai'i Island" "Maui Nui"
# 
# plots = unique(isl_dat$PlotID) ; length(unique(isl_dat$PlotID)) 
# 
# # # ################################################################################
# 
# Beta_MAP  <- c()
# 
# for (i in 1:length(plots)) {
#   for (j in i:length(plots)) {
#     if (i!=j) {
#       # print(c(plots[i],plots[j]))
#       
#       plot1 = filter(isl_dat, PlotID == plots[i])
#       plot2 = filter(isl_dat, PlotID == plots[j])
#       isl_2plots0 <- rbind(plot1, plot2)
#       
#       nbperm  <- 100   # number of permutations
#       
#       for (perm in 1:nbperm){
#         
#         isl_2plots <- isl_2plots0 %>% 
#           group_by(PlotID) %>%
#           sample_n(13)  # Takes randomly 10 rows per plot, i.e., 10 individuals.
#         
#         isl_2plots$n <- 1
#         
#         # Transforming data. Species in columns (as colnames) sites in rows (as rownames)
#         
#         Abun_p <- dplyr::summarize(group_by(isl_2plots, PlotID, Scientific_name),
#                                    Abundance = sum(n))
#         Abun_p <- pivot_wider(Abun_p, id_cols = Scientific_name, names_from = PlotID,
#                               values_from = Abundance, values_fill = list(Abundance=0))
#         Abun_p <- tibble::column_to_rownames(Abun_p, var = "Scientific_name") 
#         Abun_p <- as.data.frame(as.matrix(t(Abun_p)))
#         
#         beta   <- beta_C(Abun_p, C = 0.6, extrapolation = TRUE)
#         
#         # organizing data
#         PlotID   <- t(row.names(Abun_p)) # the second one I have to subtract 1
#         map      <- as.data.frame(unique(isl_2plots$MAP))
#         MAP_diff <- map[1,] - map[2,]
#         
#         x        <- cbind(beta, PlotID, MAP_diff, perm )  ## Creates Beta diversity versus MAP difference of plot pairs
#         colnames(x) <- c("beta_c", "plot_1", "plot_2", "MAP_diff", "perm")
#         
#         Beta_MAP <-  rbind(Beta_MAP, x)
#         
#         
#       }
#     }
#   }
# }

# save(Beta_MAP, file = "H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_oahu_20221101.RData") # started at 9:06 pm 1 Nov -ended 9.14 pm
# save(Beta_MAP, file = "H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_oahu_20221101_2.RData") # started at 9:43 pm 2 Nov -ended  9.44am
# save(Beta_MAP, file = "H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_mauinui_20221102.RData") # started at 9:22 pm 1 Nov  

# ##############################################################################
# Load computed Beta C

# Oahu
load("H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_oahu_20221101.RData")
Beta_MAP_o <- as.data.frame(Beta_MAP)
Beta_MAP_o$perm  <- as.integer(Beta_MAP_o$perm )

# load("H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_oahu_20221101_2.RData")
# Beta_MAP_o1 <- as.data.frame(Beta_MAP)
# Beta_MAP_o1$perm  <- (as.integer(Beta_MAP_o1$perm)+99)

Beta_MAP_o <- rbind(Beta_MAP_o, Beta_MAP_o1)
Beta_MAP_o$beta_c <- as.numeric(Beta_MAP_o$beta_c)
Beta_MAP_o$MAP_diff <- as.numeric(Beta_MAP_o$MAP_diff)

Beta_MAP_o$pair_ID <- paste(Beta_MAP_o$plot_1, Beta_MAP_o$plot_2, sep="_")

Beta_MAP_o_sum <- dplyr::summarize(group_by(Beta_MAP_o, pair_ID, MAP_diff), 
                                   BetaC_mean = mean(beta_c), BetaC_sd = sd(beta_c))
Beta_MAP_o_sum <- na.omit(Beta_MAP_o_sum)
length(unique(Beta_MAP_o_sum$pair_ID)) # 52 pairs 

# Maui Nui
load("H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_mauinui_20221102.RData")
Beta_MAP_m <- as.data.frame(Beta_MAP)
Beta_MAP_m$beta_c <- as.numeric(Beta_MAP_m$beta_c)
Beta_MAP_m$MAP_diff <- as.numeric(Beta_MAP_m$MAP_diff)

Beta_MAP_m$pair_ID <- paste(Beta_MAP_m$plot_1, Beta_MAP_m$plot_2, sep="_")

Beta_MAP_m_sum <- dplyr::summarize(group_by(Beta_MAP_m, pair_ID, MAP_diff), 
                                   BetaC_mean = mean(beta_c), BetaC_sd = sd(beta_c))
Beta_MAP_m_sum <- na.omit(Beta_MAP_m_sum)
length(unique(Beta_MAP_m_sum$pair_ID)) # 36 pairs 

# Hawaii
# load("H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_hawaii_20221101_1.RData")
# Beta_MAP_h <- as.data.frame(Beta_MAP)
# Beta_MAP_h$perm  <- as.integer(Beta_MAP_h$perm)
# load("H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_hawaii_20221103_3.RData")
# Beta_MAP_h1 <- as.data.frame(Beta_MAP)
# Beta_MAP_h1$perm  <- (as.integer(Beta_MAP_h1$perm)+25)
# load("H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_hawaii_20221103_4.RData")
# Beta_MAP_h2 <- as.data.frame(Beta_MAP)
# Beta_MAP_h2$perm  <- (as.integer(Beta_MAP_h2$perm)+50)
# load("H:/Manuscripts/Hawaii_project/Hawaii/Beta_MAP_ALL_hawaii_20221103_5.RData")
# Beta_MAP_h3 <- as.data.frame(Beta_MAP)
# Beta_MAP_h3$perm  <- (as.integer(Beta_MAP_h3$perm)+75)
# 
# Beta_MAP_h <- rbind(Beta_MAP_h, Beta_MAP_h1, Beta_MAP_h2, Beta_MAP_h3)
# save(Beta_MAP_h, file = "C:/Hawaii_project/Hawaii/Beta_MAP_h_20221104.RData")

load("C:/Hawaii_project/Hawaii/Beta_MAP_h_20221104.RData")

Beta_MAP_h$beta_c <- as.numeric(Beta_MAP_h$beta_c)
Beta_MAP_h$MAP_diff <- as.numeric(Beta_MAP_h$MAP_diff)

Beta_MAP_h$pair_ID <- paste(Beta_MAP_h$plot_1, Beta_MAP_h$plot_2, sep="_")

Beta_MAP_h_sum <- dplyr::summarize(group_by(Beta_MAP_h, pair_ID, MAP_diff), 
                                   BetaC_mean = mean(beta_c), BetaC_sd = sd(beta_c))
Beta_MAP_h_sum <- na.omit(Beta_MAP_h_sum)

length(unique(Beta_MAP_h_sum$pair_ID)) # 11713 pairs 

# ##############################################################################
## Plotting relationships 

Beta_MAP_o_sum$island = "Oahu" 
Beta_MAP_h_sum$island = "Hawaii" 
Beta_MAP_m_sum$island = "MauiNui" 
Beta_MAP_ALL_sum = rbind(Beta_MAP_h_sum, Beta_MAP_o_sum, Beta_MAP_m_sum )

unique(sort(Beta_MAP_ALL_sum$island))
coco2 = c(
  "#00AFBB", # blue
  "gray60 ", 
  "#E7B800") # yellow


beta_map = ggplot(Beta_MAP_ALL_sum, aes(x = abs(MAP_diff), y = BetaC_mean)) +   
  geom_point(aes(color = island), size = .3, show.legend = FALSE, alpha = .2) + 
  
  geom_smooth(aes(color = island, fill = island), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  
  scale_color_manual(values = coco2) + scale_fill_manual(values = coco2)+
  ylab("mean Beta C - Native")+
  theme_set(theme_minimal(base_size = 8))
beta_map

# 
pdf(file = "C:/Hawaii_project/Figures/Figure_3_Beta_all.pdf",   # Figure_1_native
    width=2.3, # 89 mm (single column) is 3.5 inches.
    height=2,
    paper = "a4")
beta_map
dev.off()

##############
# Testing if the interaction is significant or not
z <- Beta_MAP_ALL_sum
#    z <- z %>% mutate(island = factor(island, levels = c("MauiNui", "Hawaii" , "Oahu")))

glm_t_0 <- glm(BetaC_mean ~ log(abs(MAP_diff)) * island, family  = Gamma(link = "log"), data = z)
glm_t_0_noi <- glm(BetaC_mean ~ abs(MAP_diff) , family  = Gamma(link = "log"), data = z)

lrtest(glm_t_0, glm_t_0_noi)

q_colors =  1 
v_colors =  viridis::viridis(q_colors) 

beta_map2 <- jtools ::plot_summs(glm_t_0, scale = TRUE,  
                                 ci_level = 0.95, inner_ci_level = .85, 
                                 colors = v_colors) + 
  theme_set(theme_minimal(base_size = 8))

beta_map1

beta_map2

#
pdf(file = "C:/Hawaii_project/Figures/Suppl_Fig_3_Beta_all_2.pdf",   # Figure_1_native
    width=4.5, # 89 mm (single column) is 3.5 inches.
    height=1.5,
    paper = "a4")
beta_map2
dev.off()

###########








