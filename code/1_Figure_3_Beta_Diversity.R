library(dplyr, warn.conflicts = FALSE)

library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(devtools)
library(tidyr)
library(purrr)
library(lmtest) 
options(dplyr.summarise.inform = FALSE)

##################
# BETA DIVERSITY 
##################
setwd("C:/Hawaii_project/Hawaii/pub_code_data/data")

load("Beta_MAP_results_20221104.RData") # results native+aliens (highly invaded plots) 
Beta_MAP_o_sum_natali <- dplyr::summarize(group_by(Beta_MAP_o, pair_ID, MAP_diff),
                                   BetaC_mean = mean(beta_c), BetaC_sd = sd(beta_c))
Beta_MAP_o_sum_natali <- na.omit(Beta_MAP_o_sum_natali)
length(unique(Beta_MAP_o_sum_natali$pair_ID)) # 52 pairs
Beta_MAP_m_sum_natali <- dplyr::summarize(group_by(Beta_MAP_m, pair_ID, MAP_diff),
                                   BetaC_mean = mean(beta_c), BetaC_sd = sd(beta_c))
Beta_MAP_m_sum_natali <- na.omit(Beta_MAP_m_sum_natali)
length(unique(Beta_MAP_m_sum_natali$pair_ID)) # 36 pairs
Beta_MAP_h_sum_natali <- dplyr::summarize(group_by(Beta_MAP_h, pair_ID, MAP_diff),
                                   BetaC_mean = mean(beta_c), BetaC_sd = sd(beta_c))
Beta_MAP_h_sum_natali <- na.omit(Beta_MAP_h_sum_natali)
length(unique(Beta_MAP_h_sum_natali$pair_ID)) # 11713 pairs

load("Beta_c_calculated_20220916.RData") # native species only beta results 
Beta_MAP_o_sum <- na.omit(Beta_MAP_o_sum)
length(unique(Beta_MAP_o_sum$pair_ID)) # 153 pairs
Beta_MAP_m_sum <- na.omit(Beta_MAP_m_sum)
length(unique(Beta_MAP_m_sum$pair_ID)) # 2556 pairs
Beta_MAP_h_sum <- na.omit(Beta_MAP_h_sum)
length(unique(Beta_MAP_h_sum$pair_ID)) # 12729 pairs

# # sensitivity analysis using maximum coverage 30%
# load('results_Beta_MAP_03_sensit.RData')

# Plotting relationships native species only and relative uninvaded plots
Beta_MAP_o_sum$island = "Oahu"
Beta_MAP_h_sum$island = "Hawaii"
Beta_MAP_m_sum$island = "MauiNui"
Beta_MAP_sum = rbind(Beta_MAP_h_sum, Beta_MAP_o_sum, Beta_MAP_m_sum)
unique(sort(Beta_MAP_sum$island))

# Plotting relationships alien and native species and highly invaded plots
Beta_MAP_o_sum_natali$island = "Oahu"
Beta_MAP_h_sum_natali$island = "Hawaii"
Beta_MAP_m_sum_natali$island = "MauiNui"
Beta_MAP_sum_natali = rbind(Beta_MAP_h_sum_natali, Beta_MAP_o_sum_natali, Beta_MAP_m_sum_natali)
unique(sort(Beta_MAP_sum_natali$island))

coco2 = c(
  "#00AFBB", 
  "gray60", 
  "#E7B800") 

beta_map = ggplot(Beta_MAP_sum, aes(x = abs(MAP_diff), y = BetaC_mean)) +   
  geom_point(aes(color = island), size = .3, show.legend = FALSE, alpha = .2) + 
  
  geom_smooth(aes(color = island, fill = island), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  
  scale_color_manual(values = coco2) + scale_fill_manual(values = coco2)+
  ylab("mean Beta C - Native")+
  theme_set(theme_minimal(base_size = 8))
beta_map

beta_map_natali = ggplot(Beta_MAP_sum_natali, aes(x = abs(MAP_diff), y = BetaC_mean)) +   
  geom_point(aes(color = island), size = .3, show.legend = FALSE, alpha = .2) + 
  
  geom_smooth(aes(color = island, fill = island), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  
  scale_color_manual(values = coco2) + scale_fill_manual(values = coco2)+
  ylab("mean Beta C - Native")+
  theme_set(theme_minimal(base_size = 8))
beta_map_natali

# 
# ggpubr::ggarrange(beta_map, beta_map_natali, ncol = 2, nrow = 1)
# 
# pdf(file = "C:/Hawaii_project/Figures/Figure_3_20230619.pdf",   # 
#     width=4.6, # 89 mm (single column) is 3.5 inches.
#     height=2,
#     paper = "a4")
# ggpubr::ggarrange(beta_map, beta_map_natali, ncol = 2, nrow = 1)
# dev.off()
# 
# ggpubr::ggarrange(beta_map, beta_map_natali, ncol = 2, nrow = 1)

pdf(file = "C:/Hawaii_project/Figures/Figure_3_30coverage_sensit_20230714.pdf",   # 
    width=4.6, # 89 mm (single column) is 3.5 inches.
    height=2,
    paper = "a4")
ggpubr::ggarrange(beta_map, beta_map_natali, ncol = 2, nrow = 1)
dev.off()


##################################################
# Testing if the interaction is significant or not
##################################################

         # Supplementary Figure 5 #

z <- Beta_MAP_sum
#  z <- z %>% mutate(island = factor(island, levels = c("MauiNui", "Hawaii" , "Oahu")))

glm_t_0     <- glm(BetaC_mean ~ log(abs(MAP_diff)) * island, family  = Gamma(link = "log"), data = z)
glm_t_0_noi <- glm(BetaC_mean ~ abs(MAP_diff) , family  = Gamma(link = "log"), data = z)
lrtest(glm_t_0, glm_t_0_noi) # Supplementary Table 1 Beta diversity results 

q_colors =  1 
v_colors =  viridis::viridis(q_colors) 

beta_map1 <- jtools ::plot_summs(glm_t_0, scale = TRUE,  ci_level = 0.95, inner_ci_level = .85, colors = v_colors) + 
  theme_set(theme_minimal(base_size = 8))
beta_map1

pdf(file = "C:/Hawaii_project/Figures/Suppl_Fig_5_1.pdf",   # native only
    width=4.5, height=1.5, paper = "a4")
beta_map2
dev.off()

# # # 

z <-Beta_MAP_sum_natali
#  z <- z %>% mutate(island = factor(island, levels = c("MauiNui", "Hawaii" , "Oahu")))

glm_t_0     <- glm(BetaC_mean ~ log(abs(MAP_diff)) * island, family  = Gamma(link = "log"), data = z)
glm_t_0_noi <- glm(BetaC_mean ~ abs(MAP_diff) , family  = Gamma(link = "log"), data = z)
lrtest(glm_t_0, glm_t_0_noi) # Supplementary Table 1 Beta diversity results 

q_colors =  1 
v_colors =  viridis::viridis(q_colors) 

beta_map1 <- jtools ::plot_summs(glm_t_0, scale = TRUE,  ci_level = 0.95, inner_ci_level = .85, colors = v_colors) + 
  theme_set(theme_minimal(base_size = 8))
beta_map1

pdf(file = "C:/Hawaii_project/Figures/Suppl_Fig_5_2.pdf",   # aliens + native 
    width=4.5, height=1.5, paper = "a4")
beta_map2
dev.off()


