library(dplyr)
library(ggplot2)
library(iNEXT.3D)
library(ggpubr)

setwd("G:/My Drive/Hawaii_project/Hawaii/pub_code_data/data")

# ******************************************************************************
# LOAD DATA

load("datt_20230419.RData") 

length(unique(datt$Scientific_name)) # 142
length(unique(datt_nat$Scientific_name)) # 79 native species, i.e., 63 alien species

tempo <- dplyr::summarize(group_by(datt, Scientific_name, Native_Status))
tempo %>%
  group_by(Native_Status) %>%
  tally()

tempo0 <- dplyr::filter(tempo, Native_Status == "native" )
tempo1 <- dplyr::summarize(group_by(datt_nat, Scientific_name, Native_Status))
setdiff(tempo0$Scientific_name, tempo1$Scientific_name) 

length(unique(datt$Scientific_name)) # 142

# Native or all species
# Run native species only use ** datt_nat ***
# Run with all species including highly invaded plots use ** datt ***

#       data <- datt_nat       # native species only, plot filter to avoid highly invaded ones
#       data <- datt

# Plot level aggregation
min_ind <- 15 # minimum amount of individuals value for plots used for metrics

tmp           <- dplyr::summarize(group_by(data, PlotID,
                                           Scientific_name),Abundance=sum(Abundance))
tmp$Abundance <- as.numeric(tmp$Abundance)

Abun_p        <- pivot_wider(tmp, id_cols = Scientific_name, names_from = PlotID,
                                values_from = Abundance, values_fill = list(Abundance=0))
Abun_p        <- tibble::column_to_rownames(Abun_p, var = "Scientific_name") # columns are plots
Abun_p = Abun_p[,colSums(Abun_p) > min_ind]  # 306 plots min_ind = 10 --- 262 plot with 17 ind.

Abun_p$tmp <- row.names(Abun_p)
Abun_p$tmp <- gsub(" ", "_", Abun_p$tmp)
row.names(Abun_p) <- Abun_p$tmp
Abun_p$tmp <- NULL

# Cmax <- apply(Abun_p, 2, function(x)
#   iNEXT.3D:::Coverage(x, 'abundance', 2*sum(x))) %>% min %>% round(., 4) # I am using maximum coverage possible
Cmax = 0.8

# ******************************************************************************
# DIVERSITY METRIC CALCULATION

# - Taxonomic diversity
TD_est <- estimate3D(data = Abun_p, diversity = 'TD', q = c(0, 1, 2), datatype = 'abundance', base = 'coverage',
                     level = Cmax, nboot = 2)

out_TD_est <- pivot_wider(TD_est, id_cols = Assemblage, names_from = Order.q, values_from = qD)
out_TD_est <- data.frame(PlotID = out_TD_est$Assemblage,
                         tax_q0 = out_TD_est$`0`,
                         tax_q1 = out_TD_est$`1`,
                         tax_q2 = out_TD_est$`2`)

# Organizing diversity metrics
datt_pl_a <- dplyr::summarize(group_by(datt, PlotID, geo_entity2, Study),
                              MAP = max(MAP),
                              Abundance=sum(Abundance))

out_tmp1 <-plyr::join(datt_pl_a, # Adds plots info, i.e., MAP, island, etc  # CHECK THIS!!!!!!!!
                       out_tmp , by = "PlotID")

out_results = out_tmp1 %>% filter(!is.na(tax_q0))  # # CHECK THIS!!!!!!!!

out_results %>%
  group_by(geo_entity2) %>%
  tally()

# Saving results
# 
# results_native_plot <- out_results
#   save(results_native_plot, file= "results_native_plot_20230601.RData")
# 
# results_all_plot <- out_results
#   save(results_all_plot, file= "results_all_plot_20230601.RData")

# ******************************************************************************
# *************************** Figure 2  ****************************************

coco = c( "#00AFBB", "gray60", "#E7B800")

load("results_native_plot_20230601.RData")
load("results_all_plot_20230601.RData")   

# ADITIONAL VARIABLES: mean annual Air temperature and geological information (substrate ages within islands) 
datt_env <- read.csv("G:/My Drive/Hawaii_project/Hawaii/pub_code_data/data/datt_env_20240604.csv")
datt_env <- select(datt_env, PlotID, Haw_St_geo, tair_ann)

results_native_plot <-left_join(results_native_plot, datt_env, by = "PlotID") 
results_all_plot    <-left_join(results_all_plot, datt_env, by = "PlotID") 

results_native_plot %>% group_by(geo_entity2) %>% tally()
results_all_plot %>% group_by(geo_entity2) %>% tally()

##################
# ALPHA DIVERSITY 
##################

tx_q0_na <- ggplot(results_native_plot, aes(x = MAP, y = tax_q0)) +  
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE, 
              method = glm,  # To avoid negative values to be predicted we used GLM  
              method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+ 
  xlab("")+
  ylab("Taxonomic diversity (q=0) native")+ 
  theme_set(theme_minimal(base_size = 8))

tx_q0_natali <- ggplot(results_all_plot, aes(x = MAP, y = tax_q0)) +  
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+ 
  xlab("")+
  ylab("Taxonomic diversity (q=0) native+aliens")+ 
  theme_set(theme_minimal(base_size = 8))

legend <- ggplot(results_native_plot, aes(x =MAP, y = tax_q2)) +  
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = TRUE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) + scale_color_manual(values = coco) + scale_fill_manual(values = coco)

alpha = ggpubr::ggarrange(tx_q0_na,  tx_q0_natali, ncol = 2, nrow = 1)
alpha

##################
# BETA DIVERSITY 
##################

# load calculated beta pairs
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

Beta_MAP_o_sum$island = "Oahu" # Relationships native species only and relative uninvaded plots
Beta_MAP_h_sum$island = "Hawaii"
Beta_MAP_m_sum$island = "MauiNui"
Beta_MAP_sum = rbind(Beta_MAP_h_sum, Beta_MAP_o_sum, Beta_MAP_m_sum)
unique(sort(Beta_MAP_sum$island))

Beta_MAP_o_sum_natali$island = "Oahu" # Relationships alien and native species and highly invaded plots
Beta_MAP_h_sum_natali$island = "Hawaii"
Beta_MAP_m_sum_natali$island = "MauiNui"
Beta_MAP_sum_natali = rbind(Beta_MAP_h_sum_natali, Beta_MAP_o_sum_natali, Beta_MAP_m_sum_natali)
unique(sort(Beta_MAP_sum_natali$island))

# save(Beta_MAP_sum, Beta_MAP_sum_natali, file =  "Beta_MAP_results.Rdata")
load("Beta_MAP_results.Rdata") 

beta_map = ggplot(Beta_MAP_sum, aes(x = abs(MAP_diff), y = BetaC_mean)) +   
  geom_point(aes(color = island), size = .3, show.legend = FALSE, alpha = .2) + 
  
  geom_smooth(aes(color = island, fill = island), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+
  ylab("mean Beta C - Native")+
  theme_set(theme_minimal(base_size = 8))
beta_map

beta_map_natali = ggplot(Beta_MAP_sum_natali, aes(x = abs(MAP_diff), y = BetaC_mean)) +   
  geom_point(aes(color = island), size = .3, show.legend = FALSE, alpha = .2) + 
  
  geom_smooth(aes(color = island, fill = island), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+
  ylab("mean Beta C - Native+aliens")+
  theme_set(theme_minimal(base_size = 8))
beta_map_natali

beta = ggpubr::ggarrange(beta_map, beta_map_natali, ncol = 2, nrow = 1)

# Exports Figure 2

alpha_beta = ggpubr::ggarrange(alpha, beta, legend, ncol = 1, nrow = 3)
alpha_beta

pdf(file = "G:/My Drive/Hawaii_project/Figures/Fig_2_20241111.pdf",
    width=6.7, # 89 mm (single column) is 3.5 inches.
    height=7.8,
    paper = "a4")
alpha_beta
dev.off()
