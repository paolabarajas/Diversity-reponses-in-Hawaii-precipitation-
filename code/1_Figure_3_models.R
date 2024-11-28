library(dplyr)
library(ggplot2)
library(ggpubr)
library(lmtest)

# Load data
setwd("G:/My Drive/Hawaii_project/Hawaii/pub_code_data/")

load("data/results_native_plot_20230601.RData")# ALPHA
load("data/results_all_plot_20230601.RData")   

load("data/Beta_MAP_results.Rdata")# BETA

datt_env <- read.csv( # ADITIONAL VARIABLES: mean annual Air temperature and geological information (substrate age) 
  "G:/My Drive/Hawaii_project/Hawaii/pub_code_data/data/datt_env_20240604.csv")
datt_env <- select(datt_env, PlotID, Haw_St_geo, tair_ann)
datt_env <- datt_env %>%
  mutate(Haw_St_geo_1 = recode(Haw_St_geo,
                               "1" = "0.0001" , "2" = "0.000275" ,
                               "3" = "0.00112", "4" = "0.0022" ,
                               "5" = "0.004" ,  "6" = "0.0075" ,
                               "7" = "0.02" ,   "8" = "0.04", 
                               "9" = "0.09" ,   "10" = "0.46", 
                               "11" = "0.89",   "12" = "1.5" ,
                               "13" = "3" ,     "14" = "5" ))
datt_env$Haw_St_geo_1 <- as.numeric(datt_env$Haw_St_geo_1)

coco     = c("#00AFBB", "gray60", "#E7B800")
v_colors = c("#440154FF","#287d8e","#dde318") # https://waldyrious.net/viridis-palette-generator/

alpha_native      <-left_join(results_native_plot, datt_env, by = "PlotID") 
alpha_nativealien <-left_join(results_all_plot, datt_env, by = "PlotID") 
beta_native      <-Beta_MAP_sum 
beta_nativealien <-Beta_MAP_sum_natali

# Re-run models coef plotting changing levels 
# alpha_native      <- alpha_native %>%  mutate(geo_entity2 = factor(geo_entity2, levels = c("Maui Nui", "Hawai'i Island", "O'ahu Island")))
# alpha_nativealien <- alpha_nativealien %>%  mutate(geo_entity2 = factor(geo_entity2, levels = c("Maui Nui", "Hawai'i Island", "O'ahu Island")))
# beta_native       <- beta_native %>%  mutate(island = factor(island, levels = c("MauiNui", "Hawaii", "Oahu")))
# beta_nativealien  <- beta_nativealien %>%  mutate(island = factor(island, levels = c("MauiNui", "Hawaii", "Oahu")))

# Re-run models coef plotting changing levels 
alpha_native      <- alpha_native      %>%  mutate(geo_entity2 = factor(geo_entity2, levels = c("Hawai'i Island", "O'ahu Island", "Maui Nui")))
alpha_nativealien <- alpha_nativealien %>%  mutate(geo_entity2 = factor(geo_entity2, levels = c("Hawai'i Island", "O'ahu Island", "Maui Nui")))
beta_native       <- beta_native       %>%  mutate(island = factor(island, levels = c("Hawaii", "Oahu", "MauiNui")))
beta_nativealien  <- beta_nativealien  %>%  mutate(island = factor(island, levels = c("Hawaii", "Oahu", "MauiNui")))

# ******************************************************************************
# *************************** Figure 3  ****************************************

# # # # MODELS ALPHA 
glm_t_0_ptg_na    <- glm(tax_q0 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_native) 
glm_t_0_ptg_naali <- glm(tax_q0 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_nativealien) 

glm_n = jtools ::plot_summs(glm_t_0_ptg_na,   # https://jtools.jacob-long.com/articles/summ.html
                             scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 ,
                             colors = v_colors,legend.title = "Taxonomic q=0 native") +  theme(legend.position = "none")
glm_na = jtools ::plot_summs(glm_t_0_ptg_naali,   
                              scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 ,
                              colors = v_colors,legend.title = "Taxonomic q=0 native and aliens") +  theme(legend.position = "none")
nat_natali_alpha <- ggpubr::ggarrange(glm_n, glm_na)
nat_natali_alpha

# # # # MODELS BETA 
glm_beta_ptg_na    <- glm(BetaC_mean ~ log(abs(MAP_diff)) * island, family  = Gamma(link = "log"), data = beta_native)
glm_beta_ptg_naali <- glm(BetaC_mean ~ log(abs(MAP_diff)) * island, family  = Gamma(link = "log"), data = beta_nativealien)

glm_n <- jtools ::plot_summs(glm_beta_ptg_na, scale = TRUE,  ci_level = 0.95, inner_ci_level = .85, colors = "#26828e") +
  theme(legend.position = "none")
glm_na <- jtools ::plot_summs(glm_beta_ptg_naali, scale = TRUE,  ci_level = 0.95, inner_ci_level = .85, colors = "#26828e") +
  theme(legend.position = "none")

nat_natali_beta <- ggpubr::ggarrange(glm_n, glm_na)
nat_natali_beta

# ALPHA AND BETA MODEL COEFICIENTS
alpha_beta_models_co <- ggpubr::ggarrange(nat_natali_alpha, nat_natali_beta, nrow= 2)
alpha_beta_models_co

# pdf(file = "G:/My Drive/Hawaii_project/Figures/Fig_3_alpha_beta_models_20241026.pdf",   # levels: "Maui Nui", "Hawai'i Island", "O'ahu Island", 
#     width=12,  height=4, paper = "a4")
# alpha_beta_models_co
# dev.off()

pdf(file = "G:/My Drive/Hawaii_project/Figures/Fig_3_alpha_beta_models_20241026_1.pdf", # levels: "Hawai'i Island", "O'ahu Island", "Maui Nui"
    width=12,  height=4, paper = "a4")
alpha_beta_models_co
dev.off()


