library(tidyverse)
library(ggpubr)
library(lmtest) 

setwd("C:/Hawaii_project/Hawaii/pub_code_data/data")

load("results_native_plot_20230601.RData")
load("results_all_plot_20230601.RData")   
datt_env <- read.csv("C:/Hawaii_project/Hawaii/pub_code_data/data/datt_env_20240604.csv") # ADITIONAL VARIABLES: mean annual Air temperature and geological information (substrate ages within islands) 
datt_env <- select(datt_env, PlotID, Haw_St_geo, tair_ann)

results_native_plot <-left_join(results_native_plot, datt_env, by = "PlotID") 
results_all_plot    <-left_join(results_all_plot, datt_env, by = "PlotID") 

# MODELS ALPHA
#   z <- results_native_plot
z <- results_all_plot
#   z <- filter(z, MAP > 800 & MAP < 4000) # Sub-set MAP range. Supplementary material

# z %>% group_by(geo_entity2) %>% tally()

z <- z %>%
  mutate(geo_entity2 = factor(geo_entity2, levels = c("Maui Nui", "Hawai'i Island", "O'ahu Island")))

# INTERACTION ISLAND WITH PRECIPITATION ----
glm_t_0 <- glm(tax_q0 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z) # interaction
glm_t_1 <- glm(tax_q1 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_t_2 <- glm(tax_q2 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_p_0 <- glm(phy_q0 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_p_1 <- glm(phy_q1 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_p_2 <- glm(phy_q2 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_f_0 <- glm(FD_q0  ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_f_1 <- glm(FD_q1  ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_f_2 <- glm(FD_q2  ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
# 
# glm_t_0_noi <- glm(tax_q0 ~ log(MAP) , family  = Gamma(link = "log"), data = z)         # Without interaction
# glm_t_1_noi <- glm(tax_q1 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
# glm_t_2_noi <- glm(tax_q2 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
# glm_p_0_noi <- glm(phy_q0 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
# glm_p_1_noi <- glm(phy_q1 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
# glm_p_2_noi <- glm(phy_q2 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
# glm_f_0_noi <- glm(FD_q0  ~ log(MAP) , family  = Gamma(link = "log"), data = z)
# glm_f_1_noi <- glm(FD_q1  ~ log(MAP) , family  = Gamma(link = "log"), data = z)
# glm_f_2_noi <- glm(FD_q2  ~ log(MAP) , family  = Gamma(link = "log"), data = z)

# ADDING TEMPERATURE 
glm_t_0_t <- glm(tax_q0 ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z) # interaction
glm_t_1_t <- glm(tax_q1 ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_t_2_t <- glm(tax_q2 ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_p_0_t <- glm(phy_q0 ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_p_1_t <- glm(phy_q1 ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_p_2_t <- glm(phy_q2 ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_f_0_t <- glm(FD_q0  ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_f_1_t <- glm(FD_q1  ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_f_2_t <- glm(FD_q2  ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)

## ADDING TEMPERATURE and substrate age 
glm_t_0_tg <- glm(tax_q0 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo, family  = Gamma(link = "log"), data = z) 
glm_t_1_tg <- glm(tax_q1 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo, family  = Gamma(link = "log"), data = z)
glm_t_2_tg <- glm(tax_q2 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo, family  = Gamma(link = "log"), data = z)
glm_p_0_tg <- glm(phy_q0 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo, family  = Gamma(link = "log"), data = z)
glm_p_1_tg <- glm(phy_q1 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo, family  = Gamma(link = "log"), data = z)
glm_p_2_tg <- glm(phy_q2 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo, family  = Gamma(link = "log"), data = z)
glm_f_0_tg <- glm(FD_q0  ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo, family  = Gamma(link = "log"), data = z)
glm_f_1_tg <- glm(FD_q1  ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo, family  = Gamma(link = "log"), data = z)
glm_f_2_tg <- glm(FD_q2  ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo, family  = Gamma(link = "log"), data = z)


# # Test of interaction significance 
# # Likelihood ratio tests compares the goodness of fit of two statistical models 
# # If Pr(>Chisq) is smaller than 0.05 then model with interaction is better
# lrtest(glm_t_0, glm_t_0_noi) ; lrtest(glm_t_2, glm_t_2_noi) # models with interaction are better!
# lrtest(glm_p_0, glm_p_0_noi) ; lrtest(glm_p_2, glm_p_2_noi)
# lrtest(glm_f_0, glm_f_0_noi) ; lrtest(glm_f_2, glm_f_2_noi)
# 
# Test model performance after adding temperature as explanatory variable
AIC(glm_t_0, glm_t_0_t, glm_t_0_tg) ; AIC(glm_t_2, glm_t_2_t, glm_t_0_tg) # Models with temperature are better!
AIC(glm_p_0, glm_p_0_t, glm_t_0_tg) ; AIC(glm_p_2, glm_p_2_t, glm_t_0_tg)
AIC(glm_f_0, glm_f_0_t, glm_t_0_tg) ; AIC(glm_f_2, glm_f_2_t, glm_t_0_tg)
# 
# jtools::summ(glm_p_2)
# 
# v_colors =c("#440154FF","#481467", "#287d8e", "#1f968b", "#dde318", "#fde725") # https://waldyrious.net/viridis-palette-generator/
# coco = c( "#00AFBB", "gray60", "#E7B800")

# Plots PREC only
# glm_log = jtools ::plot_summs(glm_t_0,  glm_t_2, glm_p_0, glm_p_2, glm_f_0, glm_f_2,   #  https://jtools.jacob-long.com/articles/summ.html
#                               scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 , 
#                               colors = v_colors,legend.title = "All") +  theme(legend.position = "none")
# glm_log
# 
# 
# legend1 = jtools ::plot_summs(glm_t_0,  glm_t_2, glm_p_0, glm_p_2, glm_f_0, glm_f_2,
#                               scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 , 
#                               colors = v_colors, model.names = c("Taxonomic (q=0)",    "Taxonomic (q=2)",
#                                                                  "Phylogenetic (q=0)", "Phylogenetic (q=2)",
#                                                                  "Functional (q=0)",   "Functional (q=2)") ) +
#   theme_set(theme_minimal(base_size = 8))

# Plots Adding TEMP 
glm_log = jtools ::plot_summs(glm_t_0_t,  glm_t_2_t, glm_p_0_t, glm_p_2_t, glm_f_0_t, glm_f_2_t,   #  https://jtools.jacob-long.com/articles/summ.html
                              scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 , 
                              colors = v_colors,legend.title = "All") +  theme(legend.position = "none")
glm_log

###############################################################################
#    glm_log_native  <- glm_log  # Maui and Oahu
#    glm_log_native1 <- glm_log  # Hawaii and Oahu

#    glm_log_all <- glm_log
#    glm_log_all1 <- glm_log

ggpubr::ggarrange(glm_log_native, glm_log_native1,
                  glm_log_all, glm_log_all1 ) # , legend1, ncol = 2, nrow = 3)


pdf(file = "C:/Hawaii_project/Figures/Suppl_Fig_XX_20240607_temp.pdf",
    width=10, # 89 mm (single column) is 3.5 inches.
    height=9)
ggpubr::ggarrange(glm_log_native, glm_log_native1,
                  glm_log_all, glm_log_all1)
dev.off()


###############################################################################
# Test effect of soil subatrate in island diversity -
#   z <- results_native_plot
#   z <- results_all_plot

# Oahu
isl = filter(z, geo_entity2 == "O'ahu Island")
glm_t_0_tg <- glm(tax_q0 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl) 
glm_t_2_tg <- glm(tax_q2 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_p_0_tg <- glm(phy_q0 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_p_2_tg <- glm(phy_q2 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_f_0_tg <- glm(FD_q0  ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_f_2_tg <- glm(FD_q2  ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
Oahu_coef = jtools ::plot_summs(tax_q0 = glm_t_0_tg,  tax_q2 = glm_t_2_tg, 
                                ph_q0 = glm_p_0_tg,   ph_q2 = glm_p_2_tg, 
                                fd_q0 = glm_f_0_tg,   fd_q2 = glm_f_2_tg,
                                scale = TRUE,  ci_level = 0.95, inner_ci_level = .85, legend.title = "Oahu_coef" ) 

# Maui
isl = filter(z, geo_entity2 == "Maui Nui")
glm_t_0_tg <- glm(tax_q0 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl) 
glm_t_2_tg <- glm(tax_q2 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_p_0_tg <- glm(phy_q0 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_p_2_tg <- glm(phy_q2 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_f_0_tg <- glm(FD_q0  ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_f_2_tg <- glm(FD_q2  ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
Maui_coef = jtools ::plot_summs(tax_q0 = glm_t_0_tg,  tax_q2 = glm_t_2_tg, 
                                ph_q0 = glm_p_0_tg,   ph_q2 = glm_p_2_tg, 
                                fd_q0 = glm_f_0_tg,   fd_q2 = glm_f_2_tg,
                                scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 , legend.title = "Maui_coef" ) 

# Hawaii
isl = filter(z, geo_entity2 == "Hawai'i Island")
glm_t_0_tg <- glm(tax_q0 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl) 
glm_t_2_tg <- glm(tax_q2 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_p_0_tg <- glm(phy_q0 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_p_2_tg <- glm(phy_q2 ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_f_0_tg <- glm(FD_q0  ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
glm_f_2_tg <- glm(FD_q2  ~ log(MAP) * Haw_St_geo + log(tair_ann), family  = Gamma(link = "log"), data = isl)
Hawaii_coef = jtools ::plot_summs(tax_q0 = glm_t_0_tg,  tax_q2 = glm_t_2_tg, 
                                  ph_q0 = glm_p_0_tg,   ph_q2 = glm_p_2_tg, 
                                  fd_q0 = glm_f_0_tg,   fd_q2 = glm_f_2_tg,
                                  scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 , legend.title = "Hawaii_coef" ) 

#    native <- ggpubr::ggarrange(Oahu_coef, Maui_coef, Hawaii_coef, ncol = 1, nrow = 3) 

#    native_aliens <- ggpubr::ggarrange(Oahu_coef, Maui_coef, Hawaii_coef, ncol = 1, nrow = 3) 

pdf(file = "C:/Hawaii_project/Figures/Suppl_Fig_9_temp_soil_20240611.pdf",
    width=10, # 89 mm (single column) is 3.5 inches.
    height=12)
# ggpubr::ggarrange(native, native_aliens)
dev.off()



