library(tidyverse)
library(ggpubr)
library(lmtest) 

# Load data
setwd("G:/My Drive/Hawaii_project/Hawaii/pub_code_data/")

load("data/results_native_plot_20230601.RData")# ALPHA
load("data/results_all_plot_20230601.RData")   # ALPHA

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

# ******************************************************************************
# Figure S1
# ******************************************************************************

#   z <- alpha_native
#   z <- alpha_nativealien

tx_q1 <- ggplot(z, aes(x =MAP, y = tax_q1)) +
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) +
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE,
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+
  xlab("")+
  ylab("(q=1)")+
  theme_set(theme_minimal(base_size = 8))

tx_q2 <- ggplot(z, aes(x =MAP, y = tax_q2)) +
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) +
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE,
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+
  xlab("")+
  ylab("(q=2)")+
  theme_set(theme_minimal(base_size = 8))

legend <- ggplot(z, aes(x =MAP, y = tax_q2)) +
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = TRUE,
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) + scale_color_manual(values = coco) + scale_fill_manual(values = coco)
# # #
glm_t_1_ptg_na <- glm(tax_q1 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_native)
glm_t_2_ptg_na <- glm(tax_q2 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_native)

glm_t_1_ptg_naali <- glm(tax_q1 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_nativealien)
glm_t_2_ptg_naali <- glm(tax_q2 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_nativealien)

glm_n = jtools ::plot_summs(glm_t_1_ptg_na,   glm_t_2_ptg_na,
                            scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 ,
                            colors = v_colors, legend.title = "Native") 
glm_na = jtools ::plot_summs(glm_t_1_ptg_naali,  glm_t_2_ptg_naali, 
                             scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 ,
                             colors = v_colors, legend.title = "Native and aliens") 

nat_natali_alpha <- ggpubr::ggarrange(glm_n, glm_na)
nat_natali_alpha

# #
alpha_native <- alpha_native %>%   # changing levels 
  mutate(geo_entity2 = factor(geo_entity2, levels = c("Hawai'i Island", "O'ahu Island", "Maui Nui")))
alpha_nativealien <- alpha_nativealien %>%
  mutate(geo_entity2 = factor(geo_entity2, levels = c("Hawai'i Island", "O'ahu Island", "Maui Nui")))

glm_t_1_ptg_na <- glm(tax_q1 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_native)
glm_t_2_ptg_na <- glm(tax_q2 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_native)

glm_t_1_ptg_naali <- glm(tax_q1 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_nativealien)
glm_t_2_ptg_naali <- glm(tax_q2 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_nativealien)

glm_n = jtools ::plot_summs(glm_t_1_ptg_na,   glm_t_2_ptg_na,
                            scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 ,
                            colors = v_colors, legend.title = "Native") 
glm_na = jtools ::plot_summs(glm_t_1_ptg_naali,  glm_t_2_ptg_naali, 
                             scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 ,
                             colors = v_colors, legend.title = "Native and aliens") 

nat_natali_alpha_1 <- ggpubr::ggarrange(glm_n, glm_na)
nat_natali_alpha_1

ggpubr::ggarrange(nat_natali_alpha, nat_natali_alpha_1, nrow = 2, ncol = 1)

# pdf(file = "G:/My Drive/Hawaii_project/Figures/Fig_S1_eh_20241111.pdf", 
    width=15,  height=6, paper = "a4r")
ggpubr::ggarrange(nat_natali_alpha, nat_natali_alpha_1, nrow = 2, ncol = 1)
dev.off()

# ******************************************************************************
# Figure S2
# ******************************************************************************

alpha_native      <- filter(alpha_native, MAP > 800 & MAP < 4000) 
alpha_nativealien <- filter(alpha_nativealien, MAP > 800 & MAP < 4000) 

glm_t_0_ptg_na    <- glm(tax_q0 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_native) 
glm_t_0_ptg_naali <- glm(tax_q0 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_nativealien) 

alpha_native <- alpha_native %>%   # changing levels 
  mutate(geo_entity2 = factor(geo_entity2, levels = c("Hawai'i Island", "O'ahu Island", "Maui Nui")))
alpha_nativealien <- alpha_nativealien %>%
  mutate(geo_entity2 = factor(geo_entity2, levels = c("Hawai'i Island", "O'ahu Island", "Maui Nui")))

glm_t_0_ptg_na_    <- glm(tax_q0 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_native) 
glm_t_0_ptg_naali_ <- glm(tax_q0 ~ log(MAP) * geo_entity2 + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = alpha_nativealien) 

glm_n_truncated = jtools ::plot_summs(glm_t_0_ptg_na_,  
                            scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 ,
                            colors = v_colors, legend.title = "Native") 
glm_nat_truncated = jtools ::plot_summs(glm_t_0_ptg_naali_,
                                      scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 ,
                                      colors = v_colors, legend.title = "Native") 

nat_natali_alpha_trunc <- ggpubr::ggarrange(glm_n_truncated, glm_nat_truncated)
nat_natali_alpha_trunc

# pdf(file = "G:/My Drive/Hawaii_project/Figures/Fig_S2_20241111.pdf", 
    width=7,  height=2, paper = "a4")
nat_natali_alpha_trunc
dev.off()

# ******************************************************************************
# Figure S3
# ******************************************************************************
# Sensitivity analysis using maximum coverage 30% - beta diversity
load('data/results_Beta_MAP_03_sensit.RData')

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

# ******************************************************************************
# Figure S4

# Fig S4 c and d: Substrate age and MAP
a = ggplot(alpha_native, aes(x = Haw_St_geo_1 , y =MAP)) +  
  geom_point(aes(color = geo_entity2), size = 2, alpha = 0.4, show.legend = TRUE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = TRUE,
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+ 
  xlab("Substrate age")+
  ylab("MAP")+ 
  theme_set(theme_minimal(base_size = 8))

b = ggplot(alpha_nativealien, aes(x = Haw_St_geo_1 , y =MAP)) +  
  geom_point(aes(color = geo_entity2), size = 2, alpha = 0.4, show.legend = TRUE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = TRUE,
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+ 
  xlab("Substrate age")+
  ylab("MAP")+ 
  theme_set(theme_minimal(base_size = 8))

ggpubr::ggarrange(a, b)

# Fig S4 e and f: Substrate age and diversity
nat <- ggplot(alpha_native, aes(x = Haw_St_geo_1, y = tax_q0)) +  
  geom_point(aes(color = geo_entity2), size = 1, show.legend = TRUE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = TRUE, alpha = 0.3,
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .5) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+ 
  xlab("Substrate age (million year ago)")+
  ylab("Taxonomic diversity (q=0)")+ 
  theme_set(theme_minimal(base_size = 8))
nat

natali <- ggplot(alpha_nativealien, aes(x = Haw_St_geo_1, y = tax_q0)) +  
  geom_point(aes(color = geo_entity2), size = 1, show.legend = TRUE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = TRUE, alpha = 0.3,
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .5) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+ 
  xlab("Substrate age (million year ago)")+
  ylab("Taxonomic diversity (q=0)")+ 
  theme_set(theme_minimal(base_size = 8))
natali

ggpubr::ggarrange(nat,natali)

# ******************************************************************************
# TABLE S1
# ******************************************************************************

# TEST OF THE INTERACTION MAP*Island 

#   z <- alpha_native
#   z <- alpha_nativealien

# With interaction
glm_t_0_ptg <- glm(tax_q0 ~ log(MAP) * geo_entity2  + Haw_St_geo_1 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_t_1_ptg <- glm(tax_q1 ~ log(MAP) * geo_entity2  + Haw_St_geo_1 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_t_2_ptg <- glm(tax_q2 ~ log(MAP) * geo_entity2  + Haw_St_geo_1 + tair_ann, family  = Gamma(link = "log"), data = z)
# Without interaction
glm_t_0_ptg_noi <- glm(tax_q0 ~ log(MAP)  + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = z)
glm_t_1_ptg_noi <- glm(tax_q1 ~ log(MAP)  + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = z)
glm_t_2_ptg_noi <- glm(tax_q2 ~ log(MAP)  + tair_ann + Haw_St_geo_1, family  = Gamma(link = "log"), data = z)
# Likelihood ratio tests compares the goodness of fit of two statistical models. If Pr(>Chisq) is smaller than 0.05 then model with interaction is better
lrtest(glm_t_0_ptg, glm_t_0_ptg_noi) # Models with interaction are better!
lrtest(glm_t_1_ptg, glm_t_1_ptg_noi)
lrtest(glm_t_2_ptg, glm_t_2_ptg_noi)

# ******************************************************************************
# TABLE S2
# ******************************************************************************
# TEST OF ADDING/REMOVING substrate age and MAT. Do these variables help explaining more?

glm_t_0_ptg <- glm(tax_q0 ~ log(MAP) * geo_entity2  + Haw_St_geo_1 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_t_1_ptg <- glm(tax_q1 ~ log(MAP) * geo_entity2  + Haw_St_geo_1 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_t_2_ptg <- glm(tax_q2 ~ log(MAP) * geo_entity2  + Haw_St_geo_1 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_t_0_pt <- glm(tax_q0 ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_t_1_pt <- glm(tax_q1 ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_t_2_pt <- glm(tax_q2 ~ log(MAP) * geo_entity2 + tair_ann, family  = Gamma(link = "log"), data = z)
glm_t_0_p  <- glm(tax_q0 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_t_1_p  <- glm(tax_q1 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_t_2_p  <- glm(tax_q2 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)

AIC(glm_t_0_ptg, glm_t_0_pt, glm_t_0_p) # WRITE including both temp and substrate age helps explain the viability of diversity better!
AIC(glm_t_1_ptg, glm_t_1_pt, glm_t_1_p)
AIC(glm_t_2_ptg, glm_t_2_pt, glm_t_2_p)

