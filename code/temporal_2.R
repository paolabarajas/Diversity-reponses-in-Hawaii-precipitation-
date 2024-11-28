library(ape)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(devtools)
library(tidyr)
library(lmtest) 
library(mgcv)

######################################## BORRAR
# TEST OF INTERACTION ISLAND WITH PRECIPITATION ----

#   z <- results_native_plot
#   z <- results_all_plot
#   z <- filter(z, MAP > 800 & MAP < 4000) # for Sub-set MAP range

z <- z %>%
  mutate(geo_entity2 = factor(geo_entity2, levels = c("Maui Nui", "Hawai'i Island", "O'ahu Island")))

# interaction
glm_t_0 <- glm(tax_q0 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_t_1 <- glm(tax_q1 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_t_2 <- glm(tax_q2 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)

# Without interaction
glm_t_0_noi <- glm(tax_q0 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_t_1_noi <- glm(tax_q1 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_t_2_noi <- glm(tax_q2 ~ log(MAP) , family  = Gamma(link = "log"), data = z)

# Test of interaction significance 
# Likelihood ratio tests compares the goodness of fit of two statistical models 
# So, if Pr(>Chisq) is smaller than 0.05 then model with interaction is better
lrtest(glm_t_0, glm_t_0_noi) #  model with interaction is better!
lrtest(glm_t_2, glm_t_2_noi) 

jtools::summ(glm_t_0)

v_colors =c("#440154FF","#287d8e","#dde318") # https://waldyrious.net/viridis-palette-generator/

# https://jtools.jacob-long.com/articles/summ.html
glm_log = jtools ::plot_summs(glm_t_0,  glm_t_1, glm_t_2, 
                              scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 , 
                              colors = v_colors,legend.title = "All") +  theme(legend.position = "none")
glm_log


legend1 = jtools ::plot_summs(glm_t_0, glm_t_1,  glm_t_2, 
                              scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 , 
                              colors = v_colors, model.names = c("Taxonomic (q=0)", "Taxonomic (q=1)", "Taxonomic (q=2)") ) +
  theme_set(theme_minimal(base_size = 8))

# ******************************************************************************
#  glm_log_native  <- glm_log
#  glm_log_native1 <- glm_log

#  glm_log_all <- glm_log
#  glm_log_all1 <- glm_log

ggpubr::ggarrange(glm_log_native, glm_log_native1,
                  glm_log_all, glm_log_all1, legend1, ncol = 2, nrow = 3)

# pdf(file = "C:/Hawaii_project/Figures/Fig_3_Supp_20230425.pdf",
#     width=10, # 89 mm (single column) is 3.5 inches.
#     height=16)
# ggpubr::ggarrange(glm_log_native, glm_log_native1,
#                   glm_log_all, glm_log_all1,legend1, ncol = 2, nrow = 3)
# dev.off()

pdf(file = "G:/My Drive/Hawaii_project/Figures/Fig_6_Supp_20230425.pdf",
    width=10, # 89 mm (single column) is 3.5 inches.
    height=16)
ggpubr::ggarrange(glm_log_native, glm_log_native1,
                  glm_log_all, glm_log_all1,legend1, ncol = 2, nrow = 3)
dev.off()

######################################## BORRAR

#   z <- filter(z, MAP > 800 & MAP < 4000) # Sub-set MAP range. Supplementary material



# ******************************************************************************
#                    FIRST REVISIONS - work Oct 2024                           *
# ******************************************************************************

#   z <- results_native_plot
#   z <- results_all_plot
#   z <- filter(z, MAP > 800 & MAP < 4000) # Sub-set MAP range. Supplementary material
# Check the global relationship. i.e., all islands merged
# RESULTS: both global relationships are almost cero. this is bc there are many plots
# with very low diversity is super wer areas, mosly coming from hawaii. 
nat <- ggplot(z, aes(x = MAP, y = tax_q0)) +  
  geom_point(size = .3, show.legend = FALSE) + 
  geom_smooth(show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  xlab("")+ ylab("Taxonomic diversity (q=0)")+ #ylim(0,15)+ # for native only: ylim(0,15) # for all ylim(0,17)
  theme_set(theme_minimal(base_size = 8))
native <- glm(tax_q0 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
summary(native)
ali <- ggplot(z, aes(x = MAP, y = tax_q0)) +  
  geom_point(size = .3, show.legend = FALSE) + 
  geom_smooth(show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  xlab("")+ ylab("Taxonomic diversity (q=0)")+ #ylim(0,15)+ # for native only: ylim(0,15) # for all ylim(0,17)
  theme_set(theme_minimal(base_size = 8))
alien <- glm(tax_q0 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
summary(alien)

pdf(file = "G:/My Drive/Hawaii_project/Figures/20241008_global_relatioship.pdf",
    width=5, # 89 mm (single column) is 3.5 inches.
    height=2)
ggpubr::ggarrange(nat, ali, ncol = 2)
dev.off()

# Check the temp and elevation ranges of the plots samples and included on our study. 
range(na.omit(datt$Elev_m))
range(na.omit(datt$MAT))
range(na.omit(datt_nat$Elev_m))
range(na.omit(datt_nat$MAT))

# Test for non-linear and non-monotonic relationship betwen MAP and diversity
nat    <- results_native_plot
natali <- results_all_plot

# I can use a correlation test, specifically for non linear correlations 
# https://github.com/ProcessMiner/nlcor
# https://towardsdatascience.com/estimating-non-linear-correlation-in-r-62c6571cb1db
install_github("ProcessMiner/nlcor")
library(nlcor)

c <- nlcor(nat$MAP, nat$tax_q0, plt = T)
c$cor.estimate

nat1 = filter(nat, geo_entity2 == "O'ahu Island"   ) # "O'ahu Island"   "Hawai'i Island" "Maui Nui"  
c <- nlcor(nat1$MAP, nat1$tax_q0, plt = T)
c$cor.estimate

# I can use GAM, generalized additive model ( I DID NOT DO THAT)
# testing whether or not that functional estimate is constant, 
# https://stats.stackexchange.com/questions/35893/how-do-i-test-a-nonlinear-association
g = gam(tax_q0 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = nat)
summary(g)
plot(g,  all.terms = TRUE) 

# I think the idea of substrate age would be that you do not 
# need to include island anymore, as island co-varies with 
# the substrate age (right?). In that way, your age effect is 
# more explicit than when comparing 3 islands of different age. 
# I understand that this may take too much time and re-analysis,
# but something to keep in mind perhaps for revisions.


##################################################
# Testing if the interaction is significant or not BETA
##################################################
z <- Beta_MAP_sum
glm_t_0     <- glm(BetaC_mean ~ log(abs(MAP_diff)) * island, family  = Gamma(link = "log"), data = z)
glm_t_0_noi <- glm(BetaC_mean ~ abs(MAP_diff) , family  = Gamma(link = "log"), data = z)
lrtest(glm_t_0, glm_t_0_noi) # Supplementary Table 1 Beta diversity results 

q_colors =  1 
v_colors =  viridis::viridis(q_colors) 

beta_map1 <- jtools ::plot_summs(glm_t_0, scale = TRUE,  ci_level = 0.95, inner_ci_level = .85, colors = v_colors) + 
  theme_set(theme_minimal(base_size = 8))

beta_map2 <- jtools ::plot_summs(glm_t_0, scale = TRUE,  ci_level = 0.95, inner_ci_level = .85, colors = v_colors) + 
  theme_set(theme_minimal(base_size = 8))

z <- z %>% mutate(island = factor(island, levels = c("MauiNui", "Hawaii" , "Oahu")))
glm_t_0     <- glm(BetaC_mean ~ log(abs(MAP_diff)) * island, family  = Gamma(link = "log"), data = z)
glm_t_0_noi <- glm(BetaC_mean ~ abs(MAP_diff) , family  = Gamma(link = "log"), data = z)
lrtest(glm_t_0, glm_t_0_noi) # Supplementary Table 1 Beta diversity results 

q_colors =  1 
v_colors =  viridis::viridis(q_colors) 

beta_map1 <- jtools ::plot_summs(glm_t_0, scale = TRUE,  ci_level = 0.95, inner_ci_level = .85, colors = v_colors) + 
  theme_set(theme_minimal(base_size = 8))

beta_map2 <- jtools ::plot_summs(glm_t_0, scale = TRUE,  ci_level = 0.95, inner_ci_level = .85, colors = v_colors) + 
  theme_set(theme_minimal(base_size = 8))


a <- ggpubr::ggarrange(beta_map1, beta_map2, ncol = 2, nrow = 1)
b <- ggpubr::ggarrange(beta_map1, beta_map2, ncol = 2, nrow = 1)

ggpubr::ggarrange(a, b, ncol = 1, nrow = 2) 


,# pdf(file = "G:/My Drive/Hawaii_project/Figures/Fig_3_20241022.pdf",   # native only
#     width=3.8, height=1.2, paper = "a4")
# beta_map1
# dev.off()

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
##################################################




cor.test(alpha_native$Haw_St_geo_1, alpha_native$MAP,  method = "pearson") 
cor.test(alpha_native$Haw_St_geo_1, alpha_native$tax_q0,  method = "pearson") 

cor.test(alpha_nativealien$Haw_St_geo_1, alpha_nativealien$MAP,  method = "pearson") 
cor.test(alpha_nativealien$Haw_St_geo_1, alpha_nativealien$tax_q0,  method = "pearson") 

###############################################################################
### plot substrate age and MAP



# # ******************************************************************************
# # NOTE HOW CONFETY THIS IS
# plasma_pal <- c("red", viridis::viridis(n = 13))
# 
# ggplot(z, aes(x = MAP, y = tax_q0)) +  
#   geom_point(aes(color = as.factor(Haw_St_geo) ), size = .3, show.legend = TRUE) + 
#   geom_smooth(aes(color = as.factor(Haw_St_geo), fill = as.factor(Haw_St_geo)), show.legend = TRUE, 
#               method = glm, method.args = list(family = Gamma(link = "log")),
#               size= .3) +
#   scale_color_manual(values = plasma_pal) + scale_fill_manual(values = plasma_pal)+   
#   xlab("")+
#   ylab("Taxonomic diversity (q=0)")+ #ylim(0,15)+ # for native only: ylim(0,15) # for all ylim(0,17)
#   theme_set(theme_minimal(base_size = 8))

