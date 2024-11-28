library(dplyr)
library(iNEXT.3D)
library(ape)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gg.gap)
library(devtools)
library(phylobase)
library(tidyr)
library(lmtest) 
library(mgcv) 

setwd("G:/My Drive/Hawaii_project/Hawaii/pub_code_data/data")

# ******************************************************************************

load("datt_20230419.RData") # datt_20230419.RData built up in code: 01_data_prep_Phylo_Trees_suppl_Fig2.R

length(unique(datt$Scientific_name)) # 142
length(unique(datt_nat$Scientific_name)) # 79 native species, thus 63 alien species

tempo <- dplyr::summarize(group_by(datt, Scientific_name, Native_Status))
tempo %>%
  group_by(Native_Status) %>%
  tally()

tempo0 <- dplyr::filter(tempo, Native_Status == "native" )
tempo1 <- dplyr::summarize(group_by(datt_nat, Scientific_name, Native_Status))
setdiff(tempo0$Scientific_name, tempo1$Scientific_name) 

length(unique(datt$Scientific_name)) # 142

# Tree
load("data_phylo_trees_20230419.RData")
# tree <- hawaii.tree_native$scenario.2
tree <- hawaii.tree_all$scenario.2
tree <- as(tree$run.1, "phylo")
tree = drop.tip(tree, c("Melicope kaalaensis", "Psychotria kaduana"))
class(tree)
length(tree$tip.label)
plot(tree, cex = .7, main = "Hawaii.tree_native+alien")

# Traits
load("data_Trait_imputed_20230418.RData") 
traits = impute_out

# Native or all species
# Run native species only use ** datt_nat ***
# Run with all species including highly invaded plots use ** datt ***

# data <- datt_nat  # native species only, plot filter to avoid highly invaded ones
data <- datt

# Plot level aggregation
min_ind <- 15 # minimum amount of individuals value for plots used for metrics

tmp           <- dplyr::summarize(group_by(data, PlotID,
                                           Scientific_name),Abundance=sum(Abundance))
tmp$Abundance <- as.numeric(tmp$Abundance)

Abun_p           <- pivot_wider(tmp, id_cols = Scientific_name, names_from = PlotID,
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
# DIVERSITY METRICS 

# - Taxonomic diversity
TD_est <- estimate3D(data = Abun_p, diversity = 'TD', q = c(0, 1, 2), datatype = 'abundance', base = 'coverage',
                     level = Cmax, nboot = 2)

out_TD_est <- pivot_wider(TD_est, id_cols = Assemblage, names_from = Order.q, values_from = qD)
out_TD_est <- data.frame(PlotID = out_TD_est$Assemblage,
                         tax_q0 = out_TD_est$`0`,
                         tax_q1 = out_TD_est$`1`,
                         tax_q2 = out_TD_est$`2`)

# - Phylogenetic diversity
PD_est <- estimate3D(data = Abun_p, diversity = 'PD', q = c(0, 1, 2), datatype = 'abundance', base = 'coverage',
                     level = Cmax, nboot = 0, PDtree = tree, PDtype = "PD") # PDtype = "PD" (effective total branch length)
out_PD_est  <- pivot_wider(PD_est, id_cols = Assemblage, names_from = Order.q, values_from = qPD)
out_PD_est <- data.frame(PlotID = out_PD_est$Assemblage,
                         phy_q0 = out_PD_est$`0`,
                         phy_q1 = out_PD_est$`1`,
                         phy_q2 = out_PD_est$`2`)

# - Functional diversity
traits$Scientific_name <- gsub(" ", "_", traits$Scientific_name)

mask <- as.data.frame(rownames(Abun_p))
mask$Scientific_name <- mask$`rownames(Abun_p)`
traits <- left_join(mask, traits, by = "Scientific_name")
rownames(traits) <- traits$Scientific_name # arrange only traits in columns. Species names as rownames
traits$Scientific_name <- NULL
traits$`rownames(Abun_p)` <- NULL

for (i in 1:ncol(traits)) {
  if (class(traits[,i]) == "character") traits[,i] <- factor(traits[,i], levels = unique(traits[,i]))
}
distM <- cluster::daisy(x = traits, metric = "gower") %>% as.matrix()

FD_est <- estimate3D(data = Abun_p, diversity = 'FD', q = c(0, 1, 2), datatype = 'abundance', base = 'coverage',
                     level = Cmax, nboot = 0, FDdistM = distM)

out_FD_est  <- pivot_wider(FD_est, id_cols = Assemblage, names_from = Order.q, values_from = qAUC)
out_FD_est <- data.frame(PlotID = out_FD_est$Assemblage,
                         FD_q0 = out_FD_est$`0`,
                         FD_q1 = out_FD_est$`1`,
                         FD_q2 = out_FD_est$`2`)

# Organizing diversity metrics
out_tmp <-plyr::join(out_TD_est, out_PD_est, by = "PlotID")
out_tmp <-plyr::join(out_tmp, out_FD_est,    by = "PlotID")

datt_pl_a <- dplyr::summarize(group_by(datt, PlotID, geo_entity2, Study),
                              MAP = max(MAP),
                              Abundance=sum(Abundance))

out_tmp1 <-plyr::join(datt_pl_a, # Adds plots info, i.e., MAP, island, etc
                       out_tmp , by = "PlotID")

out_results = out_tmp1 %>% filter(!is.na(tax_q0))

out_results %>%
  group_by(geo_entity2) %>%
  tally()

# ******************************************************************************
# Saving results
# 
# results_native_plot <- out_results
#   save(results_native_plot, file= "results_native_plot_20230601.RData")
# 
# results_all_plot <- out_results
#   save(results_all_plot, file= "results_all_plot_20230601.RData")

# ******************************************************************************

# PLOTTING DIVERSITY METRICS Figure 2 ----
coco = c(
  "#00AFBB", 
  "gray60", 
  "#E7B800")

load("results_native_plot_20230601.RData")
load("results_all_plot_20230601.RData")   

# ADITIONAL VARIABLES: mean annual Air temperature and geological information (substrate ages within islands) 
datt_env <- read.csv("C:/Hawaii_project/Hawaii/pub_code_data/data/datt_env_20240604.csv")
datt_env <- select(datt_env, PlotID, Haw_St_geo, tair_ann)

results_native_plot <-left_join(results_native_plot, datt_env, by = "PlotID") 
results_all_plot    <-left_join(results_all_plot, datt_env, by = "PlotID") 

#   z <- results_native_plot
#   z <- results_all_plot
#   z <- filter(z, MAP > 800 & MAP < 4000) # Sub-set MAP range. Supplementary material

z %>% group_by(geo_entity2) %>% tally()

# To avoid negative values to be predicted we used GLM Gamma 
# Taxonomic
tx_q0 <- ggplot(z, aes(x = MAP, y = tax_q0)) +  
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+ 
  xlab("")+
  ylab("Taxonomic diversity (q=0)")+ #ylim(0,15)+ # for native only: ylim(0,15) # for all ylim(0,17)
  theme_set(theme_minimal(base_size = 8))
  
tx_q2 <- ggplot(z, aes(x =MAP, y = tax_q2)) +  
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+ 
  xlab("")+
  ylab("(q=2)")+ #ylim(0,7)+  # for native only: ylim(0,7) # for all ylim(0,14)
  theme_set(theme_minimal(base_size = 8))
  
# Phylogenetic 
p0 <- ggplot(z, aes(x =MAP, y = phy_q0)) +
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+
  xlab("")+
  ylab("Phylogenetic diversity (q=0)")+ #ylim(320,1300)+ #  for native only: ylim(100,1300)  # for all ylim(320,1300)
  theme_set(theme_minimal(base_size = 8))

p2 <- ggplot(z, aes(x =MAP, y = phy_q2)) +   
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+
  ylab("(q=2)")+ # ylim(100,500)+ #  for native only: ylim(100,500)  # for all 320,600
  xlab("")+
  theme_set(theme_minimal(base_size = 8))

# Functional
f0 <- ggplot(z, aes(x =MAP, y = FD_q0)) +
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +  
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+
  ylab("Functional diversity (q=0)")+ #ylim(.99, 4) +#  #  for native only no ylim()
  xlab("Mean annual precipitation (mm)")+
    theme_set(theme_minimal(base_size = 8))

f2 <- ggplot(z, aes(x =MAP, y = FD_q2)) +   
  geom_point(aes(color = geo_entity2), size = .3, show.legend = FALSE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = FALSE, 
              method = glm, method.args = list(family = Gamma(link = "log")),
              size= .3) +
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+

  ylab("(q=2)")+ #ylim(.99, 4)+  #  for native only: ylim(.99, 3.5)
  xlab("Mean annual precipitation (mm)")+
  theme_set(theme_minimal(base_size = 8))

legend <- ggplot(z, aes(x = MAP, y = FD_q2)) +   
  geom_point(aes(color = geo_entity2), size = .3 ,show.legend = TRUE) + 
  geom_smooth(aes(color = geo_entity2, fill = geo_entity2), show.legend = TRUE, method = glm,
              method.args = list(family = Gamma(link = "log"))) +    
  scale_color_manual(values = coco) + scale_fill_manual(values = coco)+
  ylab("")+ xlab('ALL: Hawaii 254, Maui 86, Oahu 35')+   # ALL: Hawaii 254, Maui 86, Oahu 35  # Native: Hawaii 190, Maui 74, Oahu 18
  theme_set(theme_minimal(base_size = 8))

# level_plot_glm_nat =ggpubr::ggarrange(tx_q0,  tx_q2,
#                   p0,  p2,
#                   f0,  f2,
#                   legend,
#                   ncol = 2, nrow = 4)
# level_plot_glm_nat

level_plot_glm_all =ggpubr::ggarrange(tx_q0,  tx_q2,
                                      p0,  p2,
                                      f0,  f2,
                                      legend,
                                      ncol = 2, nrow = 4)
level_plot_glm_all

pdf(file = "C:/Hawaii_project/Figures/Fig_2_20230425.pdf",
    width=7, # 89 mm (single column) is 3.5 inches.
    height=5.5,
    paper = "a4")
ggpubr::ggarrange(level_plot_glm_nat, level_plot_glm_all)
dev.off()

pdf(file = "C:/Hawaii_project/Figures/Fig_5_supp_20230425.pdf",   
    width=7, # 89 mm (single column) is 3.5 inches.
    height=5.5,
    paper = "a4")
ggpubr::ggarrange(level_plot_glm_nat, level_plot_glm_all)
dev.off()

# INTERACTION ISLAND WITH PRECIPITATION ----

#   z <- results_native_plot
#   z <- results_all_plot

z <- filter(z, MAP > 800 & MAP < 4000) # for Sub-set MAP range

z <- z %>%
  mutate(geo_entity2 = factor(geo_entity2, levels = c("Maui Nui", "Hawai'i Island", "O'ahu Island")))

# interaction
glm_t_0 <- glm(tax_q0 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_t_1 <- glm(tax_q1 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_t_2 <- glm(tax_q2 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_p_0 <- glm(phy_q0 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_p_1 <- glm(phy_q1 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_p_2 <- glm(phy_q2 ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_f_0 <- glm(FD_q0  ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_f_1 <- glm(FD_q1  ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)
glm_f_2 <- glm(FD_q2  ~ log(MAP) * geo_entity2, family  = Gamma(link = "log"), data = z)

# Without interaction
glm_t_0_noi <- glm(tax_q0 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_t_1_noi <- glm(tax_q1 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_t_2_noi <- glm(tax_q2 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_p_0_noi <- glm(phy_q0 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_p_1_noi <- glm(phy_q1 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_p_2_noi <- glm(phy_q2 ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_f_0_noi <- glm(FD_q0  ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_f_1_noi <- glm(FD_q1  ~ log(MAP) , family  = Gamma(link = "log"), data = z)
glm_f_2_noi <- glm(FD_q2  ~ log(MAP) , family  = Gamma(link = "log"), data = z)

# Test of interaction significance 
# Likelihood ratio tests compares the goodness of fit of two statistical models 
# So, if Pr(>Chisq) is smaller than 0.05 then model with interaction is better

lrtest(glm_t_0, glm_t_0_noi) #  model with interaction is better!
lrtest(glm_t_2, glm_t_2_noi) 
lrtest(glm_p_0, glm_p_0_noi) 
lrtest(glm_p_2, glm_p_2_noi)
lrtest(glm_f_0, glm_f_0_noi) 
lrtest(glm_f_2, glm_f_2_noi)

jtools::summ(glm_p_2)

v_colors =c("#440154FF","#481467", "#287d8e", "#1f968b", "#dde318", "#fde725") # https://waldyrious.net/viridis-palette-generator/

# https://jtools.jacob-long.com/articles/summ.html
glm_log = jtools ::plot_summs(glm_t_0,  glm_t_2,glm_p_0, glm_p_2, glm_f_0, glm_f_2,
                              scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 , 
                              colors = v_colors,legend.title = "All") +  theme(legend.position = "none")
glm_log


legend1 = jtools ::plot_summs(glm_t_0,  glm_t_2, glm_p_0, glm_p_2, glm_f_0, glm_f_2,
                              scale = TRUE,  ci_level = 0.95, inner_ci_level = .85 , 
                              colors = v_colors, model.names = c("Taxonomic (q=0)",    "Taxonomic (q=2)",
                                                                 "Phylogenetic (q=0)", "Phylogenetic (q=2)",
                                                                 "Functional (q=0)",   "Functional (q=2)") ) +
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




