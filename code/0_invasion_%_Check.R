######################################################################
# check invasion percentage per plot to discard highly invaded plots  
#####################################################################

require(dplyr)
require(reshape2)

##################

rm(list = ls())

# datt<-read.csv("C:/Hawaii_project/Hawaii_diversity-master/Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv", header=T)

datt = read.csv("C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2.csv",sep="," ) 


##################
# Code Nov 2021: how I filtered for highly invaded plots at OpenNahele level. This is the same filtered plots as in lines 34-36

summary(datt$Plot_Prop_Invaded) # 1 means 100% invaded

plot60 = filter(datt, Plot_Prop_Invaded < 0.6)  # Filtered less than 60 % of alien spp.
plot40 = filter(datt, Plot_Prop_Invaded < 0.4)  # Filtered less than 40 % of alien spp.
plot20 = filter(datt, Plot_Prop_Invaded < 0.2) # Filtered less than 20 % of alien spp.

##################
# Code Jun 2021: how I filtered for highly invaded plots at plot level

invaded  <- dplyr::summarize(group_by(datt, Island = geo_entity2, PlotID, Native_Status, Plot_Area,   # summarize creates a new data frame at plot level
                                      Scientific_name), Trees=sum(Abundance_ha))

# Calculate invasion percentage per plot 
invadedd <- dcast(invaded, Island + PlotID ~ Native_Status, value.var="Trees", sum)
invadedd$PercInvaded <- (invadedd$alien*100)/(invadedd$native+invadedd$alien) # Hasta esta linea necesito para filtrar plots dominados por aliens
lattice::histogram(invadedd$PercInvaded, type = 'count')

plot60 = filter(invadedd, PercInvaded < 60)  # Filtered less than 60 % of alien spp.
plot40 = filter(invadedd, PercInvaded < 40)  # Filtered less than 40 % of alien spp.
plot20 = filter(invadedd, PercInvaded < 20) # Filtered less than 20 % of alien spp.

# How many plots do I end up having per island?
plot40 %>% group_by(Island) %>% count(Island)

save(plot60, plot40, plot20, file = 'H:/Hawaii_project/data_pao/plotos604020.RData')

#=====================================================================
test = dplyr::filter (datt, Study == "Hatheway_1952" )

invaded  <- dplyr::summarize(group_by(test, Island = geo_entity2, PlotID, Native_Status, Plot_Area,   # summarize creates a new data frame at plot level
                                      Scientific_name), Trees=sum(Abundance))

# Calculate invasion percentage per plot 
invadedd <- dcast(invaded, Island + PlotID ~ Native_Status, value.var="Trees", sum)
invadedd$PercInvaded <- (invadedd$alien*100)/(invadedd$native+invadedd$alien)
