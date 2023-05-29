#############################################
# check invasion across islands and plots   #
#############################################

require(dplyr)
require(reshape2)

##################

rm(list = ls())

datt = read.csv("C:/Hawaii_project/additional_studies/HawIslandsAbundance_2SizeClasses_100plus_v2.csv",sep="," ) 

datt<-filter(datt, SizeClass==5) %>%
  filter(., Native_Status=="alien" | Native_Status=="native") # . tells R to use all variables in the dataframe

## Island-level alien abundance

all_g<-summarise(group_by(datt, geo_entity2, Native_Status), TotalN=sum(Abundance_ha))

all_gg<-dcast(all_g, geo_entity2~Native_Status,value.var = "TotalN", sum )
all_gg$all<-all_gg$alien+all_gg$native

all_gg$PercAlien<-100*(all_gg$alien/all_gg$all)

invasion_abundance<-dplyr::select(all_gg, Island=geo_entity2, RelAbundance_Alien=PercAlien)

#########################
# Percent invaded plots #
#########################

datt$Native_Status<-as.character(datt$Native_Status)
datt$Native_Status2<-ifelse(datt$Native_Status=="native",0,NA)
datt$Native_Status2<-ifelse(datt$Native_Status=="alien",1,datt$Native_Status2)

#Plots

invaded<-summarize(group_by(datt, Island=geo_entity2,PlotID,Native_Status), Trees=sum(Abundance_ha))
invadedd<-dcast(invaded, Island+PlotID~Native_Status,value.var="Trees",sum)

invadedd$PercInvaded<-invadedd$alien/(invadedd$native+invadedd$alien)
invadedd$PercInvaded<-ifelse(invadedd$PercInvaded==Inf,1,invadedd$PercInvaded)
invadedd$Invaded_binary<-ifelse(invadedd$alien>0,1,0)

summarise(group_by(invadedd, Island, Invaded_binary), Plots=length(unique(PlotID)))

sum(invadedd$Invaded_binary) # 227/517 plots (0.4390716)

sum(invadedd$alien)/sum(invadedd$native+invadedd$alien) # 0.3772957 of individuals are classified as invasive

invaded_island<-summarize(group_by(invadedd, Island), Invaded=sum(Invaded_binary), Plots=length(unique(PlotID)), Invaded_Trees=sum(alien), TreesTot=sum(alien+native))
invaded_island$PercInvaded_Plot<-invaded_island$Invaded/invaded_island$Plots
invaded_island$PercInvaded_Abundance<-invaded_island$Invaded_Trees/invaded_island$TreesTot

invaded_island<-select(invaded_island,Island,PlotN=Plots,PercInvaded_Plot, PercInvaded_Abundance )

library(rms)

invaded_p<-dplyr::filter(invadedd, Invaded_binary==1)
invaded_island_abund_CI<- invaded_p %>%
  group_by(Island) %>%
  #do(data.frame(rbind(smedian.hilow(.$PercInvaded)))
  do(data.frame(rbind(smean.cl.boot(.$PercInvaded, B=1000))))

detach("package:rms")

invaded_island_abund_CI<-select(invaded_island_abund_CI, Island, Mean_PercInvaded_Abundance=Mean, lCI=Lower,uCI=Upper)

invaded_islands<-left_join(invaded_island,invaded_island_abund_CI, by="Island")

invaded_islands<-select(invaded_islands, Island, Plots=PlotN, PercInvaded_Plot, PercInvaded_Abundance,
                        Mean_PercInvaded_Plot=Mean_PercInvaded_Abundance, l95Perc_PercInvaded_Plot=lCI,u95Perc_PercInvaded_Plot= uCI)
                        