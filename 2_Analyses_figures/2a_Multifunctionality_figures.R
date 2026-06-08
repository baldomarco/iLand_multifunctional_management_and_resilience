#----------------------------------------------------------------------------------2025/2026
# Laura Dobor, CZU, dobor@fld.czu.cz
# Study: iLand modeling management and resilience on Kostelec area
# 2026.04.15.
#
#     ANALYSES AND VISUALIZATION
#
#
#----------------------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)   
library(fields)
library(ggradar)
library(ggiraphExtra)
library(cowplot)



setwd("C:/Users/baldo/Documents/GitHub/iLand_management_and_resilience/")

dataroot<-"Output_summary_tables/"
plotroot<-"Figures/"

date<-"2025-04-19"     # date of the final simulation
version<-"NO_DISASTER"  

# Read data:
MF.all<-read.csv(paste0(dataroot,date,"_multifunctionality.csv"))
lnd<- read.csv(paste0(dataroot,date,"_landscape.csv"))
  

#_______________________________________________________________________________


species.we.have<-unique(lnd$species)                               

variable_names <- c(
  `nat.mortality.volperha` = "MORT",
  `impact` = "WIND+BB",
  `NEP` = "NEP",
  `standing.volume.decidious` = "BroadlVOL",
  `BA.decidious` = "BroadlBA",
  `deadwood.c.ag` = "DEADWOOD",
  `carbonstock` = "TOTALC",
  `shannon_BA_landscape` = "ShannonBA",
  `shannon_VOL_landscape` = "ShannonVOL",
  `annual.harvest` = "HARVEST",
  `annual.increment` = "INCREMENT",
  `standing.volume` = "VOLUME",
  `LAI` = "LAI", 
  `rcp45` = "RCP4.5",
  `rcp85` = "RCP8.5",
  `EC-EARTH_RACMO22E-r1`="EC-EARTH_RACMO22E-r1",
  `HadGEM2_CCLM`="HadGEM2_CCLM",
  `refclim`="refclim",
  `MPI_CCLM`="MPI_CCLM",
  `NCC_HIRHAM5`='NCC_HIRHAM5',
  `-`="-",
  `w5` = "w5",
  `w6` = "w6", 
  `w7` = "w7", 
  `w8` = "w8",
  `w9` = "w9"
  
)
cols.all=c( "rops"="#e0e0e0", "acpl"="#A9A9A9",   "alin"="#696969", "alvi"="#2e2e2e",
            "bepe"="#fadfad", 
            "casa"="#7eeadf", "coav"="#20c6b6",  
            "tipl"="#645394", "ulgl"="#311432" ,
            "saca"="#D8BFD8",  "soar"="#DDA0DD", "soau"="#BA55D3",
            "pice"="#D27D2D", "pini"="#a81c07",
            "algl"="#2ECBE9","tico"="#128FC8",  "potr"="#00468B","poni"="#5BAEB7",
            "frex"="#fe9cb5","cabe"="#fe6181","acps"="#fe223e",
            "lade"="#FFFE71","abal"="#FFD800", "pisy"="#A4DE02",
            "fasy"="#76BA1B", "piab"="#006600",
            "quro"="#FF7F00", "qupe"="#FF9900", "qupu"="#CC9900" 
)



new_order_gg.all=c("alvi","alin", "acpl", "rops","bepe" ,"coav", "casa", "ulgl", "tipl",  "soau", "soar", "saca",  "pini", "pice",
                   "poni", "algl", "tico", "potr",  "frex","cabe", "acps",  "lade", "abal",  "qupu", "qupe","quro","pisy", "fasy", "piab")

# This will show at the end only the species we really have on the landscape. 

cols<-cols.all[names(cols.all) %in% species.we.have]
new_order_gg<- new_order_gg.all[new_order_gg.all %in% species.we.have]
#_______________________________________________________________________________

#1) I want species composition barplots with volume

avg.spec.vol<-lnd %>% filter(year>36&year<65) %>% group_by(mgm, rcp,species) %>% summarise(volume=mean(volume_m3))


g1<-ggplot(avg.spec.vol, aes(mgm,volume, fill=factor(species, levels=new_order_gg)))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Species comp. based on volume mid30y avg")+
  facet_grid(~rcp)+
  labs(x = "Management",y="Volume m3/ha",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))



# species composition, %

sum.vol<-lnd %>% filter(year>36&year<65) %>% group_by(mgm, rcp, model, windcase, year) %>% summarise(totalvolume=sum(volume_m3))
sum.vol2<-sum.vol %>% group_by(mgm,rcp) %>% summarise(totalvolume=mean(totalvolume))

avg.spec.vol2<-left_join(avg.spec.vol,sum.vol2, by=c("mgm", "rcp")) %>% mutate(vol.prop=volume/totalvolume)

g2<-ggplot(avg.spec.vol2, aes(mgm,vol.prop, fill=factor(species, levels=new_order_gg)))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Species comp. based on volume  mid30y avg")+
  facet_grid(~rcp)+
  labs(x = "Management",y="Proportion",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))


# Go for the indicators --> I want spider diagrams with the mean 

vars<-c("NEP","carbonstock","LAI","standing.volume","annual.increment","annual.harvest","shannon_BA_landscape","deadwood.c.ag")

MF.all2<-MF.all %>% filter(year>36&year<65) %>% group_by(mgm, rcp) %>% summarise_all(mean) %>% select(-year) %>% mutate(deadwood.c.ag=deadwood.c.ag/1000,carbonstock=carbonstock/1000. )

MF.all2.long<-pivot_longer(MF.all2, cols=c("NEP","carbonstock","LAI","standing.volume","annual.increment","annual.harvest","shannon_BA_landscape","deadwood.c.ag")) %>% select(mgm,rcp,name, value)

MF.all3<-pivot_wider(MF.all2.long,names_from = mgm )



g3<-ggRadar(data=MF.all3,mapping = aes(colour = rcp, facet=name), 
             rescale = FALSE, interactive = FALSE, use.label = TRUE, size = 2,alpha=0.1,
             legend.position = "right", scales="free") +theme_bw()+
  ggtitle("Multifunctionality indicators mid30y avg")+
  scale_fill_manual(values=(c("#3a6ea5","#f2c14e","#f78154" )))+
  scale_color_manual(values=(c("#3a6ea5","#f2c14e","#f78154" )))+
  theme(strip.background =element_rect(fill="white"))

data.frame(MF.all3)




# ---------------------------------------------------------------Do the normalization of indicators:

# Make 30y average for all simulation runs:

MF.mid30yavg<-MF.all %>% group_by(rcp,model,mgm, windcase) %>% filter(year>36&year<65) %>% summarise_all(mean) %>% select(-year)

# Make a long version with the indicators in one column:
MF.mid30yavg.long<-data.frame(pivot_longer(MF.mid30yavg,cols=c(colnames(MF.mid30yavg)[5:17])))

# global maximums:
globalmax<-data.frame(MF.mid30yavg.long %>% group_by(name) %>% summarise(max=max(value)))
  
# We will use min-max normalization, using 0 as minimum in most of the cases, except NEP: NEP min will be the overall minimum:
globalNEPmin<-data.frame(MF.mid30yavg.long %>% filter(name=="NEP") %>% summarise(min=min(value)))$min
print(paste0("NEP minimum across all simulations: ", globalNEPmin))
  
# Add a column on min for normalization:
MF.mid30yavg.long<-MF.mid30yavg.long %>% mutate(minfornorm= case_when(name=="NEP"   ~ globalNEPmin,
                                                                        name!="NEP" ~0))
  
  
 # Add the global maxes there as well:
 MF.mid30yavg.long<-left_join(MF.mid30yavg.long,globalmax, by="name")
  
#Calculate the normlaized value
rescale.alter<-MF.mid30yavg.long %>% mutate(value.normalized1 = (value - minfornorm) / (max - minfornorm)) 
head(rescale.alter)
  


# I want to make a table: management and rcps are the columnames, ES services rownames
# Make the average of the wind and models, keep rcps
  
myvars<-c("NEP", "carbonstock","LAI","standing.volume","annual.increment", "annual.harvest","shannon_BA_landscape","deadwood.c.ag")
  
  
ES_table0<-rescale.alter %>% filter(name %in% myvars) %>% group_by(name, rcp, mgm) %>% mutate(name = factor(name, levels = myvars)) %>% 
  summarise(value=mean(value), value.norm=mean(value.normalized1)) 
  
ES_table<-ES_table0 %>% select(-value)
  
  
ES_table_wide<-pivot_wider(ES_table, names_from = c(mgm, rcp), values_from = value.norm) %>% arrange(name) 
write.csv(ES_table_wide,paste0(dataroot,"/generated_multi-functionality_tables/20260407_ES_normalized_values_individual_values.csv"))

ES_table0<-rescale.alter %>% filter(name %in% myvars) %>% group_by(name, rcp, mgm) %>% mutate(name = factor(name, levels = myvars)) %>% 
  summarise(value=mean(value), value.norm=mean(value.normalized1))

ES_table<-ES_table0 %>% select(-value.norm)

ES_table_wide<-pivot_wider(ES_table, names_from = c(mgm, rcp), values_from = value)  %>% arrange(name) 
write.csv(ES_table_wide,paste0(dataroot,"/generated_multi-functionality_tables/20260407_ES_absolute_values_individual_values.csv"))




# Want to have the 4 ecosys servies as the avg of different indicators using normalized values


fintab.norm<-rescale.alter %>% select(rcp,model,mgm,windcase, name, value.normalized1)

fintab.norm.wide<-fintab.norm %>% pivot_wider(names_from = name,values_from ="value.normalized1")

fintab.norm<-data.frame(fintab.norm.wide %>% group_by(rcp,model,mgm, windcase) %>% mutate(Climate=sum(NEP,carbonstock)/2.,
                                                                                           Water=sum(LAI,standing.volume)/2., 
                                                                                           Production=sum(annual.increment,annual.harvest)/2., 
                                                                                           Biodiversity=sum(shannon_BA_landscape,deadwood.c.ag)/2.) )

fintab.norm<-data.frame(fintab.norm %>% select("rcp","model","mgm","windcase","Climate","Production","Water","Biodiversity") %>% 
                              mutate(score=(Climate+Production+Water+Biodiversity)))

# At this point I have all cases the ES scores, and overall score

# I want a summarized averages of the cases, but also the min-max-or other stats for whiskers:

mean<-fintab.norm %>% 
  group_by(rcp, mgm) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

mean<-fintab.norm  %>% group_by(rcp, mgm) %>% summarise(Climate=mean(Climate),Production=mean(Production),Water=mean(Water),Biodiversity=mean(Biodiversity)) 

g16<- ggRadar(data=mean,mapping = aes(colour = mgm, facet=rcp), 
             rescale = FALSE, interactive = FALSE, use.label = TRUE, size = 2,alpha=0.1,
             legend.position = "bottom", scales="free") +theme_bw()+ylim(0,1)+
  ggtitle("Indicator groups mid30y avg")+
  scale_fill_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))


g16




score.norm<-fintab.norm

long<-pivot_longer(score.norm,cols=c("Climate","Production","Water","Biodiversity"))
longstats<-long %>% group_by(rcp,mgm, name) %>% summarise(s=mean(value), smin=min(value), smax=max(value))
MFs<-score.norm %>% select(rcp,model,mgm,windcase,score) %>% group_by(rcp,mgm) %>% summarise(sall=mean(score), sallmin=min(score), sallmax=max(score))


longstats<-data.frame(left_join(longstats,MFs, by=c("rcp","mgm")))


g17<-ggplot(longstats, aes(mgm,s, fill=name)) +
  ggtitle("MF score mid30y avg")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity")+
  geom_errorbar( aes(x=mgm, ymin=sallmin, ymax=sallmax), position = "identity",width = 0.3)+
  facet_grid(~rcp)+  theme_bw()+ylab("Multifunctionality score")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))




# -------------------------------- EFFECT OF CLIMATE CHANGE ON THE MULTIFUCTIONALITY:
# DIFFERENCES TO THE REFERENCE CASE:

todiff<-long %>% select(rcp,model, mgm, windcase, name, value)

todiffref<-todiff %>% filter(rcp=="-") %>% rename(rcpref=rcp,refval=value) %>% select(-model)
todifffut<-todiff %>% filter(rcp!="-")

diff<-left_join(todifffut,todiffref,by=c("mgm", "windcase","name")) %>% mutate(absdiff=value-refval, 
                                                                               percdiff=100*(value-refval)/refval)

meandiff<-data.frame(diff %>% group_by(rcp,mgm,name) %>% summarise(meanpercdiff=mean(percdiff),  
                                                                   minpercdiff=min(percdiff),
                                                                   maxpercdiff=max(percdiff),
                                                                   q1=quantile(percdiff, 0.25),
                                                                   q2=quantile(percdiff, 0.75)
                                                                   
                                                                   ))



g29<-ggplot(meandiff, aes(mgm,meanpercdiff, fill=rcp)) +
  ggtitle("With min-max range")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = minpercdiff, ymax = maxpercdiff),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black") +
  facet_grid(~name)+  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))


g29b<-ggplot(meandiff, aes(mgm,meanpercdiff, fill=rcp)) +
  ggtitle("With interquartile range")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = q1, ymax = q2),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black") +
  facet_grid(~name)+  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))



g30<-ggplot(meandiff, aes(name,meanpercdiff, fill=rcp)) +
  ggtitle("With min-max range")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = minpercdiff, ymax = maxpercdiff),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black") +
  facet_grid(~mgm)+  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))

g30b<-ggplot(meandiff, aes(name,meanpercdiff, fill=rcp)) +
  ggtitle("With interquartile range")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = q1, ymax = q2),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black")+
  facet_grid(~mgm)+  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))
g30




# Same differences for the overall multifunctionality


todiffref<- score.norm %>% filter(rcp=="-") %>% rename(rcpref=rcp,refscore=score) %>% select(-model, -Climate,-Production, -Water,-Biodiversity)
todifffut<- score.norm %>% filter(rcp!="-")

diff<-left_join(todifffut,todiffref,by=c("mgm", "windcase")) %>% mutate(absdiff=score-refscore, 
                                                                               percdiff=100*(score-refscore)/refscore)

meandiff2<-data.frame(diff %>% group_by(rcp,mgm) %>% summarise(meanpercdiff=mean(percdiff),minpercdiff=min(percdiff),
                                                               maxpercdiff=max(percdiff),
                                                               q1=quantile(percdiff, 0.25),
                                                               q2=quantile(percdiff, 0.75)))


g31<-ggplot(meandiff2, aes(mgm,meanpercdiff, fill=rcp)) +
  ggtitle("With min-max range")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e")))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = minpercdiff, ymax = maxpercdiff),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black")+
  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))

g31b<-ggplot(meandiff2, aes(mgm,meanpercdiff, fill=rcp)) +
  ggtitle("With interquartile range")+
  scale_fill_manual(values=(c( "#5fad56", "#f2c14e")))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = q1, ymax = q2),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black")+
  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))



pdf(paste0(plotroot, "2a_Multifunctionality_speciesprop_MF.pdf"), height = 6, width = 10)
print(g1)
print(g2)
print(g16)
print(g29)
print(g29b)
print(g30)
print(g30b)
print(g31)
print(g31b)
dev.off()

meandiff2<- meandiff2 %>% mutate(name="MF SCORE")
g31b<-ggplot(meandiff2, aes(mgm,meanpercdiff, fill=rcp)) +
  ggtitle("")+
  scale_fill_manual(values=(c( "#5fad56", "#f2c14e")))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = q1, ymax = q2),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black")+
  facet_wrap(~name)+ theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))

g30c<-ggplot(meandiff, aes(name,meanpercdiff, fill=rcp)) +
  ggtitle("")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = q1, ymax = q2),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black")+
  facet_wrap(~mgm, nrow=1)+  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))
g30c




pdf(paste0(plotroot, "2a_Multifunctionality_climate_change_effect_barplot.pdf"), height = 6, width = 14)
plot_grid(g30c, g31b, ncol = 2, rel_widths = c(3, 1),align = "h", axis = "tb")
dev.off()



table<-longstats %>% select(rcp,mgm,name, s)
table.wide<-pivot_wider(table, names_from=name, values_from=s)






# WRITE SOME DATA OUT FOR FURTHER ANALYSES:
write.csv(fintab.norm,paste0(dataroot,"/generated_multi-functionality_tables/20260415_MF_ES_score.csv"))
write.csv(rescale.alter,paste0(dataroot,"/generated_multi-functionality_tables/20260415_MF_ES_score_individual_values.csv"))

