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
library(MESS)


setwd("D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/")


dataroot<-"Output_summary_tables/"
plotroot<-"Figures/"


version<-"DISASTER2"
date<-"2025-04-18"
damage.all<-read.csv(paste0(dataroot,date,"_damages_DISASTER2.csv"))
recovery.all<-read.csv( paste0(dataroot,date,"_recovery_DISASTER2.csv"))


MF<-read.csv(paste0(dataroot,"generated_multi-functionality_tables/20260415_MF_ES_score.csv"))


# We look the 10 year period after the large disturbances:


d<-damage.all %>% filter(year>49& year<60)


# I want to have a plot where models and winds are there as a range for each mgm

dsum<-d %>% group_by(mgm,year, rcp) %>% summarise(mean.wind=mean(wind), min.wind=min(wind), max.wind=max(wind),
                                                  mean.bb=mean(barkbeetle), min.bb=min(barkbeetle), max.bb=max(barkbeetle))


# I want to have a plot with total damage  for each case
# First I summarize the damaged volume for each simulation run for the 10 year period which we filtered:

s<-d %>% group_by(mgm,model,windcase, rcp) %>% summarise(wind=sum(wind),bb=sum(barkbeetle)) %>% mutate(total=wind+bb)

# Then I calculate model and windcases minimums, maximums and averages:
smean<-s %>% group_by(mgm, rcp) %>% summarise(mean.wind=mean(wind), 
                                              mean.bb=mean(bb)  )

smean_long<-pivot_longer(smean, cols=c("mean.wind","mean.bb"))



# how to translate these values to relative damage?
# compare it to pre.dist value??

lnd.vol<-d %>% filter(year==50) %>% group_by(mgm,rcp) %>% summarise(meanvol=mean(landscape_volume_yearstart))
smean2<-left_join(smean,lnd.vol, by=c("mgm","rcp") ) %>% mutate(perc.wind=100*mean.wind/meanvol, perc.bb=100*mean.bb/meanvol) %>% dplyr::select(-mean.wind, -mean.bb, -meanvol)


smean2_long<-pivot_longer(smean2, cols=c("perc.wind","perc.bb"))




#-------------------------------------------------------------------------- Recovery times?

head(recovery.all)
recovery.all<-recovery.all %>% mutate(recovery.time=recovery_year_volume-year, case=paste0(windcase,model))
rec<-recovery.all %>% group_by(mgm,rcp,model,windcase) %>% summarize(recovery.time=max(recovery.time))

recovery.all1<-recovery.all %>% mutate(r=case_when(  year>recovery_year_volume ~ NA,
                                                     year<=recovery_year_volume ~Perc_Diff_VOL ))

df_endpoints<-recovery.all1 %>% group_by(mgm,rcp,model,windcase) %>% summarise(rt=max(recovery.time)) %>% mutate(value=0)

df_endpoints2<-df_endpoints %>% group_by(mgm,rcp) %>% summarize(mean1=mean(rt), min1=min(rt), max1=max(rt))



not.recovered<-df_endpoints %>% filter(rt==Inf) 
not.recoveredno<-df_endpoints %>% filter(rt==Inf) %>% summarize(n=n())
NCC_HIRHAM5<- df_endpoints %>% filter(model=="NCC_HIRHAM5"&rcp=="rcp45")



df_endpoints<-df_endpoints %>% filter(rt<50)




ii<-df_endpoints %>% group_by(rcp,mgm) %>% summarize(recovery_time=mean(rt))





less<-recovery.all1 %>% select(year_after_impact,model, mgm, rcp, windcase,volume_m3,r)

l1<-less %>% filter(is.na(r)==F) %>% group_by(model, rcp, mgm, windcase) %>%  
  arrange(year_after_impact) %>% 
  summarize(auc=auc(year_after_impact,r)) %>% 
  mutate(norm.auc=auc/-5000.)

#---------------------MAKE ONE EXAMPLE FOR ONE MODEL AND ONE WIND-->
example<-l1 %>% filter(model=="NCC_HIRHAM5",windcase=="w8")

pdf(paste0(plotroot, "2d_Resilience_calculation_example_AUC_time_series_coloring.pdf"), height = 6, width = 10)

less %>%
  filter(model=="NCC_HIRHAM5",windcase=="w8",!is.na(r)) %>%
  group_by(model, rcp, mgm, windcase) %>%
  arrange(year_after_impact) %>%
  mutate(  auc = auc(year_after_impact, r)) %>%
  ggplot(aes(x = year_after_impact, y = r)) +
  geom_ribbon(aes(ymin = r, ymax = 0), fill = "skyblue", alpha = 0.4) +
  geom_line(color = "forestgreen") +
  facet_grid(rcp~ model + mgm + windcase) +
  labs(    x = "Years after impact",
           y = "% difference of volume compared to pre-dist level"
  ) +
  
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-65,              # position at top
                    label =  paste("AUC=",round(auc, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-75,              # position at top
                    label =  paste0("normAUC=",round(norm.auc, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x = 40,              # position at right edge
                    y = -85,              # position at top
                    label =  paste0("1-normAUC=",round(1-norm.auc, 2))),
                hjust = 1., vjust = 1.,
                color = "red", size = 3.5,
                inherit.aes = FALSE  )+
  xlim(0,50)+ylim(-100,0)+theme_bw()

dev.off()

# MAKE ANOTHER EXAMPLE, WHERE PUT ALL THE RECOVERY TIME, AUC AND IMPACT VALUES ON THE PLOT


pdf(paste0(plotroot, "2d_Resilience_calculation_example_AUC_time_series_example_with_values.pdf"), height = 10, width = 10)

# look the adaptation and w8 case:

example<-l1 %>% filter((mgm=="ADAPTATION"&windcase=="w8"))

example<-left_join(example,rec,by=c("model","rcp","mgm","windcase"))

dsum2<-d %>% filter(year==50) %>% mutate(rel.wind.impact=100*wind/landscape_volume_yearstart) %>%
  select(-year,-barkbeetle,-wind,-landscape_volume_yearstart,-impact,-relimpact)

example<-left_join(example,dsum2,by=c("model","rcp","mgm","windcase"))




less %>%
  filter((mgm=="ADAPTATION"&windcase=="w8"),!is.na(r)) %>%
  group_by(model, rcp, mgm, windcase) %>%
  arrange(year_after_impact) %>%
  mutate(  auc = auc(year_after_impact, r)) %>%
  ggplot(aes(x = year_after_impact, y = r)) +
  geom_ribbon(aes(ymin = r, ymax = 0), fill = "skyblue", alpha = 0.4) +
  geom_line(color = "forestgreen") +
  facet_grid(rcp~ model + mgm + windcase) +
  labs(    x = "Years after impact",
           y = "% difference of volume compared to pre-dist level"
  ) +
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-45,              # position at top
                    label =  paste("impact=",round(rel.wind.impact, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-55,              # position at top
                    label =  paste("RT=",round(recovery.time, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-65,              # position at top
                    label =  paste("AUC=",round(auc, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-75,              # position at top
                    label =  paste0("normAUC=",round(norm.auc, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x = 40,              # position at right edge
                    y = -85,              # position at top
                    label =  paste0("1-normAUC=",round(1-norm.auc, 2))),
                hjust = 1., vjust = 1.,
                color = "red", size = 3.5,
                inherit.aes = FALSE  )+
  xlim(0,50)+ylim(-100,0)+theme_bw()+
  theme(
   # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

dev.off()





#I want to put them all into one plot, coloring the resilience indicator


less<-left_join(less,l1,by=c("rcp", "model" , "mgm" , "windcase"))

pdf(paste0(plotroot, "2d_Resilience_spagetti_relative_volume_in_time.pdf"), height = 7, width = 8)
ggplot(less, aes(x = year_after_impact, y = r, group=paste(rcp, model , mgm , windcase), colour = 1-norm.auc)) +
  geom_line() +
  labs(    x = "Years after impact",
           y = "% difference of volume compared to pre-dist level"
  ) +
  xlim(0,50)+ylim(-100,0)+theme_bw()+theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 50)+
  geom_hline(yintercept = -100)
dev.off()
# which are the most resilient ones???
pdf(paste0(plotroot, "2d_Resilience_per_rcp_model.pdf"), height = 8, width = 8)
ggplot(l1, aes(x = mgm, y = 1-norm.auc, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) +
  facet_grid(model~rcp) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(0.7,1)+
  labs(x = "MGM", y = "1-normAUC", title = "Resilience")
dev.off()

pdf(paste0(plotroot, "2d_Resilience_all_whiskerplot.pdf"), height = 8, width = 8)
ggplot(l1, aes(x = mgm, y = 1-norm.auc, fill = rcp)) +
  geom_boxplot(width=0.5) +
  theme_bw() +
  ylim(0.7,1)+
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "MGM", y = "1-normAUC", title = "Resilience")

dev.off()


to_plot<-left_join(l1,MF,by=c("model","mgm","windcase","rcp"))


pdf(paste0(plotroot, "2d_Resilience_vs_MF_without_bars.pdf"), height = 8, width = 8)


p_main<-ggplot(to_plot ,aes( score,   1-norm.auc ))+
  geom_point( aes(color=  factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))), size=2)+
  geom_smooth(method = "lm")+
  labs(x = "MF score",y="Resilience(1-norm.auc)", col="mgm")+
  facet_grid(~rcp)+
  scale_color_manual(values=(c( "#f2c14e","chocolate", "black", "#62d75f","#248721" )))+
  theme_bw()+
  theme(legend.position = "bottom",panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))
p_main
dev.off()

