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





setwd("C:/Users/baldo/Documents/GitHub/iLand_management_and_resilience/")

dataroot<-"Output_summary_tables/"
plotroot<-"Figures/"


version<-"DISASTER2"
date<-"2025-04-18"
damage.all<-read.csv(paste0(dataroot,date,"_damages_DISASTER2.csv"))
recovery.all<-read.csv( paste0(dataroot,date,"_recovery_DISASTER2.csv"))
 



# We look the 10 year period after the large disturbances:


d<-damage.all %>% filter(year>49& year<60)


# I want to have a plot where models and winds are there as a range for each mgm

dsum<-d %>% group_by(mgm,year, rcp) %>% summarise(mean.wind=mean(wind), min.wind=min(wind), max.wind=max(wind),
                                                  mean.bb=mean(barkbeetle), min.bb=min(barkbeetle), max.bb=max(barkbeetle))

ylim.bb <- c(0, 40)                                                                                          # in this example, precipitation look the link down
ylim.w <- c(0, 150) 

# This is needed to mantain the proportion in the two axis
b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 

g0<-ggplot(dsum ,aes(year,mean.wind))+
  geom_col(fill="#4d9078",col="black")+
  geom_errorbar(aes(ymin=min.wind, ymax=max.wind), width=.4,) +
  facet_grid(mgm~rcp)+
  geom_line(aes(y = a+ mean.bb*b), data = dsum, size=0.9, col="#f78154")+ 
  geom_ribbon(data = dsum, alpha = 0.4, aes(ymin = a+min.bb*b, ymax = a+max.bb*b), fill="#f78154") +
  scale_y_continuous(name="Wind damage m3/ha", sec.axis = sec_axis(~ (. - a)/b,name = "Bark beetle damage m3/ha"))+
  ggtitle("Absolute damages")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))
print(g0)

# I want to have a plot with total damage  for each case
# First I summarize the damaged volume for each simulation run for the 10 year period which we filtered:

s<-d %>% group_by(mgm,model,windcase, rcp) %>% summarise(wind=sum(wind),bb=sum(barkbeetle)) %>% mutate(total=wind+bb)

# Then I calculate model and windcases minimums, maximums and averages:
smean<-s %>% group_by(mgm, rcp) %>% summarise(mean.wind=mean(wind), 
                                              mean.bb=mean(bb),
                                              min.wind=min(wind),
                                              max.wind=max(wind),
                                              min.bb=min(bb),
                                              max.bb=max(bb))




smean_long<-pivot_longer(smean, cols=c("mean.wind","mean.bb"))


smean_long <- smean %>%
  pivot_longer(
    cols = c(mean.wind, mean.bb, min.wind, max.wind, min.bb, max.bb),
    names_to = c(".value", "variable"),
    names_pattern = "(mean|min|max)\\.(.*)"
  )

# I want 3 plots, one for wind, one for bb and one for total, with the wind-model ranges

g11 <- ggplot(smean_long,
            aes(x = factor(mgm, levels = c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION","UNMANAGED")),
                y = mean,
                fill = variable)) +
  geom_col(position = position_dodge(width = 0.8)) +
  
  # whiskers
  geom_errorbar(aes(ymin = min, ymax = max),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  
  labs(x = "Management",
       y = "Sum of 10 years wind damage m3/ha",
       fill = "Type") +
  
  scale_fill_manual(values = c("bb" = "#f78154", "wind" = "#4d9078")) +
  
  facet_grid(variable~rcp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_rect(fill = "white"))

print(g11)

g1<-ggplot(smean_long ,aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")),
                            y = mean, 
                       fill=variable  ))+
  geom_col()+
  labs(x = "Management",y="Sum of 10 years wind damage m3/ha",fill = "Agent")+
  scale_fill_manual(values=(c("#f78154","#4d9078" )))+
  facet_grid(~rcp)+
  ggtitle("Mean distrubed volume 10years total")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background =element_rect(fill="white"))

print(g1)
# how to translate these values to relative damage?
# compare it to pre.dist value??

lnd.vol<-d %>% filter(year==50) %>% group_by(mgm,rcp) %>% summarise(meanvol=mean(landscape_volume_yearstart))
smean2<-left_join(smean,lnd.vol, by=c("mgm","rcp") ) %>% mutate(perc.wind=100*mean.wind/meanvol, 
                                                                perc.bb=100*mean.bb/meanvol,
                                                                perc.wind.min=100*min.wind/meanvol, 
                                                                perc.bb.min=100*min.bb/meanvol,
                                                                perc.wind.max=100*max.wind/meanvol, 
                                                                perc.bb.max=100*max.bb/meanvol,
                                                                ) %>% dplyr::select(-mean.wind, -mean.bb,-min.wind, -min.bb,-max.wind, -max.bb, -meanvol)


st<-smean2 %>% mutate(total=perc.wind+perc.bb)

ad<-st %>% filter(mgm=="ADAPTATION") %>% mutate(adapt=total) %>% dplyr::select(rcp,adapt) 


egu<-left_join(st,ad,by=c("rcp")) %>% mutate(diff.from.adapt=total-adapt)

egu %>% filter(rcp=="-")
egu %>% filter(rcp=="rcp45")
egu %>% filter(rcp=="rcp85")


smean2_long<-pivot_longer(smean2, cols=c("perc.wind","perc.bb","perc.wind.min","perc.bb.min","perc.wind.max","perc.bb.max"))

smean2_long2<-smean2_long %>% filter(name %in% (c("perc.wind","perc.bb")))

g2<-ggplot(smean2_long2 ,aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")),
                        value, 
                        fill=name  ))+
  geom_col()+
  labs(x = "Management",y="Sum of 10 years wind damage % of standing volume",fill = "Agent")+
  scale_fill_manual(values=(c("#f78154","#4d9078" )))+
  facet_grid(~rcp)+
  ggtitle("Mean distrubed volume 10years total in % of lnd volume")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background =element_rect(fill="white"))



print(g2)

 #################################################################HERE
# add the whiskers case:


g22 <- ggplot(smean2_long,
              aes(x = factor(mgm, levels = c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION","UNMANAGED")),
                  y = mean,
                  fill = variable)) +
  geom_col(position = position_dodge(width = 0.8)) +
  
  # whiskers
  geom_errorbar(aes(ymin = min, ymax = max),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  
  labs(x = "Management",
       y = "Sum of 10 years wind damage % of standing volume",
       fill = "Type") +
  
  scale_fill_manual(values = c("bb" = "#f78154", "wind" = "#4d9078")) +
  
  facet_grid(variable~rcp) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_rect(fill = "white"))







# Recovery times?

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
recovery.all1 %>% filter(rcp=="-", year==49)


g4<-ggplot(recovery.all1 )+
  geom_line(aes( year_after_impact ,r, colour = mgm  , group = interaction(model, windcase)))+
  geom_point(data = df_endpoints, aes(x = rt, y = value, group = interaction(model,windcase)), col="black")+
  geom_errorbarh(data = df_endpoints2, aes(xmin = min1, xmax = max1, y = 10, col=mgm), height = 4, lwd=1) +
  labs(x = "Years after the large windstorm",y="Volume difference compared to pre-dist level %",fill = "Agent")+
  # scale_fill_manual(values=(c("chocolate","grey" )))+
  facet_grid(mgm~rcp)+
  scale_color_manual(values = c("#f2c14e", "chocolate", "black", "#62d75f", "#248721"))+
  ylim(-50,20)+
  geom_hline(yintercept=0, lty=2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background =element_rect(fill="white"))

print(g4)


ii<-df_endpoints %>% group_by(rcp,mgm) %>% summarize(recovery_time=mean(rt))




g5<-ggplot(rec ,aes( mgm,recovery.time , fill=mgm  ))+
  geom_col()+
  labs(x = "Management",y="Recovery time (years)",fill = "mgm")+
 # scale_fill_manual(values=(c("chocolate","grey" )))+
  facet_grid(windcase~rcp+model)+
  scale_fill_manual(values=(c("#f2c14e", "chocolate", "black",  "#62d75f", "#248721" )))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background =element_rect(fill="white"))
print(g5)



#dev.off()


pdf(paste0(plotroot, "2b_Relative_disturbed_volume_",date,"_",version,".pdf"), height = 4, width = 5)
print(g2)
dev.off()
#
pdf(paste0(plotroot, "2b_Recovery_trajectories_",date,"_",version,".pdf"), height = 8, width = 10)     # volume in time!!!
print(g4)
dev.off()

pdf(paste0(plotroot, "2b_Absolute_damages_after_wind_",date,"_",version,".pdf"),height = 8, width = 10)
print(g0)
dev.off()


library(MESS)

# CONTINUE HERE::::::::::::::::::
less<-recovery.all1 %>% select(year_after_impact,model, mgm, rcp, windcase,volume_m3,r)

l1<-less %>% filter(is.na(r)==F) %>% group_by(model, rcp, mgm, windcase) %>%  
  arrange(year_after_impact) %>% 
  summarize(auc=auc(year_after_impact,r)) %>% 
  mutate(norm.auc=auc/-5000.)



l2<-l1 %>% filter(model=="NCC_HIRHAM5",windcase=="w8")

l2





less %>%
  filter(model=="NCC_HIRHAM5",windcase=="w8",!is.na(r)) %>%
  group_by(model, rcp, mgm, windcase) %>%
  arrange(year_after_impact) %>%
  mutate(  auc = auc(year_after_impact, r)) %>%
  ggplot(aes(x = year_after_impact, y = r)) +
  geom_ribbon(aes(ymin = r, ymax = 1), fill = "skyblue", alpha = 0.4) +
  geom_line(color = "forestgreen") +
  facet_grid(rcp~ model + mgm + windcase) +
  labs(    x = "Years after impact",
    y = "% difference of volume compared to pre-dist level"
  ) +theme_bw()+
  xlim(0,50)+ 

  geom_text(    data = l2,
    aes(x = Inf,              # position at right edge
      y = Inf,              # position at top
      label =  round(norm.auc, 2)),
    hjust = 1.1, vjust = 1.5,
    color = "blue", size = 3.5,
    inherit.aes = FALSE  )+
  xlim(0,50)+ylim(-100,0)


#I want to put them all into one plot, coloring the resilience indicator


less<-left_join(less,l1,by=c("rcp", "model" , "mgm" , "windcase"))


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

# which are the most resilient ones???
ggplot(l1, aes(x = mgm, y = 1-norm.auc, fill = model)) +
  geom_boxplot(width=0.2) +
  facet_grid(model~rcp) +
  theme_bw() +
  ylim(0.7,1)+
  labs(x = "MGM", y = "Normalized AUC", title = "Normalized AUC by MGM and RCP")

ggplot(l1, aes(x = mgm, y = 1-norm.auc, fill = rcp)) +
  geom_boxplot(width=0.5) +
  theme_bw() +
  ylim(0.7,1)+
  labs(x = "MGM", y = "Normalized AUC", title = "Normalized AUC by MGM and RCP")





