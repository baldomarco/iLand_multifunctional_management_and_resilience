#----------------------------------------------------------------------------------2025/2026
# Laura Dobor, CZU, dobor@fld.czu.cz
# Study: iLand modeling management and resilience on Kostelec area
# 2026.05.04.
#
#     ANALYSES AND VISUALIZATION
#
#
#----------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)   
library(readxl)




setwd("D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/")

dataroot<-"Output_summary_tables/"
plotroot<-"Figures/"


a<-read.csv(paste0(dataroot,"20260421_impact_recoverytime_auc.csv"))
b<-read.csv(paste0(dataroot,"1b_Median_recovery_timings.csv"))

head(b)



a1<-a %>% select(mgm,model,windcase,rcp,impact,one.minus.norm.auc)
a1.long<-pivot_longer(a1, cols = c("impact", "one.minus.norm.auc"))

a1.long$rcp[which(a1.long$rcp=="-")]<-"refclim"


a1.long.ref<-a1.long %>% filter(rcp=='refclim') %>% rename(refvalue=value) %>% select(-rcp, -model)
a1.long.scen<-a1.long %>% filter(rcp!='refclim') 

diff<-left_join(a1.long.scen,a1.long.ref, by=c("mgm","windcase", "name")) %>% mutate(diff=value-refvalue,
                                                                                     diff.perc= 100*(value-refvalue)/refvalue)

sumdat <- diff %>%
  group_by(mgm, name, rcp) %>%
  summarise(
    mean_diff = mean(diff.perc, na.rm = TRUE),
    q25 = quantile(diff.perc, 0.25, na.rm = TRUE),
    q75 = quantile(diff.perc, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
#---------------- median recovery times


sumdat %>% filter(mgm=="ADAPTATION")

b1.long <- b %>% separate(    X,    into = c("tmp", "group"),    sep = "="  ) %>%  
  select(-tmp) %>%  
  separate(    group,    into = c("rcp", "management"),    sep = "\\."  ) %>%
  select(rcp,management,median.ceiling) %>%
  rename(mgm=management,med=median.ceiling) %>%
  mutate(name="Est.med.recovery.time")


b1.long.ref<-b1.long %>% filter(rcp=="refclim") %>% mutate(refvalue=med)  %>% select(-rcp,-med)

b1.long.scen<-b1.long %>% filter(rcp!="refclim") %>% mutate(value=med) %>% select(-med)



diff2<-left_join(b1.long.scen,b1.long.ref, by=c("mgm", "name")) %>% mutate(diff=value-refvalue, diff.perc= 100*(value-refvalue)/refvalue)

diff3<-diff2 %>%  select(mgm,name,rcp,diff.perc) %>% rename(mean_diff=diff.perc) %>% mutate(q25=NA,q75=NA)


diff2 %>% filter(mgm=="ADAPTATION")

dim(diff3)
dim(sumdat)

sumdat<-rbind(sumdat,diff3)

g3<-ggplot(sumdat, aes(x = name, y = mean_diff, fill = rcp)) +
  scale_fill_manual(values=(c( "#5fad56", "#f2c14e")))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(
    aes(ymin = q25, ymax = q75),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  facet_wrap(~ mgm, nrow = 1) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Variable",
    y = "Difference in %",
    fill = "RCP"
  )


pdf(paste0(plotroot,"2g_Column_graph_perc_diff_rt_impact_res.pdf"), width=10,height=6)
print(g3)
dev.off()








a1.long<-a1.long %>% group_by(mgm,rcp,name) %>% summarize(med=median(value), upper=max(value),lower=min(value))
                                       

c1.long<-c %>% select(Management,Climate,recovery,recovery_upper,recovery_lower) %>%rename(mgm=Management,rcp=Climate,med=recovery,upper=recovery_upper,lower=recovery_lower) %>% mutate(name="recovery.prob")

d<-rbind(a1.long,c1.long)


dd<-d %>%
  mutate(
    rcp = factor(rcp, levels = c("refclim", "rcp45", "rcp85")),
    name = factor(name,levels = c("impact", "recovery.prob", "one.minus.norm.auc")))  
  
ggplot(dd,aes(x = mgm, y = med, fill = rcp)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper),  position = position_dodge(width = 0.7),   width = 0.2  ) +
  facet_wrap(~ name, scales = "free_y", nrow = 1) +
  theme_minimal() +
  labs(   x = "MGM",  y = "Value",  fill = "Climate scenario" )+
 scale_fill_manual(values=(c("#3a6ea5","#f2c14e","#f78154" )))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))



#---------------------------- SPIDER GRAPH

d_plot <- d %>%
  mutate(
    rcp  = factor(rcp, levels = c("refclim", "rcp45", "rcp85")),
    name = factor(name, levels = c("impact", "recovery.prob", "one.minus.norm.auc")),
    mgm  = factor(mgm, levels = c("ADAPTATION", "BAU","BIOECONOMY","CONSERVATION","UNMANAGED"))
  ) %>%
  arrange(rcp, name, mgm)


library(ggradar)
library(ggiraphExtra)
library(cowplot)
library(ggradar)

d_wide <- d_plot %>%
  ungroup() %>%
  select(mgm, rcp, name, med) %>%
  pivot_wider(
    names_from = mgm,
    values_from = med
  )

ggRadar(data=d_wide ,mapping = aes(colour = rcp, facet=name), 
        rescale = FALSE, interactive = FALSE, use.label = TRUE, size = 2,alpha=0.1,
        legend.position = "right", scales="free") +theme_bw()+
  ggtitle("Median of variables across simulations")+
  scale_fill_manual(values=(c("#3a6ea5","#f2c14e","#f78154" )))+
  scale_color_manual(values=(c("#3a6ea5","#f2c14e","#f78154" )))+
  theme(strip.background =element_rect(fill="white"))



#--------------------------- CC differences



head(d)

