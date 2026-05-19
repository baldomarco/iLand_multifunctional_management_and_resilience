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

a1.long<-a1.long %>% group_by(mgm,rcp,name) %>% summarize(med=median(value), upper=max(value),lower=min(value))
                                       

b1.long <- b %>% separate(    X,    into = c("tmp", "group"),    sep = "="  ) %>%  
           select(-tmp) %>%  
           separate(    group,    into = c("rcp", "management"),    sep = "\\."  ) %>%
          select(rcp,management,median.ceiling) %>%
          rename(mgm=management,med=median.ceiling) %>%
          mutate(name="Est.med.recovery.time")



d<-rbind(a1.long,b1.long)


dd<-d %>%
  mutate(
    rcp = factor(rcp, levels = c("refclim", "rcp45", "rcp85")),
    name = factor(name,levels = c("impact", "Est.med.recovery.time", "one.minus.norm.auc")))  
  
g1<-ggplot(dd,aes(x = mgm, y = med, fill = rcp)) +
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

pdf(paste0(plotroot,"2f_Column_graph_absolute_impact_rt_res.pdf"), width=10,height=6)
print(g1)
dev.off()
#---------------------------- SPIDER GRAPH

d_plot <- d %>%
  mutate(
    rcp  = factor(rcp, levels = c("refclim", "rcp45", "rcp85")),
    name = factor(name, levels = c("impact", "Est.med.recovery.time", "one.minus.norm.auc")),
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

g2<-ggRadar(data=d_wide ,mapping = aes(colour = rcp, facet=name), 
        rescale = FALSE, interactive = FALSE, use.label = TRUE, size = 2,alpha=0.1,
        legend.position = "right", scales="free") +theme_bw()+
  ggtitle("")+
  scale_fill_manual(values=(c("#3a6ea5","#f2c14e","#f78154" )))+
  scale_color_manual(values=(c("#3a6ea5","#f2c14e","#f78154" )))+
  theme(strip.background =element_rect(fill="white"))


pdf(paste0(plotroot,"2f_Spider_graph_absolute_impact_rt_res.pdf"), width=10,height=6)
print(g2)
dev.off()





#--------------------------- CC differences



head(d)

