#----------------------------------------------------------------------------------2025/2026
# Laura Dobor, CZU, dobor@fld.czu.cz
# Study: iLand modeling management and resilience on Kostelec area
# 2026.04.20.
#
#     ANALYSES AND VISUALIZATION
#
#
#----------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)   





setwd("D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/")

dataroot<-"Output_summary_tables/"
plotroot<-"Figures/"


version<-"DISASTER2"
date<-"2025-04-18"
damage.all<-read.csv(paste0(dataroot,date,"_damages_DISASTER2.csv"))
recovery.all<-read.csv( paste0(dataroot,date,"_recovery_DISASTER2.csv"))

