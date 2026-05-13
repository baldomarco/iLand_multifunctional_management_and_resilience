library(tidyverse)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Load data
#df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")
df<- read_csv("D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/Output_summary_tables/20260421_impact_recoverytime_auc.csv")

# ── 0. Parameters ─────────────────────────────────────────────────────────────
sim_horizon <- 50


management_colors <- c(
  "ADAPTATION"   = "#E69F00",
  "BAU"          = "#185FA5",
  "BIOECONOMY"   = "#1D9E75",
  "CONSERVATION" = "#D85A30",
  "UNMANAGED"    = "#787670"
)

# ── 1. Recode & flag non-recoverers ──────────────────────────────────────────
df <- df |>
  mutate(
    rcp       = recode(rcp, `-` = "refclim"),
    group_key = paste(mgm, rcp, model, windcase, sep = "_"),
    # TRUE for the 27 cases that never recovered
    is_censored = is.infinite(rt) | is.na(rt)
  )

# Sanity check: confirm censored cases are clustered in rcp45
df |>
  filter(is_censored) |>
  count(rcp, mgm) |>
  print()

# ── 2. Impute rt — primary & sensitivity ─────────────────────────────────────
# Primary: rt_imp = 50 for non-recoverers
# Sensitivity: rt_imp_sens = 75 for non-recoverers
df <- df |>
  mutate(
    rt_imp      = if_else(is_censored, as.numeric(sim_horizon),    rt)
  )

# Quick summary: compare distributions with and without imputation
cat("\n── rt distribution summary (recovered cases only) ──\n")
df |> filter(!is_censored) |> summarise(
  n    = n(),
  mean = mean(rt),
  sd   = sd(rt),
  med  = median(rt),
  min  = min(rt),
  max  = max(rt)
) |> print()

cat("\n── rt_imp distribution summary (all cases, primary imputation) ──\n")
df |> summarise(
  n           = n(),
  n_imputed   = sum(is_censored),
  mean        = mean(rt_imp),
  sd          = sd(rt_imp),
  med         = median(rt_imp),
  min         = min(rt_imp),
  max         = max(rt_imp)
) |> print()




a<-df %>% select(mgm, model, windcase,rcp, rt_imp, is_censored)
head(a)

df2 <- a %>%  mutate(  event = ifelse(is_censored, 0, 1)  )

surv_obj <- Surv(  time  = df2$rt_imp,  event = df2$event)

#  MANAGEMENT EFFECT:

km_mgm <- survfit(  surv_obj ~ mgm,  data = df2)

ggsurvplot(  km_mgm,  data = df2,  risk.table = TRUE,  pval = TRUE,
  conf.int = TRUE,  xlab = "Years after disturbance",  
  ylab = "Probability of NOT yet recovering",
  legend.title = "Management",  ggtheme = theme_bw())

#  RCP EFFECT:
km_rcp <- survfit(  surv_obj ~ rcp   ,  data = df2)

ggsurvplot( km_rcp, data = df2, risk.table = TRUE,pval = TRUE,  conf.int = TRUE,
  xlab = "Years after disturbance",
  ylab = "Probability of NOT yet recovering",
  legend.title = "Management",
  ggtheme = theme_bw() )




summary(km_rcp, times = 23)
# Which recovers faster?
# Look at t specific year


summary(km_mgm, times = 23)


km23 <- summary(km_mgm, times = 23)







# CHANGE TO PROBABILITY OF RECOVERY: (fun="event")

ggsurvplot(  km_mgm,  data = df2,  risk.table = TRUE,  pval = TRUE,fun = "event",
             conf.int = TRUE,  xlab = "Years after disturbance",  
             ylab = "Probability of recovering",
             legend.title = "Management",  ggtheme = theme_bw())



ggsurvplot( km_rcp, data = df2, risk.table = TRUE,pval = TRUE,  conf.int = TRUE,fun = "event",
            xlab = "Years after disturbance",
            ylab = "Probability of recovering",
            legend.title = "Management",
            ggtheme = theme_bw() )





















