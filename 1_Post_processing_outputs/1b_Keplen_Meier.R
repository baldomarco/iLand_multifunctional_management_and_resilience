library(tidyverse)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Load data

df<- read_csv("C:/Users/baldo/Documents/GitHub/iLand_management_and_resilience/Output_summary_tables/20260421_impact_recoverytime_auc.csv")

# ── 0. Parameters ─────────────────────────────────────────────────────────────
sim_horizon <- 50

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

mgm_cols <- c(
  "#f2c14e",
  "chocolate",
  "black",
  "#62d75f",
  "#248721"
)

rcp_cols <- c("#3a6ea5","#f2c14e","#f78154" )

a<-df %>% select(mgm, model, windcase,rcp, rt_imp, is_censored)
head(a)

df2 <- a %>%  mutate(  event = ifelse(is_censored, 0, 1)  )


df2<-df2 %>%
  mutate(
    rcp = factor(rcp, levels = c("refclim", "rcp45", "rcp85")),
    mgm = factor(mgm,levels = c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED")))  



surv_obj <- Surv(  time  = df2$rt_imp,  event = df2$event)

#  MANAGEMENT EFFECT ONLY:

km_mgm <- survfit(  surv_obj ~ mgm,  data = df2)

g1<-ggsurvplot(  km_mgm,  data = df2,  risk.table = TRUE,  pval = TRUE,
  conf.int = TRUE,  xlab = "Years after disturbance", 
  palette = mgm_cols, lwd=1.5,
  ylab = "Probability of recovering",
  fun = "event",
  legend.title = "Management",  ggtheme = theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )  )

#  RCP EFFECT ONLY:
km_rcp <- survfit(  surv_obj ~ rcp   ,  data = df2)

g2<-ggsurvplot(  km_rcp,  data = df2,  risk.table = TRUE,  pval = TRUE,
                 conf.int = TRUE,  xlab = "Years after disturbance", 
                 palette = rcp_cols, lwd=1.5,
                 ylab = "Probability of recovering",
                 fun = "event",
                 legend.title = "Management",  ggtheme = theme_bw() +
                   theme(
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()
                   )  )




# COMBINED MGM RCP EFFECT:
# here we have too small number of simulations and many of them are censored. So Confidence intervals failing. But I still want to keep the median lines.
# Plot them.


df2<-df2 %>%
  mutate(
    rcp = factor(rcp, levels = c("refclim", "rcp45", "rcp85")),
    mgm = factor(mgm,levels = c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED")))  


# create interaction variable
df2$group <- interaction(df2$rcp, df2$mgm)

# fit KM
km_rcp_mgm <- survfit(surv_obj ~ group, data = df2)

#median: the time when 50% of simulations have recovered.
km_df <- data.frame(km_rcp_mgm$n)


km_df <- data.frame(
  group = names(km_rcp_mgm$strata),
  n = km_rcp_mgm$n,
  events = km_rcp_mgm$n.event,
  median = summary(km_rcp_mgm)$table[, "median"],
  lower95 = summary(km_rcp_mgm)$table[, "0.95LCL"],
  upper95 = summary(km_rcp_mgm)$table[, "0.95UCL"]
)

km_table<-as.data.frame(summary(km_rcp_mgm)$table)
km_table<- km_table %>% mutate(median.ceiling =ceiling(km_table$median))


write.csv(km_table,"C:/Users/baldo/Documents/GitHub/iLand_management_and_resilience/Output_summary_tables/1b_Median_recovery_timings.csv")




df2 <- df2 %>%  mutate(  rcp_facet = factor(      rcp,      levels = c("refclim", "rcp45", "rcp85"),      labels = c("1_refclim", "2_rcp45", "3_rcp85") ) )

km_rcp_mgm <- survfit(surv_obj ~ rcp + mgm, data = df2)


# extract survfit summary
ss <- surv_summary( survfit(surv_obj ~ mgm + rcp_facet, data = df2),  data = df2)

# for event curves: event = 1 - surv
ss$event <- 1 - ss$surv

# first time reaching 100%
full_recovery <- ss %>%
  group_by(strata) %>%
  filter(event >= 1) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    rcp_facet = str_extract(strata,
                            "1_refclim|2_rcp45|3_rcp85")
  )

full_recovery


p <- ggsurvplot_facet(
  km_rcp_mgm,
  data = df2,
  facet.by = "rcp_facet",
  conf.int = FALSE,
  risk.table = FALSE,
  fun = "event",
  palette = rep(mgm_cols,3),
  lwd = 1.5,
  ggtheme = theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ),
  surv.median.line = "hv",
  xlab = "Years after disturbance",
  ylab = "Probability of recovering",
  legend.title = "Management"
)


p$plot <- p$plot +

  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_wrap(
    ~rcp_facet,
    labeller = labeller(
      rcp_facet = c(
        "1_refclim" = "refclim",
        "2_rcp45" = "rcp45",
        "3_rcp85" = "rcp85"
      )
    )
      
  )

plotroot<-"C:/Users/baldo/Documents/GitHub/iLand_management_and_resilience/Figures/"

plot1<-paste0(plotroot,"1b_Keplen_Meier_interaction_median_only.pdf")
plot2<-paste0(plotroot,"1b_Keplen_Meier_mgm.pdf")
plot3<-paste0(plotroot,"1b_Keplen_Meier_rcp.pdf")




pdf(plot1, height=6,width=12)
print(p)
dev.off()


pdf(plot2, height=8,width=10)
print(g1)
dev.off()



pdf(plot3, height=8,width=10)
print(g2)
dev.off()


write.csv(full_recovery,"C:/Users/baldo/Documents/GitHub/iLand_management_and_resilience/Output_summary_tables/1b_Full_recovery_timings.csv")

fastest.fullrecov<-min(full_recovery$time)


# WE DECIDED NOT TO GO THIS DIRECTION rather use the median recovery timings.
# extract KM summary at time = 23
#km23 <- summary(km_rcp_mgm, times = 23)

# build table
#recovery_23 <- data.frame(
#  strata = km23$strata,
#  time = km23$time,
#  survival = km23$surv,
#  recovery = 1 - km23$surv,
#  lower_ci = 1 - km23$upper,
#  upper_ci = 1 - km23$lower,
#  n_risk = km23$n.risk,
#  n_event = km23$n.event
#)



#recovery_23
#write.csv(recovery_23,"D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/Output_summary_tables/1b_Recovery_probability_at_y23.csv")

summary(km_rcp_mgm, times=23)



















