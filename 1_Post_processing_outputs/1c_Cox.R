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



df <- df %>%  mutate(  event = ifelse(is_censored, 0, 1)  )




#-------------------------------------------------------------- SET REFERENCE CASES:
# For the Cox model need to set baseline which they compare things: 
# Compared to BAU, how does each management change recovery rate?
# Compared to REFLCIM, how does each rcp change recovery rate?

df$mgm <- relevel(as.factor(df$mgm), ref = "BAU")
df$rcp <- relevel(as.factor(df$rcp), ref = "refclim")

# the first one here is the reference:
levels(as.factor(df$mgm))
levels(as.factor(df$rcp))
#-------------------------------------------------------------------------------------------
surv_obj <- Surv( time = df$rt_imp,  event = df$event)


# --- FIT AN ADDITIVE MODEL:

#This estimates:
## management effects adjusted for climate
## climate effects adjusted for management

cox1 <- coxph(  surv_obj ~ mgm + rcp ,  data = df)



# --- FIT INTERACTION MODEL
# Does management effectiveness change under climate change?
cox2 <- coxph(surv_obj ~ mgm * rcp ,  data = df)


summary(cox1)
summary(cox2)


#---- COMAPRE THE MODELS:
#If interaction improves model substantially → climate modifies management effectiveness.
#If not → management and climate effects are mostly additive.

AIC(cox1, cox2)
anova(cox1, cox2, test = "LRT")

# PRoportional hazards assumption Check proportional hazards
cox.zph(cox2)


#Non-significant p-values --> PH assumption OK.

#Significant p-values -->Effects change over time.
#Ecologically this may mean:  management helps early but not late recovery.


#---------------------------- GRAPHICAL OPTIONS:

library(broom)

#--- COX1

hr_df1 <- broom::tidy(cox1, exponentiate = TRUE, conf.int = TRUE)
ggplot(hr_df1,
       aes(x = estimate,
           y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low,      xmax = conf.high),     height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed",        color = "red") +
  scale_x_log10() +
  labs(   x = "Hazard ratio (log scale)",   y = "",   title = "Effects on recovery rate") +
  theme_bw(base_size = 14)

#--- COX2
hr_df2 <- broom::tidy(cox2, exponentiate = TRUE, conf.int = TRUE)
ggplot(hr_df2,
       aes(x = estimate,
           y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low,    xmax = conf.high),         height = 0.2) +
  geom_vline(xintercept = 1,       linetype = "dashed",            color = "red") +
  scale_x_log10() +
  labs(  x = "Hazard ratio (log scale)",  y = "",   title = "Effects on recovery rate"  ) +
  theme_bw(base_size = 14)




#-------------------------------------------- Interaction plot
newdat <- expand.grid(
  mgm = levels(df$mgm),
  rcp = levels(df$rcp)
)

sf <- survfit(cox2, newdata = newdat)
med <- surv_median(sf)
med$mgm <- newdat$mgm
med$rcp <- newdat$rcp

ggplot(med,   aes(x = rcp,           y = median,           color = mgm,           group = mgm)) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  
  scale_color_manual(values = management_colors) +
  
  labs(    y = "Predicted recovery time",
    x = "Climate scenario"
  ) +
  
  theme_bw(base_size = 14)






#------------------------------ maybe these are below not meaningful... need to check it


sf <- survfit(cox1, newdata = newdat)
med <- surv_median(sf)
med$mgm <- newdat$mgm
med$rcp <- newdat$rcp

ggplot(med,   aes(x = rcp,           y = median,           color = mgm,           group = mgm)) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  
  scale_color_manual(values = management_colors) +
  
  labs(    y = "Predicted recovery time",
           x = "Climate scenario"
  ) +
  
  theme_bw(base_size = 14)

#---------- ???For diagnostics, visualize Schoenfeld residuals:
ph_test <- cox.zph(cox2)

ggcoxzph(ph_test)
