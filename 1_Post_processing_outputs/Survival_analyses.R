library(tidyverse)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Load data
df <- read_csv("C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/20260421_impact_recoverytime_auc.csv")

# ── 0. Parameters ─────────────────────────────────────────────────────────────
sim_horizon <- 50
penalty_primary    <- 50   # primary imputation:  rt = sim_horizon (boundary)
penalty_sensitivity <- 75  # sensitivity check:   rt = 75 (beyond window)

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
    rt_imp      = if_else(is_censored, as.numeric(penalty_primary),    rt),
    rt_imp_sens = if_else(is_censored, as.numeric(penalty_sensitivity), rt)
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

# ── 3. Long format for ANOVA/discrimination plot ──────────────────────────────
# Mirrors the structure of your existing impact / one.minus.norm.auc analysis
# Three resilience variables: impact, one.minus.norm.auc, rt_imp
df_long <- df |>
  select(mgm, rcp, model, windcase, group_key, is_censored,
         impact, one.minus.norm.auc, rt_imp, rt_imp_sens) |>
  pivot_longer(
    cols      = c(impact, one.minus.norm.auc, rt_imp),
    names_to  = "variable",
    values_to = "value"
  ) |>
  mutate(
    Management = factor(mgm),
    Climate    = factor(rcp)
  )

# ── 4. Compute mean differences vs. reference climate (refclim) ──────────────
# Same logic as your existing discrimination plot
ref_means <- df_long |>
  filter(rcp == "refclim") |>
  group_by(mgm, variable) |>
  summarise(ref_mean = mean(value, na.rm = TRUE), .groups = "drop")

mean_diffs <- df_long |>
  filter(rcp != "refclim") |>
  group_by(mgm, rcp, variable) |>
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    se_val   = sd(value, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) |>
  left_join(ref_means, by = c("mgm", "variable")) |>
  mutate(
    mean_diff = mean_val - ref_mean,
    ci_low    = mean_diff - 1.96 * se_val,
    ci_high   = mean_diff + 1.96 * se_val
  )

# ── 5. Sensitivity check: repeat with rt_imp_sens ────────────────────────────
df_long_sens <- df |>
  select(mgm, rcp, model, windcase, is_censored,
         impact, one.minus.norm.auc, rt_imp_sens) |>
  rename(rt_imp = rt_imp_sens) |>          # reuse same column name for pivot
  pivot_longer(
    cols      = c(impact, one.minus.norm.auc, rt_imp),
    names_to  = "variable",
    values_to = "value"
  ) |>
  mutate(Management = factor(mgm), Climate = factor(rcp))

ref_means_sens <- df_long_sens |>
  filter(rcp == "refclim") |>
  group_by(mgm, variable) |>
  summarise(ref_mean = mean(value, na.rm = TRUE), .groups = "drop")

mean_diffs_sens <- df_long_sens |>
  filter(rcp != "refclim") |>
  group_by(mgm, rcp, variable) |>
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    se_val   = sd(value, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) |>
  left_join(ref_means_sens, by = c("mgm", "variable")) |>
  mutate(
    mean_diff = mean_val - ref_mean,
    ci_low    = mean_diff - 1.96 * se_val,
    ci_high   = mean_diff + 1.96 * se_val,
    imputation = "sensitivity (rt=75)"
  )

mean_diffs <- mean_diffs |> mutate(imputation = "primary (rt=50)")

# Combined for sensitivity comparison
mean_diffs_combined <- bind_rows(mean_diffs, mean_diffs_sens)

# ── 6. Main discrimination plot (primary imputation) ─────────────────────────
# Replicates your existing plot style, now with rt_imp as third variable
variable_labels <- c(
  "impact"           = "Impact",
  "one.minus.norm.auc" = "one.minus.norm.auc",
  "rt_imp"           = "Recovery time (rt)"
)

plot_primary <- ggplot(
  mean_diffs |> mutate(variable = recode(variable, !!!variable_labels)),
  aes(x = variable, y = mean_diff, fill = rcp)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(width = 0.7),
    width = 0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  # Flag that rt non-recoverers are imputed
  annotate("text", x = 3, y = Inf, vjust = 1.5, hjust = 0.5,
           label = "rt non-recoverers\nimputed at 50 yr",
           size = 2.5, color = "grey40", fontface = "italic") +
  scale_fill_manual(
    values = c("rcp45" = "#56B4E9", "rcp85" = "#E69F00"),
    labels = c("rcp45" = "RCP 4.5", "rcp85" = "RCP 8.5")
  ) +
  facet_grid(variable ~ mgm, scales = "free_y", switch = "y") +
  labs(
    title = "Mean difference from reference climate — Impact, AUC, Recovery Time",
    x     = "Variable",
    y     = "Mean difference",
    fill  = "RCP",
    caption = paste0(
      "Note: 27 non-recovering cases under RCP4.5 imputed at rt = ",
      penalty_primary, " yr (simulation horizon).\n",
      "Sensitivity analysis with rt = ", penalty_sensitivity,
      " yr available in plot_sensitivity object."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey90", color = "black"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.5),
    plot.caption     = element_text(size = 8, color = "grey40", hjust = 0)
  )

print(plot_primary)

# ── 7. Sensitivity comparison plot ───────────────────────────────────────────
# Shows only rt_imp to highlight sensitivity of imputation choice
plot_sensitivity <- mean_diffs_combined |>
  filter(variable == "rt_imp") |>
  mutate(variable = recode(variable, !!!variable_labels)) |>
  ggplot(aes(x = rcp, y = mean_diff, fill = imputation)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(width = 0.7),
    width = 0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c(
    "primary (rt=50)"      = "#009E73",
    "sensitivity (rt=75)"  = "#CC79A7"
  )) +
  facet_wrap(~ mgm, nrow = 1) +
  labs(
    title   = "Sensitivity analysis — effect of imputation value on Recovery Time (rt)",
    x       = "Climate scenario",
    y       = "Mean difference from reference",
    fill    = "Imputation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey90", color = "black"),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.5)
  )

print(plot_sensitivity)

# ── 8. Export imputed dataset for downstream use (survival analysis etc.) ─────
# This is the clean version of df ready for your Cox/AFT pipeline
df_final <- df |>
  select(mgm, rcp, model, windcase, group_key,
         impact, one.minus.norm.auc,
         rt, rt_imp, rt_imp_sens, is_censored)

# Uncomment to save:
# write_csv(df_final, "C:/Users/baldo/Documents/GitHub/RESONATE_WP4/multifunctionality/ANOVA_discrimination_analysis/df_with_rt_imputed.csv")