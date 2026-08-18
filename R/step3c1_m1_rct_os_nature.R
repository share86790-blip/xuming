#!/usr/bin/env Rscript

# Step 3C-1 | M1 RCT-OS | Nature-R
# R-only rendering. Statistical/visual values are read from the frozen source CSV.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(metafor)
  library(digest)
  library(svglite)
  library(ragg)
})

INPUT <- "data/M1_RCT_OS_Nature_source.csv"
OUTDIR <- "outputs"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

EXPECTED_CSV_SHA256 <- "482cdaa0f1d21f20f59356d75f2b4a6d222d994d0cfaaa2db18ef9800a3bae80"
actual_sha <- digest(file = INPUT, algo = "sha256", serialize = FALSE)
stopifnot(identical(tolower(actual_sha), tolower(EXPECTED_CSV_SHA256)))

dat <- read_csv(INPUT, show_col_types = FALSE)

required_cols <- c(
  "Result_ID","Study","Year","Design","Backbone","Comparison","Outcome",
  "Analysis_Set","Nature_Role","HR","CI_Lower","CI_Upper","logHR","SE_logHR",
  "N_Intervention","N_Comparator","Final_RoB","Nature_Plot_Status"
)
stopifnot(all(required_cols %in% names(dat)))
stopifnot(identical(dat$Result_ID, c("R04","R18")))
stopifnot(nrow(dat) == 2L)
stopifnot(all(dat$Design == "RCT"))
stopifnot(all(dat$Outcome == "OS"))
stopifnot(all(dat$Backbone == "TACE"))
stopifnot(all(dat$Analysis_Set == "Primary"))
stopifnot(all(dat$Nature_Role == "MAIN_POOLED_M1"))
stopifnot(all(dat$Nature_Plot_Status == "READY_FOR_NATURE_PLOT"))
stopifnot(all(is.finite(dat$HR) & is.finite(dat$CI_Lower) & is.finite(dat$CI_Upper)))
stopifnot(all(dat$HR > 0 & dat$CI_Lower > 0 & dat$CI_Upper > 0))
stopifnot(all(dat$CI_Lower < dat$HR & dat$HR < dat$CI_Upper))

# Recompute transformations from the frozen published HR/CI and verify the locked values.
z975 <- qnorm(0.975)
dat <- dat %>%
  mutate(
    logHR_check = log(HR),
    SE_check = (log(CI_Upper) - log(CI_Lower)) / (2 * z975),
    vi = SE_logHR^2,
    N_total = N_Intervention + N_Comparator
  )
stopifnot(max(abs(dat$logHR_check - dat$logHR)) < 3e-5)
stopifnot(max(abs(dat$SE_check - dat$SE_logHR)) < 3e-5)

# REML random-effects: Wald and HKSJ are both retained.
fit_wald <- rma.uni(
  yi = logHR,
  vi = vi,
  data = dat,
  method = "REML",
  test = "z",
  slab = Study
)
fit_hksj <- rma.uni(
  yi = logHR,
  vi = vi,
  data = dat,
  method = "REML",
  test = "hksj",
  slab = Study
)
stopifnot(fit_wald$k == 2L)
stopifnot(abs(fit_wald$tau2 - fit_hksj$tau2) < 1e-12)

study_weight <- 100 * weights(fit_wald) / sum(weights(fit_wald))

model_results <- tibble(
  Method = c("REML-Wald", "REML-HKSJ"),
  Pooled_HR = exp(c(as.numeric(coef(fit_wald)), as.numeric(coef(fit_hksj)))),
  CI_Lower = exp(c(fit_wald$ci.lb, fit_hksj$ci.lb)),
  CI_Upper = exp(c(fit_wald$ci.ub, fit_hksj$ci.ub)),
  P_value = c(fit_wald$pval, fit_hksj$pval),
  k = fit_wald$k,
  tau2 = fit_wald$tau2,
  I2_pct = fit_wald$I2,
  Q = fit_wald$QE,
  Q_p = fit_wald$QEp
)

study_results <- dat %>%
  mutate(Weight_pct = study_weight) %>%
  select(Result_ID, Study, N_total, HR, CI_Lower, CI_Upper, Weight_pct)

write_csv(model_results, file.path(OUTDIR, "M1_model_results.csv"))
write_csv(study_results, file.path(OUTDIR, "M1_study_results.csv"))

# Nature figure contract: clinical quantitative forest, white background, restrained neutral palette.
palette_contract <- c(
  neutral_dark = "#272727",
  neutral_mid = "#767676",
  pale_band = "#F4F4F4"
)

theme_nature_contract <- function(base_size = 6.5, base_family = "Liberation Sans") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = element_line(linewidth = 0.35, colour = "black"),
      axis.title = element_text(size = 6.5),
      axis.text = element_text(size = 6),
      plot.title = element_text(size = 7, face = "bold"),
      plot.caption = element_text(size = 5.5, hjust = 0),
      panel.grid = element_blank(),
      plot.margin = margin(2, 2, 2, 2, unit = "mm")
    )
}
theme_set(theme_nature_contract())

plot_rows <- tibble(
  row_id = c("R04","R18","Wald","HKSJ"),
  label = c(
    dat$Study[dat$Result_ID == "R04"],
    dat$Study[dat$Result_ID == "R18"],
    "REML pooled — Wald",
    "REML pooled — HKSJ"
  ),
  N_label = c(
    paste0("n=", dat$N_total[dat$Result_ID == "R04"]),
    paste0("n=", dat$N_total[dat$Result_ID == "R18"]),
    "", ""
  ),
  HR = c(
    dat$HR[dat$Result_ID == "R04"],
    dat$HR[dat$Result_ID == "R18"],
    model_results$Pooled_HR[model_results$Method == "REML-Wald"],
    model_results$Pooled_HR[model_results$Method == "REML-HKSJ"]
  ),
  lo = c(
    dat$CI_Lower[dat$Result_ID == "R04"],
    dat$CI_Lower[dat$Result_ID == "R18"],
    model_results$CI_Lower[model_results$Method == "REML-Wald"],
    model_results$CI_Lower[model_results$Method == "REML-HKSJ"]
  ),
  hi = c(
    dat$CI_Upper[dat$Result_ID == "R04"],
    dat$CI_Upper[dat$Result_ID == "R18"],
    model_results$CI_Upper[model_results$Method == "REML-Wald"],
    model_results$CI_Upper[model_results$Method == "REML-HKSJ"]
  ),
  weight = c(study_weight[1], study_weight[2], NA_real_, NA_real_),
  y = c(4, 3, 1.8, 1)
) %>%
  mutate(
    effect_text = if_else(
      row_id %in% c("R04","R18"),
      sprintf("%.2f (%.3f–%.3f)", HR, lo, hi),
      sprintf("%.3f (%.3f–%.3f)", HR, lo, hi)
    ),
    weight_text = if_else(is.na(weight), "", sprintf("%.1f%%", weight))
  )

study_rows <- plot_rows %>% filter(row_id %in% c("R04","R18"))
pooled_rows <- plot_rows %>% filter(row_id %in% c("Wald","HKSJ"))
pooled_wald <- plot_rows %>% filter(row_id == "Wald")
pooled_hksj <- plot_rows %>% filter(row_id == "HKSJ")

p_left <- ggplot(plot_rows, aes(y = y)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1.55, ymax = 2.05,
           fill = palette_contract["pale_band"], colour = NA) +
  geom_text(data = study_rows, aes(x = 0, label = label),
            hjust = 0, size = 6.5 / ggplot2::.pt, family = "Liberation Sans") +
  geom_text(data = pooled_rows, aes(x = 0, label = label),
            hjust = 0, size = 6.5 / ggplot2::.pt, family = "Liberation Sans", fontface = "bold") +
  geom_text(aes(x = 0.98, label = N_label),
            hjust = 1, size = 5.8 / ggplot2::.pt,
            family = "Liberation Sans", colour = palette_contract["neutral_mid"]) +
  annotate("text", x = 0, y = 4.65, label = "Study",
           hjust = 0, size = 6.2 / ggplot2::.pt,
           family = "Liberation Sans", fontface = "bold") +
  annotate("text", x = 0.98, y = 4.65, label = "Sample size",
           hjust = 1, size = 6.2 / ggplot2::.pt,
           family = "Liberation Sans", fontface = "bold") +
  coord_cartesian(xlim = c(0,1), ylim = c(0.55,4.9), clip = "off") +
  theme_void(base_family = "Liberation Sans") +
  theme(plot.margin = margin(2,1,2,2,unit="mm"))

p_forest <- ggplot(plot_rows, aes(y = y)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1.55, ymax = 2.05,
           fill = palette_contract["pale_band"], colour = NA) +
  geom_vline(xintercept = 1, linetype = "dashed",
             linewidth = 0.35, colour = palette_contract["neutral_mid"]) +
  geom_segment(data = study_rows,
               aes(x = lo, xend = hi, yend = y),
               linewidth = 0.5, colour = palette_contract["neutral_dark"]) +
  geom_point(data = study_rows,
             aes(x = HR, size = weight),
             shape = 15, colour = palette_contract["neutral_dark"]) +
  scale_size_continuous(range = c(2.2, 4.4), guide = "none") +
  geom_segment(data = pooled_wald,
               aes(x = lo, xend = hi, yend = y),
               linewidth = 0.7, colour = palette_contract["neutral_dark"]) +
  geom_point(data = pooled_wald,
             aes(x = HR), shape = 18, size = 2.7,
             colour = palette_contract["neutral_dark"]) +
  geom_segment(data = pooled_hksj,
               aes(x = lo, xend = hi, yend = y),
               linewidth = 0.55, colour = palette_contract["neutral_mid"]) +
  geom_point(data = pooled_hksj,
             aes(x = HR), shape = 23, size = 2.5, stroke = 0.45,
             fill = "white", colour = palette_contract["neutral_mid"]) +
  scale_x_log10(
    limits = c(0.1, 3.0),
    breaks = c(0.1,0.2,0.5,1,2,3),
    labels = c("0.1","0.2","0.5","1","2","3")
  ) +
  coord_cartesian(ylim = c(0.55,4.9), clip = "off") +
  labs(x = "Hazard ratio (95% CI)", y = NULL) +
  theme_nature_contract() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = margin(2,1,2,1,unit="mm")
  )

p_right <- ggplot(plot_rows, aes(y = y)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1.55, ymax = 2.05,
           fill = palette_contract["pale_band"], colour = NA) +
  geom_text(data = study_rows, aes(x = 0, label = effect_text),
            hjust = 0, size = 6.1 / ggplot2::.pt, family = "Liberation Sans") +
  geom_text(data = pooled_rows, aes(x = 0, label = effect_text),
            hjust = 0, size = 6.1 / ggplot2::.pt, family = "Liberation Sans", fontface = "bold") +
  geom_text(aes(x = 0.98, label = weight_text),
            hjust = 1, size = 5.8 / ggplot2::.pt,
            family = "Liberation Sans", colour = palette_contract["neutral_mid"]) +
  annotate("text", x = 0, y = 4.65, label = "HR (95% CI)",
           hjust = 0, size = 6.2 / ggplot2::.pt,
           family = "Liberation Sans", fontface = "bold") +
  annotate("text", x = 0.98, y = 4.65, label = "Weight",
           hjust = 1, size = 6.2 / ggplot2::.pt,
           family = "Liberation Sans", fontface = "bold") +
  coord_cartesian(xlim = c(0,1), ylim = c(0.55,4.9), clip = "off") +
  theme_void(base_family = "Liberation Sans") +
  theme(plot.margin = margin(2,2,2,1,unit="mm"))

fig <- p_left + p_forest + p_right +
  plot_layout(widths = c(1.65, 2.30, 1.70)) +
  plot_annotation(
    caption = sprintf(
      "REML random-effects. Q=%.3f, P=%.3f; I²=%.1f%%; τ²=%.3f. HR<1 favours intensified therapy.",
      fit_wald$QE, fit_wald$QEp, fit_wald$I2, fit_wald$tau2
    ),
    theme = theme(
      plot.caption = element_text(
        family = "Liberation Sans", size = 5.5,
        hjust = 0, colour = palette_contract["neutral_mid"]
      )
    )
  )

save_pub_r <- function(plot, filename, width_mm = 183, height_mm = 70, dpi = 600) {
  w <- width_mm / 25.4
  h <- height_mm / 25.4

  svglite::svglite(paste0(filename, ".svg"), width = w, height = h)
  print(plot)
  dev.off()

  grDevices::cairo_pdf(
    paste0(filename, ".pdf"), width = w, height = h,
    family = "Liberation Sans"
  )
  print(plot)
  dev.off()

  ragg::agg_tiff(
    paste0(filename, ".tiff"),
    width = w, height = h, units = "in", res = dpi
  )
  print(plot)
  dev.off()

  ragg::agg_png(
    paste0(filename, ".png"),
    width = w, height = h, units = "in", res = 300
  )
  print(plot)
  dev.off()
}

save_pub_r(fig, file.path(OUTDIR, "Figure_M1_RCT_OS_Nature_R"))

cat("STEP 3C-1 | M1 RCT-OS | R/Nature: COMPLETE\n")
print(model_results)
print(study_results)
