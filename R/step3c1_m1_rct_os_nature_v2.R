#!/usr/bin/env Rscript

# Step 3C-1 | M1 RCT-OS | Nature-R V2 visual refinement
# IMPORTANT: statistics/data are NOT changed. This script first runs the locked
# analysis script, then redraws the same results with a restrained blue-grey palette.

source("R/step3c1_m1_rct_os_nature.R", encoding = "UTF-8")

palette_v2 <- c(
  ink = "#20252B",
  study = "#466B82",
  pooled = "#183B56",
  hksj = "#7A8792",
  muted = "#66727C",
  reference = "#A7AFB6",
  pooled_band = "#F2F6F9",
  white = "#FFFFFF"
)

theme_nature_v2 <- function(base_size = 7.1, base_family = "Liberation Sans") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line = element_line(linewidth = 0.38, colour = palette_v2["ink"]),
      axis.ticks = element_line(linewidth = 0.35, colour = palette_v2["ink"]),
      axis.title = element_text(size = 7.1, colour = palette_v2["ink"]),
      axis.text = element_text(size = 6.4, colour = palette_v2["ink"]),
      plot.caption = element_text(size = 5.8, hjust = 0, colour = palette_v2["muted"]),
      panel.grid = element_blank(),
      plot.margin = margin(2, 2, 2, 2, unit = "mm")
    )
}

# Group both pooled rows with one very light band so Wald and HKSJ remain parallel.
p_left_v2 <- ggplot(plot_rows, aes(y = y)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.62, ymax = 2.08,
           fill = palette_v2["pooled_band"], colour = NA) +
  geom_text(data = study_rows, aes(x = 0, label = label),
            hjust = 0, size = 7.0 / ggplot2::.pt,
            family = "Liberation Sans", colour = palette_v2["ink"]) +
  geom_text(data = pooled_rows, aes(x = 0, label = label),
            hjust = 0, size = 7.0 / ggplot2::.pt,
            family = "Liberation Sans", fontface = "bold",
            colour = palette_v2["ink"]) +
  geom_text(aes(x = 0.98, label = N_label),
            hjust = 1, size = 6.0 / ggplot2::.pt,
            family = "Liberation Sans", colour = palette_v2["muted"]) +
  annotate("text", x = 0, y = 4.68, label = "Study",
           hjust = 0, size = 6.5 / ggplot2::.pt,
           family = "Liberation Sans", fontface = "bold",
           colour = palette_v2["ink"]) +
  annotate("text", x = 0.98, y = 4.68, label = "Sample size",
           hjust = 1, size = 6.5 / ggplot2::.pt,
           family = "Liberation Sans", fontface = "bold",
           colour = palette_v2["ink"]) +
  coord_cartesian(xlim = c(0,1), ylim = c(0.52,4.92), clip = "off") +
  theme_void(base_family = "Liberation Sans") +
  theme(plot.margin = margin(2,1,2,2,unit="mm"))

p_forest_v2 <- ggplot(plot_rows, aes(y = y)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.62, ymax = 2.08,
           fill = palette_v2["pooled_band"], colour = NA) +
  geom_vline(xintercept = 1, linetype = "dashed",
             linewidth = 0.38, colour = palette_v2["reference"]) +
  geom_segment(data = study_rows,
               aes(x = lo, xend = hi, yend = y),
               linewidth = 0.62, colour = palette_v2["study"],
               lineend = "round") +
  geom_point(data = study_rows,
             aes(x = HR, size = weight),
             shape = 15, colour = palette_v2["study"]) +
  scale_size_continuous(range = c(2.4, 4.6), guide = "none") +
  geom_segment(data = pooled_wald,
               aes(x = lo, xend = hi, yend = y),
               linewidth = 0.82, colour = palette_v2["pooled"],
               lineend = "round") +
  geom_point(data = pooled_wald,
             aes(x = HR), shape = 18, size = 3.0,
             colour = palette_v2["pooled"]) +
  geom_segment(data = pooled_hksj,
               aes(x = lo, xend = hi, yend = y),
               linewidth = 0.65, colour = palette_v2["hksj"],
               lineend = "round") +
  geom_point(data = pooled_hksj,
             aes(x = HR), shape = 23, size = 2.8, stroke = 0.55,
             fill = palette_v2["white"], colour = palette_v2["hksj"]) +
  scale_x_log10(
    limits = c(0.1, 3.0),
    breaks = c(0.1,0.2,0.5,1,2,3),
    labels = c("0.1","0.2","0.5","1","2","3")
  ) +
  coord_cartesian(ylim = c(0.52,4.92), clip = "off") +
  labs(x = "Hazard ratio (95% CI)", y = NULL) +
  theme_nature_v2() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = margin(2,1,2,1,unit="mm")
  )

p_right_v2 <- ggplot(plot_rows, aes(y = y)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.62, ymax = 2.08,
           fill = palette_v2["pooled_band"], colour = NA) +
  geom_text(data = study_rows, aes(x = 0, label = effect_text),
            hjust = 0, size = 6.4 / ggplot2::.pt,
            family = "Liberation Sans", colour = palette_v2["ink"]) +
  geom_text(data = pooled_wald, aes(x = 0, label = effect_text),
            hjust = 0, size = 6.4 / ggplot2::.pt,
            family = "Liberation Sans", fontface = "bold",
            colour = palette_v2["pooled"]) +
  geom_text(data = pooled_hksj, aes(x = 0, label = effect_text),
            hjust = 0, size = 6.4 / ggplot2::.pt,
            family = "Liberation Sans", fontface = "bold",
            colour = palette_v2["hksj"]) +
  geom_text(aes(x = 0.98, label = weight_text),
            hjust = 1, size = 6.0 / ggplot2::.pt,
            family = "Liberation Sans", colour = palette_v2["muted"]) +
  annotate("text", x = 0, y = 4.68, label = "HR (95% CI)",
           hjust = 0, size = 6.5 / ggplot2::.pt,
           family = "Liberation Sans", fontface = "bold",
           colour = palette_v2["ink"]) +
  annotate("text", x = 0.98, y = 4.68, label = "Weight",
           hjust = 1, size = 6.5 / ggplot2::.pt,
           family = "Liberation Sans", fontface = "bold",
           colour = palette_v2["ink"]) +
  coord_cartesian(xlim = c(0,1), ylim = c(0.52,4.92), clip = "off") +
  theme_void(base_family = "Liberation Sans") +
  theme(plot.margin = margin(2,2,2,1,unit="mm"))

fig_v2 <- p_left_v2 + p_forest_v2 + p_right_v2 +
  plot_layout(widths = c(1.72, 2.38, 1.74)) +
  plot_annotation(
    caption = sprintf(
      "REML random-effects: Q=%.3f, P=%.3f; I²=%.1f%%; τ²=%.3f. HR < 1 favours intensified therapy.",
      fit_wald$QE, fit_wald$QEp, fit_wald$I2, fit_wald$tau2
    ),
    theme = theme(
      plot.caption = element_text(
        family = "Liberation Sans", size = 5.8,
        hjust = 0, colour = palette_v2["muted"]
      )
    )
  )

save_pub_r(
  fig_v2,
  file.path(OUTDIR, "Figure_M1_RCT_OS_Nature_R_V2"),
  width_mm = 183,
  height_mm = 72,
  dpi = 600
)

cat("STEP 3C-1 | M1 RCT-OS | Nature-R V2: COMPLETE\n")
cat("Statistics unchanged; only visual styling was modified.\n")
print(model_results)
