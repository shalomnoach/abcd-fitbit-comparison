# LOAD PACKAGES ----------------------------------------------------------------

library(here)
source(here("config.R"))

library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)

dir.create(outputs_dir, showWarnings = FALSE)

## LOAD INPUTS ----------------------------------------------------------------

abcd_percentiles <- read_csv(
  file.path(outputs_dir, "abcd-percentiles.csv"),
  show_col_types = FALSE
)
nhanes_percentiles <- read_csv(
  file.path(outputs_dir, "nhanes-percentiles.csv"),
  show_col_types = FALSE
)
ffcws_percentiles <- read_csv(
  file.path(outputs_dir, "ffcws-percentiles.csv"),
  show_col_types = FALSE
)
pats_percentiles <- read_csv(
  file.path(outputs_dir, "pats-percentiles.csv"),
  show_col_types = FALSE
)
iglowstein <- read_csv(
  here("data", "reference", "iglowstein.csv"),
  show_col_types = FALSE
)
williams <- read_csv(
  here("data", "reference", "williams.csv"),
  show_col_types = FALSE
)
galland_studies <- read_csv(
  here("data", "reference", "galland.csv"),
  show_col_types = FALSE
) |>
  rename(age = Age, duration = `Mean sleep duration`, se = SE) |>
  mutate(weight = 1 / se)

## SMOOTH CURVES --------------------------------------------------------------

## Meta-regression (weights = 1/SE) to derive prediction SE for ribbon
galland_metareg <- lm(
  duration ~ I((age / 10)^2),
  data = galland_studies,
  weights = weight
)

galland_pred_ages <- seq(0, 19, by = 0.1)
galland_metareg_preds <- predict(
  galland_metareg,
  newdata = data.frame(age = galland_pred_ages),
  se.fit = TRUE
)

galland_predictions <- data.frame(
  age = galland_pred_ages,
  duration = galland_metareg_preds$fit,
  se_fit = galland_metareg_preds$se.fit,
  ymin = galland_metareg_preds$fit - galland_metareg_preds$se.fit,
  ymax = galland_metareg_preds$fit + galland_metareg_preds$se.fit
)

abcd_figure_percentiles <- abcd_percentiles

abcd_loess_p10 <- loess(
  p10 ~ age_mid,
  data = abcd_figure_percentiles,
  span = 0.75
)
abcd_loess_p25 <- loess(
  p25 ~ age_mid,
  data = abcd_figure_percentiles,
  span = 0.75
)
abcd_loess_p75 <- loess(
  p75 ~ age_mid,
  data = abcd_figure_percentiles,
  span = 0.75
)
abcd_loess_p90 <- loess(
  p90 ~ age_mid,
  data = abcd_figure_percentiles,
  span = 0.75
)

abcd_pred_ages <- seq(
  min(abcd_figure_percentiles$age_mid),
  max(abcd_figure_percentiles$age_mid),
  length.out = 100
)

smoothed_abcd <- data.frame(
  age = abcd_pred_ages,
  p10_smooth = predict(abcd_loess_p10, newdata = abcd_pred_ages),
  p25_smooth = predict(abcd_loess_p25, newdata = abcd_pred_ages),
  p75_smooth = predict(abcd_loess_p75, newdata = abcd_pred_ages),
  p90_smooth = predict(abcd_loess_p90, newdata = abcd_pred_ages)
)

nhanes_loess_p10 <- loess(p10 ~ age_mid, data = nhanes_percentiles, span = 0.75)
nhanes_loess_p25 <- loess(p25 ~ age_mid, data = nhanes_percentiles, span = 0.75)
nhanes_loess_p75 <- loess(p75 ~ age_mid, data = nhanes_percentiles, span = 0.75)
nhanes_loess_p90 <- loess(p90 ~ age_mid, data = nhanes_percentiles, span = 0.75)

nhanes_pred_ages <- seq(
  min(nhanes_percentiles$age_mid),
  max(nhanes_percentiles$age_mid),
  length.out = 100
)

smoothed_nhanes <- data.frame(
  age = nhanes_pred_ages,
  p10_smooth = predict(nhanes_loess_p10, newdata = nhanes_pred_ages),
  p25_smooth = predict(nhanes_loess_p25, newdata = nhanes_pred_ages),
  p75_smooth = predict(nhanes_loess_p75, newdata = nhanes_pred_ages),
  p90_smooth = predict(nhanes_loess_p90, newdata = nhanes_pred_ages)
)

iglowstein_figure_data <- iglowstein |> rename(age_years = age)
williams_figure_data <- williams

## PLOT SETTINGS --------------------------------------------------------------

abcd_color <- "#377eb8"
iglowstein_color <- "#e41a1c"
williams_color <- "#984ea3"
nhanes_color <- "#4daf4a"
galland_color <- "#e7298a"
ffcws_color <- "#a65628"
pats_color <- "#ff7f00"

base_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Open Sans", size = 14),
    plot.title = element_text(face = "bold", size = 16, family = "Open Sans"),
    axis.title = element_text(size = 14, family = "Open Sans", face = "bold"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(family = "Open Sans", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

x_limits <- c(0, 19)
y_limits <- c(5, 13)

abcd_base_layers <- list(
  geom_ribbon(
    data = smoothed_abcd,
    aes(x = age, ymin = p10_smooth, ymax = p90_smooth),
    fill = abcd_color,
    alpha = 0.15
  ),
  geom_ribbon(
    data = smoothed_abcd,
    aes(x = age, ymin = p25_smooth, ymax = p75_smooth),
    fill = abcd_color,
    alpha = 0.20
  ),
  geom_smooth(
    data = abcd_figure_percentiles,
    aes(x = age_mid, y = p10),
    method = "loess",
    se = FALSE,
    color = abcd_color,
    linewidth = 0.8,
    span = 0.75
  ),
  geom_smooth(
    data = abcd_figure_percentiles,
    aes(x = age_mid, y = p50),
    method = "loess",
    se = FALSE,
    color = abcd_color,
    linewidth = 0.8,
    span = 0.75
  ),
  geom_smooth(
    data = abcd_figure_percentiles,
    aes(x = age_mid, y = p90),
    method = "loess",
    se = FALSE,
    color = abcd_color,
    linewidth = 0.8,
    span = 0.75
  )
)

shared_scales <- list(
  scale_x_continuous(
    breaks = seq(0, 18, by = 3),
    expand = expansion(mult = c(0.02, 0.02))
  ),
  scale_y_continuous(breaks = seq(5, 13, by = 2)),
  coord_cartesian(xlim = x_limits, ylim = y_limits)
)

## BUILD PANELS ---------------------------------------------------------------

plot_iglowstein <- ggplot() +
  abcd_base_layers +
  geom_ribbon(
    data = iglowstein_figure_data,
    aes(x = age_years, ymin = p10, ymax = p90),
    fill = iglowstein_color,
    alpha = 0.15
  ) +
  geom_ribbon(
    data = iglowstein_figure_data,
    aes(x = age_years, ymin = p25, ymax = p75),
    fill = iglowstein_color,
    alpha = 0.20
  ) +
  geom_line(
    data = iglowstein_figure_data,
    aes(x = age_years, y = p10),
    color = iglowstein_color,
    linewidth = 0.8
  ) +
  geom_line(
    data = iglowstein_figure_data,
    aes(x = age_years, y = p50),
    color = iglowstein_color,
    linewidth = 0.8
  ) +
  geom_line(
    data = iglowstein_figure_data,
    aes(x = age_years, y = p90),
    color = iglowstein_color,
    linewidth = 0.8
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 6.65,
    yend = 6.65,
    color = abcd_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 6.65,
    label = "ABCD",
    hjust = 0,
    size = 5,
    color = abcd_color,
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 5.95,
    yend = 5.95,
    color = iglowstein_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 5.95,
    label = "ZLS",
    hjust = 0,
    size = 5,
    color = iglowstein_color,
    fontface = "bold"
  ) +
  labs(title = "A", x = "Age (years)", y = "Mean Sleep Duration (hours)") +
  shared_scales +
  base_theme

plot_williams <- ggplot() +
  abcd_base_layers +
  geom_ribbon(
    data = williams_figure_data,
    aes(x = age, ymin = p10, ymax = p90),
    fill = williams_color,
    alpha = 0.15
  ) +
  geom_ribbon(
    data = williams_figure_data,
    aes(x = age, ymin = p25, ymax = p75),
    fill = williams_color,
    alpha = 0.20
  ) +
  geom_line(
    data = williams_figure_data,
    aes(x = age, y = p10),
    color = williams_color,
    linewidth = 0.8
  ) +
  geom_line(
    data = williams_figure_data,
    aes(x = age, y = p50),
    color = williams_color,
    linewidth = 0.8
  ) +
  geom_line(
    data = williams_figure_data,
    aes(x = age, y = p90),
    color = williams_color,
    linewidth = 0.8
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 6.65,
    yend = 6.65,
    color = abcd_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 6.65,
    label = "ABCD",
    hjust = 0,
    size = 5,
    color = abcd_color,
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 5.95,
    yend = 5.95,
    color = williams_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 5.95,
    label = "PSID-CDS",
    hjust = 0,
    size = 5,
    color = williams_color,
    fontface = "bold"
  ) +
  labs(title = "B", x = "Age (years)", y = "Mean Sleep Duration (hours)") +
  shared_scales +
  base_theme

plot_galland <- ggplot() +
  abcd_base_layers +
  geom_point(
    data = galland_studies,
    aes(x = age, y = duration, size = weight),
    color = galland_color,
    alpha = 0.20,
    shape = 16
  ) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  geom_ribbon(
    data = galland_predictions,
    aes(x = age, ymin = ymin, ymax = ymax),
    fill = galland_color,
    alpha = 0.40
  ) +
  geom_line(
    data = galland_predictions,
    aes(x = age, y = duration),
    color = galland_color,
    linewidth = 1
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 6.65,
    yend = 6.65,
    color = abcd_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 6.65,
    label = "ABCD",
    hjust = 0,
    size = 5,
    color = abcd_color,
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 5.95,
    yend = 5.95,
    color = galland_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 5.95,
    label = "Galland et al. (2018)",
    hjust = 0,
    size = 5,
    color = galland_color,
    fontface = "bold"
  ) +
  labs(title = "C", x = "Age (years)", y = "Mean Sleep Duration (hours)") +
  shared_scales +
  base_theme

plot_nhanes <- ggplot() +
  abcd_base_layers +
  geom_ribbon(
    data = smoothed_nhanes,
    aes(x = age, ymin = p10_smooth, ymax = p90_smooth),
    fill = nhanes_color,
    alpha = 0.15
  ) +
  geom_ribbon(
    data = smoothed_nhanes,
    aes(x = age, ymin = p25_smooth, ymax = p75_smooth),
    fill = nhanes_color,
    alpha = 0.20
  ) +
  geom_smooth(
    data = nhanes_percentiles,
    aes(x = age_mid, y = p10),
    method = "loess",
    se = FALSE,
    color = nhanes_color,
    linewidth = 0.8,
    span = 0.75
  ) +
  geom_smooth(
    data = nhanes_percentiles,
    aes(x = age_mid, y = p50),
    method = "loess",
    se = FALSE,
    color = nhanes_color,
    linewidth = 0.8,
    span = 0.75
  ) +
  geom_smooth(
    data = nhanes_percentiles,
    aes(x = age_mid, y = p90),
    method = "loess",
    se = FALSE,
    color = nhanes_color,
    linewidth = 0.8,
    span = 0.75
  ) +
  annotate(
    "segment",
    x = 1,
    xend = 2,
    y = 12.65,
    yend = 12.65,
    color = abcd_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2.2,
    y = 12.65,
    label = "ABCD",
    hjust = 0,
    size = 5,
    color = abcd_color,
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = 1,
    xend = 2,
    y = 11.95,
    yend = 11.95,
    color = nhanes_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2.2,
    y = 11.95,
    label = "NHANES",
    hjust = 0,
    size = 5,
    color = nhanes_color,
    fontface = "bold"
  ) +
  labs(title = "D", x = "Age (years)", y = "Mean Sleep Duration (hours)") +
  shared_scales +
  base_theme

plot_ffcws <- ggplot() +
  abcd_base_layers +
  geom_ribbon(
    data = ffcws_percentiles,
    aes(x = age_mid, ymin = p10, ymax = p90),
    fill = ffcws_color,
    alpha = 0.15
  ) +
  geom_ribbon(
    data = ffcws_percentiles,
    aes(x = age_mid, ymin = p25, ymax = p75),
    fill = ffcws_color,
    alpha = 0.20
  ) +
  geom_line(
    data = ffcws_percentiles,
    aes(x = age_mid, y = p10),
    color = ffcws_color,
    linewidth = 0.8
  ) +
  geom_line(
    data = ffcws_percentiles,
    aes(x = age_mid, y = p50),
    color = ffcws_color,
    linewidth = 0.8
  ) +
  geom_line(
    data = ffcws_percentiles,
    aes(x = age_mid, y = p90),
    color = ffcws_color,
    linewidth = 0.8
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 6.65,
    yend = 6.65,
    color = abcd_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 6.65,
    label = "ABCD",
    hjust = 0,
    size = 5,
    color = abcd_color,
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 5.95,
    yend = 5.95,
    color = ffcws_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 5.95,
    label = "FFCWS",
    hjust = 0,
    size = 5,
    color = ffcws_color,
    fontface = "bold"
  ) +
  labs(title = "E", x = "Age (years)", y = "Mean Sleep Duration (hours)") +
  shared_scales +
  base_theme

plot_pats <- ggplot() +
  abcd_base_layers +
  geom_ribbon(
    data = pats_percentiles,
    aes(x = age_mid, ymin = p10, ymax = p90),
    fill = pats_color,
    alpha = 0.15
  ) +
  geom_ribbon(
    data = pats_percentiles,
    aes(x = age_mid, ymin = p25, ymax = p75),
    fill = pats_color,
    alpha = 0.20
  ) +
  geom_line(
    data = pats_percentiles,
    aes(x = age_mid, y = p10),
    color = pats_color,
    linewidth = 0.8
  ) +
  geom_line(
    data = pats_percentiles,
    aes(x = age_mid, y = p50),
    color = pats_color,
    linewidth = 0.8
  ) +
  geom_line(
    data = pats_percentiles,
    aes(x = age_mid, y = p90),
    color = pats_color,
    linewidth = 0.8
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 6.65,
    yend = 6.65,
    color = abcd_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 6.65,
    label = "ABCD",
    hjust = 0,
    size = 5,
    color = abcd_color,
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 3,
    y = 5.95,
    yend = 5.95,
    color = pats_color,
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 3.2,
    y = 5.95,
    label = "PATS",
    hjust = 0,
    size = 5,
    color = pats_color,
    fontface = "bold"
  ) +
  labs(title = "F", x = "Age (years)", y = "Mean Sleep Duration (hours)") +
  shared_scales +
  base_theme

## SAVE FIGURE ----------------------------------------------------------------

no_x <- theme(axis.title.x = element_blank(), axis.text.x = element_blank())
no_y <- theme(axis.title.y = element_blank(), axis.text.y = element_blank())

sleep_validation_figure <-
  ((plot_iglowstein + no_x) |
    (plot_williams + no_x + no_y) |
    (plot_galland + no_x + no_y)) /
  (plot_nhanes | (plot_ffcws + no_y) | (plot_pats + no_y)) +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5)))

ggsave(
  filename = file.path(outputs_dir, "figure1.tiff"),
  plot = sleep_validation_figure,
  width = 12,
  height = 8,
  dpi = 300,
  device = "tiff",
  compression = "lzw"
)

ggsave(
  filename = file.path(outputs_dir, "figure1.png"),
  plot = sleep_validation_figure,
  width = 12,
  height = 8,
  dpi = 300
)
