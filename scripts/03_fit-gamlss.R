# FIT GAMLSS MODELS ------------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))
source(here("scripts", "gamlss-helpers.R"))

library(dplyr)
library(ggplot2)
library(gamlss2)
library(mgcv)
library(patchwork)
library(ragg)
library(readr)

dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)

## INPUTS ----------------------------------------------------------------------

cohorts <- read_csv(
  file.path(data_sets_dir, "cohorts.csv"),
  show_col_types = FALSE
)

# NHANES is fit with adult support (ages 3-80) for a stable smooth
model_cohorts <- bind_rows(
  filter(cohorts, dataset != "NHANES"),
  load_nhanes_model_support()
)

galland_studies <- read_csv(
  here("data", "reference", "galland.csv"),
  show_col_types = FALSE
) |>
  rename(
    study = `Author, year`,
    age = Age,
    duration = `Mean sleep duration`,
    se = SE
  )

## FIT AND SELECT --------------------------------------------------------------

candidates <- unlist(
  lapply(analysis_datasets, function(d) {
    fit_dataset_candidates(model_cohorts, d, empirical_centiles)
  }),
  recursive = FALSE
)
candidate_diagnostics <- bind_rows(lapply(candidates, `[[`, "diagnostics"))
selected_model <- select_global_model(candidate_diagnostics)

selected <- lapply(analysis_datasets, function(d) {
  idx <- which(vapply(
    candidates,
    function(candidate) {
      isTRUE(candidate$diagnostics$model_status == "ok") &&
        candidate$diagnostics$dataset == d &&
        candidate$diagnostics$model_label == selected_model$model_label &&
        candidate$diagnostics$family == selected_model$family
    },
    logical(1)
  ))
  materialise_candidate(candidates[[idx]])
})

modeled_centiles <- bind_rows(lapply(selected, `[[`, "centiles"))
modeled_curves <- bind_rows(lapply(selected, `[[`, "curves"))

## MODEL-SELECTION DIAGNOSTICS -------------------------------------------------

candidate_diagnostics <- candidate_diagnostics |>
  mutate(
    selected = model_status == "ok" &
      model_label == selected_model$model_label &
      family == selected_model$family
  )

candidate_diagnostics |>
  arrange(model_label, family, dataset) |>
  print(n = Inf)

print(selected_model$summary)

compute_ordering_checks(modeled_centiles) |>
  group_by(dataset) |>
  summarise(ages = n(), ordered = sum(is_ordered), .groups = "drop") |>
  print()

print(model_vs_empirical(modeled_centiles, empirical_centiles))

## GALLAND PUBLISHED CURVE -----------------------------------------------------

galland_sleep_duration <- function(age) 9.02 - 1.04 * (((age / 10)^2) - 0.83)

galland_means <- tibble(
  age_center = 1:16,
  mean_sleep = galland_sleep_duration(1:16)
)
galland_curve <- tibble(
  age = seq(min(galland_studies$age), max(galland_studies$age), by = 0.1)
) |>
  mutate(mean_sleep = galland_sleep_duration(age))

## COMPARISON TABLE (Table 1) --------------------------------------------------

abcd_modeled <- filter(modeled_centiles, dataset == "ABCD")

# Sum of reference participants over the ages shared with ABCD.
overlap_n <- function(reference) {
  ages <- intersect(abcd_modeled$age_center, reference$age_center)
  fmt_int(sum(reference$n[reference$age_center %in% ages]))
}

comparison_row <- function(
  reference_name,
  reference,
  value_col,
  reference_overlap_n
) {
  gaps <- summarise_gaps(abcd_modeled, reference, value_col)
  tibble(
    reference = reference_name,
    age_range = if (gaps$min_age == gaps$max_age) {
      as.character(gaps$min_age)
    } else {
      paste0(gaps$min_age, "–", gaps$max_age)
    },
    reference_overlap_n = reference_overlap_n,
    signed_difference_h = sprintf(
      "%.2f (%.2f to %.2f)",
      gaps$signed_mean,
      gaps$signed_min,
      gaps$signed_max
    ),
    absolute_difference_h = sprintf(
      "%.2f (%.2f to %.2f)",
      gaps$absolute_mean,
      gaps$absolute_min,
      gaps$absolute_max
    )
  )
}

comparison_table <- bind_rows(
  comparison_row(
    "NHANES",
    filter(modeled_centiles, dataset == "NHANES"),
    "p50",
    overlap_n(filter(modeled_centiles, dataset == "NHANES"))
  ),
  comparison_row(
    "FFCWS",
    filter(modeled_centiles, dataset == "FFCWS"),
    "p50",
    overlap_n(filter(modeled_centiles, dataset == "FFCWS"))
  ),
  comparison_row(
    "PATS",
    filter(modeled_centiles, dataset == "PATS"),
    "p50",
    overlap_n(filter(modeled_centiles, dataset == "PATS"))
  ),
  comparison_row("Galland", galland_means, "mean_sleep", "-")
)

print(comparison_table)

## SAVE AGGREGATE OUTPUTS -------------------------------------------------------

write_csv(modeled_centiles, file.path(outputs_dir, "modeled-centiles.csv"))
write_csv(comparison_table, file.path(outputs_dir, "comparison-table.csv"))

## FIGURE -----------------------------------------------------------------------

figure_font_family <- "Open Sans"
if (!figure_font_family %in% systemfonts::system_fonts()$family) {
  systemfonts::register_font(
    name = figure_font_family,
    plain = path.expand("~/Library/Fonts/OpenSans-Regular.ttf"),
    bold = path.expand("~/Library/Fonts/OpenSans-Bold.ttf"),
    italic = path.expand("~/Library/Fonts/OpenSans-Italic.ttf"),
    bolditalic = path.expand("~/Library/Fonts/OpenSans-BoldItalic.ttf")
  )
}

abcd_color <- "#377eb8"
nhanes_color <- "#7B3294"
ffcws_color <- "#009E9A"
pats_color <- "#F28E2B"
galland_color <- "#D81B60"

base_theme <- theme_minimal() +
  theme(
    text = element_text(family = figure_font_family, size = 14),
    plot.title = element_text(
      face = "bold",
      size = 16,
      family = figure_font_family
    ),
    axis.title = element_text(
      size = 14,
      family = figure_font_family,
      face = "bold"
    ),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(family = figure_font_family, size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

shared_scales <- list(
  scale_x_continuous(
    breaks = seq(0, 18, by = 3),
    expand = expansion(mult = c(0.02, 0.02))
  ),
  scale_y_continuous(breaks = seq(6, 12, by = 2)),
  coord_cartesian(xlim = c(0, 19), ylim = c(6, 12))
)

distribution_layers <- function(df, line_color) {
  list(
    geom_ribbon(
      data = df,
      aes(x = age, ymin = p10, ymax = p90),
      fill = line_color,
      alpha = 0.15
    ),
    geom_ribbon(
      data = df,
      aes(x = age, ymin = p25, ymax = p75),
      fill = line_color,
      alpha = 0.20
    ),
    geom_line(
      data = df,
      aes(x = age, y = p10),
      color = line_color,
      linewidth = 0.8
    ),
    geom_line(
      data = df,
      aes(x = age, y = p50),
      color = line_color,
      linewidth = 0.9
    ),
    geom_line(
      data = df,
      aes(x = age, y = p90),
      color = line_color,
      linewidth = 0.8
    )
  )
}

label_layers <- function(reference_label, reference_color) {
  list(
    annotate(
      "segment",
      x = 1,
      xend = 2,
      y = 7.15,
      yend = 7.15,
      color = abcd_color,
      linewidth = 1
    ),
    annotate(
      "text",
      x = 2.2,
      y = 7.15,
      label = "ABCD",
      hjust = 0,
      size = 5,
      color = abcd_color,
      family = figure_font_family,
      fontface = "bold"
    ),
    annotate(
      "segment",
      x = 1,
      xend = 2,
      y = 6.45,
      yend = 6.45,
      color = reference_color,
      linewidth = 1
    ),
    annotate(
      "text",
      x = 2.2,
      y = 6.45,
      label = reference_label,
      hjust = 0,
      size = 5,
      color = reference_color,
      family = figure_font_family,
      fontface = "bold"
    )
  )
}

abcd_curve <- filter(modeled_curves, dataset == "ABCD")

centile_panel <- function(
  reference_dataset,
  title,
  reference_label,
  reference_color
) {
  ggplot() +
    distribution_layers(abcd_curve, abcd_color) +
    distribution_layers(
      filter(modeled_curves, dataset == reference_dataset),
      reference_color
    ) +
    label_layers(reference_label, reference_color) +
    labs(title = title, x = "Age (years)", y = "Sleep Duration (hours)") +
    shared_scales +
    base_theme
}

plot_nhanes <- centile_panel("NHANES", "A", "NHANES", nhanes_color)
plot_ffcws <- centile_panel("FFCWS", "B", "FFCWS", ffcws_color)
plot_pats <- centile_panel("PATS", "C", "PATS", pats_color)

plot_galland <- ggplot() +
  distribution_layers(abcd_curve, abcd_color) +
  geom_point(
    data = galland_studies,
    aes(x = age, y = duration, size = 1 / se),
    color = galland_color,
    alpha = 0.20,
    shape = 16
  ) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  geom_line(
    data = galland_curve,
    aes(x = age, y = mean_sleep),
    color = galland_color,
    linewidth = 1
  ) +
  label_layers("Galland BC et al. 2018", galland_color) +
  labs(title = "D", x = "Age (years)", y = "Sleep Duration (hours)") +
  shared_scales +
  base_theme

figure1 <- (plot_nhanes | plot_ffcws) /
  (plot_pats | plot_galland) +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5)))

print(figure1)

ggsave(
  filename = file.path(outputs_dir, "figure1.png"),
  plot = figure1,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300,
  device = ragg::agg_png
)
ggsave(
  filename = file.path(outputs_dir, "figure1.tiff"),
  plot = figure1,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300,
  device = ragg::agg_tiff,
  compression = "lzw"
)
