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
modeled_parameters <- bind_rows(lapply(selected, `[[`, "parameters"))

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

## DISTRIBUTION SIMILARITY -----------------------------------------------------

compute_bhattacharyya_coefficient <- function(mu1, sigma1, mu2, sigma2) {
  distance <- fpc::bhattacharyya.dist(
    mu1 = mu1,
    mu2 = mu2,
    Sigma1 = matrix(sigma1^2, nrow = 1),
    Sigma2 = matrix(sigma2^2, nrow = 1)
  )
  unname(exp(-distance))
}

distribution_similarity_by_age <- modeled_parameters |>
  filter(dataset == "ABCD") |>
  select(
    age_center,
    abcd_mu = mu,
    abcd_sigma = sigma
  ) |>
  inner_join(
    modeled_parameters |>
      filter(dataset != "ABCD") |>
      rename(
        reference = dataset,
        reference_mu = mu,
        reference_sigma = sigma
      ),
    by = "age_center"
  ) |>
  mutate(
    bhattacharyya_coefficient = mapply(
      compute_bhattacharyya_coefficient,
      abcd_mu,
      abcd_sigma,
      reference_mu,
      reference_sigma
    )
  )

distribution_similarity_table <- distribution_similarity_by_age |>
  group_by(reference) |>
  summarise(
    bhattacharyya_mean = mean(bhattacharyya_coefficient),
    bhattacharyya_min = min(bhattacharyya_coefficient),
    bhattacharyya_max = max(bhattacharyya_coefficient),
    .groups = "drop"
  ) |>
  transmute(
    reference,
    bhattacharyya_coefficient = sprintf(
      "%.2f (%.2f to %.2f)",
      bhattacharyya_mean,
      bhattacharyya_min,
      bhattacharyya_max
    )
  ) |>
  arrange(match(reference, setdiff(analysis_datasets, "ABCD")))

## GALLAND PUBLISHED CURVE -----------------------------------------------------

galland_sleep_duration <- function(age) 9.02 - 1.04 * (((age / 10)^2) - 0.83)

galland_model <- metafor::rma(
  yi = duration,
  sei = se,
  mods = ~ I(age^2),
  data = galland_studies,
  method = "REML",
  test = "knha"
)

galland_means <- tibble(
  age_center = 1:16,
  mean_sleep = galland_sleep_duration(1:16)
)
galland_curve <- tibble(
  age = seq(min(galland_studies$age), 16, by = 0.1)
)
galland_curve_prediction <- stats::predict(
  galland_model,
  newmods = galland_curve$age^2,
  level = 95
)
galland_curve <- galland_curve |>
  mutate(
    mean_sleep = as.numeric(galland_curve_prediction$pred),
    prediction_limit_lower = as.numeric(galland_curve_prediction$pi.lb),
    prediction_limit_upper = as.numeric(galland_curve_prediction$pi.ub)
  )

## COMPARISON TABLE (Table 1) --------------------------------------------------

abcd_modeled <- filter(modeled_centiles, dataset == "ABCD")

galland_prediction_table <- abcd_modeled |>
  filter(age_center <= 16) |>
  transmute(
    age = age_center,
    abcd_modeled_mean = mean_sleep
  )
galland_age_prediction <- stats::predict(
  galland_model,
  newmods = galland_prediction_table$age^2,
  level = 95
)
galland_prediction_table <- galland_prediction_table |>
  mutate(
    galland_predicted_mean = as.numeric(galland_age_prediction$pred),
    prediction_limit_lower = as.numeric(galland_age_prediction$pi.lb),
    prediction_limit_upper = as.numeric(galland_age_prediction$pi.ub)
  )

print(galland_prediction_table, n = Inf, width = Inf)

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
) |>
  left_join(
    distribution_similarity_table |>
      select(reference, bhattacharyya_coefficient),
    by = "reference"
  )

selected_filliben <- tibble(
  reference = analysis_datasets,
  reference_filliben_r = vapply(
    selected,
    `[[`,
    numeric(1),
    "filliben_r"
  )
)
abcd_filliben_r <- selected_filliben$reference_filliben_r[
  selected_filliben$reference == "ABCD"
]

comparison_table <- comparison_table |>
  left_join(
    filter(selected_filliben, reference != "ABCD"),
    by = "reference"
  ) |>
  mutate(
    bhattacharyya_coefficient = coalesce(
      bhattacharyya_coefficient,
      "—"
    ),
    gamlss_filliben_r_abcd_reference = if_else(
      is.na(reference_filliben_r),
      sprintf("%.3f/—", abcd_filliben_r),
      sprintf(
        "%.3f/%.3f",
        abcd_filliben_r,
        reference_filliben_r
      )
    )
  ) |>
  select(-reference_filliben_r)

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
  geom_ribbon(
    data = galland_curve,
    aes(
      x = age,
      ymin = prediction_limit_lower,
      ymax = prediction_limit_upper
    ),
    fill = galland_color,
    alpha = 0.15
  ) +
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
