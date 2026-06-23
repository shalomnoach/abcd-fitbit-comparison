# SHARED HELPERS ---------------------------------------------------------------
# Formatting, empirical centile, and comparison helpers used across the
# preparation, centile, and modeling scripts.

empirical_percentiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
empirical_percentile_names <- c("p10", "p25", "p50", "p75", "p90")
empirical_point_columns <- c("mean_sleep", empirical_percentile_names)

# Format an integer with comma separators.
fmt_int <- function(x) {
  prettyNum(as.integer(round(x)), big.mark = ",")
}

# Format a number with two decimal places.
fmt2 <- function(x) {
  formatC(x, format = "f", digits = 2)
}

## EMPIRICAL CENTILES ----------------------------------------------------------
# Age-binned centiles per dataset, used as model-selection targets. Ages are
# binned to integer centers (floor(age + 0.5))

compute_empirical_summary <- function(
  df,
  value_col = "sleep_hours",
  min_n = 5L
) {
  df |>
    dplyr::filter(!is.na(.data$age_center), !is.na(.data[[value_col]])) |>
    dplyr::group_by(.data$dataset, .data$age_center) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_sleep = mean(.data[[value_col]]),
      p10 = stats::quantile(.data[[value_col]], 0.10),
      p25 = stats::quantile(.data[[value_col]], 0.25),
      p50 = stats::quantile(.data[[value_col]], 0.50),
      p75 = stats::quantile(.data[[value_col]], 0.75),
      p90 = stats::quantile(.data[[value_col]], 0.90),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n >= min_n) |>
    dplyr::arrange(.data$age_center)
}

# Survey-weighted version for NHANES (uses the complex survey design).
compute_survey_summary <- function(df, value_col = "sleep_hours", min_n = 5L) {
  old_lonely_psu <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely_psu), add = TRUE)

  design <- survey::svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~weight,
    nest = TRUE,
    data = df
  )
  value_formula <- stats::as.formula(paste0("~", value_col))

  rows <- lapply(sort(unique(df$age_center)), function(center) {
    age_design <- subset(design, age_center == center)
    n_obs <- nrow(age_design$variables)
    if (n_obs < min_n) {
      return(NULL)
    }

    quantiles <- as.numeric(stats::coef(survey::svyquantile(
      value_formula,
      age_design,
      quantiles = empirical_percentiles,
      ci = FALSE,
      na.rm = TRUE
    )))
    mean_sleep <- as.numeric(stats::coef(survey::svymean(
      value_formula,
      age_design,
      na.rm = TRUE
    )))

    tibble::tibble(
      dataset = unique(df$dataset),
      age_center = center,
      n = n_obs,
      mean_sleep = mean_sleep,
      p10 = quantiles[1],
      p25 = quantiles[2],
      p50 = quantiles[3],
      p75 = quantiles[4],
      p90 = quantiles[5]
    )
  })

  dplyr::bind_rows(rows) |>
    dplyr::arrange(.data$age_center)
}

compute_ordering_checks <- function(centiles) {
  centiles |>
    dplyr::transmute(
      dataset = .data$dataset,
      age_center = .data$age_center,
      is_ordered = .data$p10 <= .data$p25 &
        .data$p25 <= .data$p50 &
        .data$p50 <= .data$p75 &
        .data$p75 <= .data$p90
    )
}

## REFERENCE COMPARISON --------------------------------------------------------

summarise_gaps <- function(abcd, reference, value_col) {
  ages <- sort(intersect(abcd$age_center, reference$age_center))
  gap <- abcd[[value_col]][match(ages, abcd$age_center)] -
    reference[[value_col]][match(ages, reference$age_center)]

  tibble::tibble(
    min_age = min(ages),
    max_age = max(ages),
    signed_mean = mean(gap),
    signed_min = min(gap),
    signed_max = max(gap),
    absolute_mean = mean(abs(gap)),
    absolute_min = min(abs(gap)),
    absolute_max = max(abs(gap))
  )
}
