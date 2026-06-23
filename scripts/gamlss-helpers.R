# GAMLSS FAMILIES AND FITTING HELPERS ------------------------------------------

library(dplyr)
library(gamlss2)
library(mgcv)
library(readr)

## FAMILIES ---------------------------------------------------------------------

NormalHours <- function() {
  fam <- list(
    family = "NormalHours",
    names = c("mu", "sigma"),
    links = c(mu = "identity", sigma = "log"),
    score = list(
      mu = function(y, par, ...) (y - par$mu) / (par$sigma^2),
      sigma = function(y, par, ...) -1 + (y - par$mu)^2 / (par$sigma^2)
    ),
    hess = list(
      mu = function(y, par, ...) 1 / (par$sigma^2),
      sigma = function(y, par, ...) rep(2, length(y)),
      "mu.sigma" = function(y, par, ...) rep(0, length(y))
    ),
    loglik = function(y, par, ...) {
      sum(dnorm(y, mean = par$mu, sd = par$sigma, log = TRUE))
    },
    pdf = function(y, par, log = FALSE, ...) {
      dnorm(y, mean = par$mu, sd = par$sigma, log = log)
    },
    cdf = function(y, par, ...) pnorm(y, mean = par$mu, sd = par$sigma, ...),
    quantile = function(p, par, ...) {
      qnorm(p, mean = par$mu, sd = par$sigma, ...)
    },
    initialize = list(
      mu = function(y, ...) (y + mean(y, na.rm = TRUE)) / 2,
      sigma = function(y, ...) rep(sd(y, na.rm = TRUE), length(y))
    ),
    mean = function(par, ...) par$mu,
    variance = function(par, ...) par$sigma^2,
    valid.response = function(x) {
      if (!is.numeric(x)) {
        stop("The response should be numeric.")
      }
      TRUE
    }
  )
  class(fam) <- "gamlss2.family"
  fam
}

LogNormal <- function() {
  fam <- list(
    family = "LogNormal",
    names = c("mu", "sigma"),
    links = c(mu = "identity", sigma = "log"),
    score = list(
      mu = function(y, par, ...) (log(y) - par$mu) / (par$sigma^2),
      sigma = function(y, par, ...) -1 + (log(y) - par$mu)^2 / (par$sigma^2)
    ),
    hess = list(
      mu = function(y, par, ...) 1 / (par$sigma^2),
      sigma = function(y, par, ...) rep(2, length(y)),
      "mu.sigma" = function(y, par, ...) rep(0, length(y))
    ),
    loglik = function(y, par, ...) {
      sum(dlnorm(y, meanlog = par$mu, sdlog = par$sigma, log = TRUE))
    },
    pdf = function(y, par, log = FALSE, ...) {
      dlnorm(y, meanlog = par$mu, sdlog = par$sigma, log = log)
    },
    cdf = function(y, par, ...) {
      plnorm(y, meanlog = par$mu, sdlog = par$sigma, ...)
    },
    quantile = function(p, par, ...) {
      qlnorm(p, meanlog = par$mu, sdlog = par$sigma, ...)
    },
    initialize = list(
      mu = function(y, ...) (log(y) + mean(log(y), na.rm = TRUE)) / 2,
      sigma = function(y, ...) rep(sd(log(y), na.rm = TRUE), length(y))
    ),
    mean = function(par, ...) exp(par$mu + 0.5 * par$sigma^2),
    variance = function(par, ...) {
      (exp(par$sigma^2) - 1) * exp(2 * par$mu + par$sigma^2)
    },
    valid.response = function(x) {
      if (!is.numeric(x)) {
        stop("The response should be numeric.")
      }
      if (any(x <= 0, na.rm = TRUE)) {
        stop("The log-normal response requires positive durations.")
      }
      TRUE
    }
  )
  class(fam) <- "gamlss2.family"
  fam
}

model_families <- list(NormalHours = NormalHours, LogNormal = LogNormal)

## CANDIDATE MODELS -------------------------------------------------------------

analysis_datasets <- c("ABCD", "NHANES", "FFCWS", "PATS")

model_blueprints <- tibble::tribble(
  ~model_label               , ~mu_term                   , ~sigma_term                ,
  "linear_mu_constant_sigma" , "age"                      , "1"                        ,
  "smooth_mu_constant_sigma" , "s(age, k = 6, bs = 'cs')" , "1"                        ,
  "smooth_mu_smooth_sigma"   , "s(age, k = 6, bs = 'cs')" , "s(age, k = 4, bs = 'cs')"
)

model_specs <- bind_rows(lapply(analysis_datasets, function(d) {
  model_blueprints |>
    mutate(
      dataset = d,
      fit_min_age = if (d == "NHANES") 3 else NA_real_,
      fit_max_age = if (d == "NHANES") 80 else NA_real_,
      report_min_age = if (d == "NHANES") 3 else NA_real_,
      report_max_age = if (d == "NHANES") 19 else NA_real_
    )
}))

## FITTING ----------------------------------------------------------------------

standardize_model_data <- function(df, dataset) {
  model_df <- df |>
    filter(
      .data$dataset == .env$dataset,
      is.finite(.data$age),
      is.finite(.data$sleep_hours),
      .data$sleep_hours > 0
    ) |>
    mutate(
      model_weight = if_else(
        is.finite(.data$weight) & .data$weight > 0,
        .data$weight,
        NA_real_
      )
    )

  if (all(is.na(model_df$model_weight))) {
    model_df$model_weight <- 1
  }

  model_df |>
    mutate(
      model_weight = coalesce(.data$model_weight, 1),
      model_weight = .data$model_weight / mean(.data$model_weight)
    )
}

apply_age_bounds <- function(df, min_age, max_age) {
  if (is.finite(min_age)) {
    df <- filter(df, .data$age >= min_age)
  }
  if (is.finite(max_age)) {
    df <- filter(df, .data$age <= max_age)
  }
  df
}

predict_centiles <- function(model, newdata) {
  quantiles <- as.data.frame(stats::quantile(
    model,
    newdata = newdata,
    probs = empirical_percentiles
  ))
  names(quantiles) <- empirical_percentile_names
  tibble::tibble(
    mean_sleep = as.numeric(predict(
      model,
      newdata = newdata,
      type = "response"
    )),
    p10 = quantiles$p10,
    p25 = quantiles$p25,
    p50 = quantiles$p50,
    p75 = quantiles$p75,
    p90 = quantiles$p90
  )
}

score_against_empirical <- function(center_table, empirical_targets) {
  joined <- center_table |>
    select("dataset", "age_center", all_of(empirical_point_columns)) |>
    inner_join(
      empirical_targets,
      by = c("dataset", "age_center"),
      suffix = c("_modeled", "_empirical")
    )
  if (nrow(joined) == 0) {
    return(NA_real_)
  }

  diffs <- unlist(lapply(empirical_point_columns, function(p) {
    joined[[paste0(p, "_modeled")]] - joined[[paste0(p, "_empirical")]]
  }))
  mean(abs(diffs), na.rm = TRUE)
}

fit_dataset_candidates <- function(df, dataset, empirical_targets) {
  specs <- filter(model_specs, .data$dataset == .env$dataset)
  model_df_full <- standardize_model_data(df, dataset)

  candidate_results <- list()
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]
    model_df <- apply_age_bounds(
      model_df_full,
      spec$fit_min_age,
      spec$fit_max_age
    )
    report_df <- apply_age_bounds(
      model_df,
      spec$report_min_age,
      spec$report_max_age
    )
    report_min <- if (is.finite(spec$report_min_age)) {
      spec$report_min_age
    } else {
      min(report_df$age)
    }
    report_max <- if (is.finite(spec$report_max_age)) {
      spec$report_max_age
    } else {
      max(report_df$age)
    }

    centers <- report_df |>
      count(.data$dataset, .data$age_center) |>
      filter(.data$n >= 5) |>
      arrange(.data$age_center)

    formula <- stats::as.formula(paste0(
      "sleep_hours ~ ",
      spec$mu_term,
      " | ",
      spec$sigma_term
    ))

    for (family_name in names(model_families)) {
      fit <- try(
        gamlss2(
          formula,
          data = model_df,
          family = model_families[[family_name]],
          weights = model_weight,
          trace = FALSE
        ),
        silent = TRUE
      )

      if (inherits(fit, "try-error")) {
        candidate_results[[length(candidate_results) + 1L]] <- list(
          fit = NULL,
          diagnostics = tibble(
            dataset = dataset,
            model_label = spec$model_label,
            family = family_name,
            model_status = "error",
            gaic = NA_real_,
            empirical_mad = NA_real_
          )
        )
        next
      }

      center_table <- bind_cols(
        centers,
        predict_centiles(
          fit,
          tibble(age = centers$age_center, model_weight = 1)
        )
      )

      candidate_results[[length(candidate_results) + 1L]] <- list(
        fit = fit,
        dataset = dataset,
        report_min = report_min,
        report_max = report_max,
        center_table = center_table,
        diagnostics = tibble(
          dataset = dataset,
          model_label = spec$model_label,
          family = family_name,
          model_status = "ok",
          gaic = as.numeric(GAIC(fit)),
          empirical_mad = score_against_empirical(
            center_table,
            empirical_targets
          )
        )
      )
    }
  }

  candidate_results
}

select_global_model <- function(diagnostics) {
  ranked <- diagnostics |>
    filter(.data$model_status == "ok", is.finite(.data$gaic)) |>
    group_by(.data$model_label, .data$family) |>
    summarise(
      n_datasets = n_distinct(.data$dataset),
      mean_gaic = mean(.data$gaic),
      mean_empirical_mad = mean(.data$empirical_mad, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(.data$n_datasets == length(analysis_datasets)) |>
    arrange(.data$mean_gaic, .data$mean_empirical_mad)

  if (nrow(ranked) == 0) {
    stop("No common GAMLSS model fit all datasets.")
  }

  list(
    model_label = ranked$model_label[[1]],
    family = ranked$family[[1]],
    summary = ranked
  )
}

materialise_candidate <- function(candidate) {
  curve_ages <- seq(candidate$report_min, candidate$report_max, by = 0.1)
  curves <- bind_cols(
    tibble(dataset = candidate$dataset, age = curve_ages),
    predict_centiles(candidate$fit, tibble(age = curve_ages, model_weight = 1))
  )
  list(
    centiles = candidate$center_table |>
      select("dataset", "age_center", "n", all_of(empirical_point_columns)),
    curves = curves
  )
}

model_vs_empirical <- function(modeled, empirical) {
  joined <- modeled |>
    select("dataset", "age_center", all_of(empirical_point_columns)) |>
    inner_join(
      empirical |>
        select("dataset", "age_center", all_of(empirical_point_columns)),
      by = c("dataset", "age_center"),
      suffix = c("_modeled", "_empirical")
    )

  bind_rows(lapply(empirical_point_columns, function(p) {
    tibble(
      dataset = joined$dataset,
      difference = joined[[paste0(p, "_modeled")]] -
        joined[[paste0(p, "_empirical")]]
    )
  })) |>
    group_by(.data$dataset) |>
    summarise(
      mean_abs_diff = mean(abs(.data$difference), na.rm = TRUE),
      max_abs_diff = max(abs(.data$difference), na.rm = TRUE),
      .groups = "drop"
    )
}

## NHANES ADULT FIT SUPPORT -----------------------------------------------------

load_nhanes_model_support <- function() {
  nhanes_sleep <- read_csv(nhanes_sleep_path, show_col_types = FALSE)
  nhanes_demo <- bind_rows(lapply(nhanes_demo_paths, haven::read_xpt))

  nhanes_sleep |>
    left_join(nhanes_demo |> select(SEQN, RIDAGEYR), by = "SEQN") |>
    filter(
      !is.na(dur_spt_min),
      dur_spt_min > 0,
      !is.na(RIDAGEYR),
      RIDAGEYR >= 3,
      RIDAGEYR <= 80,
      !is.na(mec4yr),
      mec4yr > 0
    ) |>
    mutate(age = RIDAGEYR, nightly_sleep_hours = dur_spt_min / 60) |>
    filter(nightly_sleep_hours >= 3, nightly_sleep_hours <= 16) |>
    group_by(SEQN, age) |>
    summarise(
      n_nights = n(),
      sleep_hours = mean(nightly_sleep_hours),
      weight = first(mec4yr),
      .groups = "drop"
    ) |>
    filter(n_nights >= 3) |>
    transmute(
      dataset = "NHANES",
      age,
      age_center = floor(age + 0.5),
      sleep_hours,
      weight
    )
}
