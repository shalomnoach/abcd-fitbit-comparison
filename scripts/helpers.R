# Format an integer with comma separators.
fmt_int <- function(x) {
  prettyNum(as.integer(round(x)), big.mark = ",")
}

# Format a number with two decimal places.
fmt2 <- function(x) {
  formatC(x, format = "f", digits = 2)
}

# Format a range using the two-decimal helper above.
fmt_range <- function(x_min, x_max) {
  paste0(fmt2(x_min), " to ", fmt2(x_max))
}

# Compute age-binned percentile curves for a sleep-duration variable.
compute_age_percentiles <- function(
  df,
  age_col,
  value_col,
  bin_width = 0.5,
  min_n = 5
) {
  age_bins <- seq(
    floor(min(df[[age_col]], na.rm = TRUE)),
    ceiling(max(df[[age_col]], na.rm = TRUE)),
    by = bin_width
  )

  df |>
    mutate(
      age_bin = cut(.data[[age_col]], breaks = age_bins, include.lowest = TRUE)
    ) |>
    group_by(age_bin) |>
    summarise(
      age_mid = mean(.data[[age_col]], na.rm = TRUE),
      p10 = quantile(.data[[value_col]], 0.10, na.rm = TRUE),
      p25 = quantile(.data[[value_col]], 0.25, na.rm = TRUE),
      p50 = quantile(.data[[value_col]], 0.50, na.rm = TRUE),
      p75 = quantile(.data[[value_col]], 0.75, na.rm = TRUE),
      p90 = quantile(.data[[value_col]], 0.90, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    filter(n >= min_n)
}

# Summarize the signed difference between ABCD and a reference median curve.
signed_diff_summary <- function(abcd_df, ref_df) {
  overlap <- abcd_df |>
    filter(
      age_mid >= min(ref_df$age, na.rm = TRUE),
      age_mid <= max(ref_df$age, na.rm = TRUE)
    )

  ref_interp <- approx(
    x = ref_df$age,
    y = ref_df$p50,
    xout = overlap$age_mid,
    rule = 2
  )$y

  diffs <- overlap$p50 - ref_interp

  list(
    mean = mean(diffs, na.rm = TRUE),
    min  = min(diffs,  na.rm = TRUE),
    max  = max(diffs,  na.rm = TRUE)
  )
}
