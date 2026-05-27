# LOAD PACKAGES ----------------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))

library(dplyr)
library(readr)

dir.create(outputs_dir, showWarnings = FALSE)

## LOAD ABCD DATA -------------------------------------------------------------

abcd_nights <- readRDS(abcd_rds_path) |>
  mutate(
    day_type = if_else(dt_wknd, "weekend", "weekday")
  )

## SUMMARIZE ABCD SLEEP -------------------------------------------------------

abcd_person_sleep <- abcd_nights |>
  group_by(participant_id, session_id, age) |>
  summarise(
    n_weekday = sum(day_type == "weekday" & !is.na(sleep_duration_hours)),
    n_weekend = sum(day_type == "weekend" & !is.na(sleep_duration_hours)),
    mean_weekday = mean(
      sleep_duration_hours[day_type == "weekday"],
      na.rm = TRUE
    ),
    mean_weekend = mean(
      sleep_duration_hours[day_type == "weekend"],
      na.rm = TRUE
    ),
    weighted_duration = ((5 * mean_weekday) + (2 * mean_weekend)) / 7,
    .groups = "drop"
  ) |>
  filter(n_weekday >= 5, n_weekend >= 2, is.finite(weighted_duration))

abcd_percentiles <- compute_age_percentiles(
  abcd_person_sleep,
  age_col = "age",
  value_col = "weighted_duration",
  bin_width = 0.5,
  min_n = 5
)

## SAVE OUTPUTS ---------------------------------------------------------------

abcd_summary <- list(
  n_participant_visits = nrow(abcd_person_sleep),
  n_unique_youths = n_distinct(abcd_person_sleep$participant_id),
  mean_age = mean(abcd_person_sleep$age, na.rm = TRUE),
  min_age = min(abcd_person_sleep$age, na.rm = TRUE),
  max_age = max(abcd_person_sleep$age, na.rm = TRUE),
  mean_weighted_duration = mean(
    abcd_person_sleep$weighted_duration,
    na.rm = TRUE
  ),
  sd_weighted_duration = sd(abcd_person_sleep$weighted_duration, na.rm = TRUE),
  median_p10 = median(abcd_percentiles$p10, na.rm = TRUE),
  median_p50 = median(abcd_percentiles$p50, na.rm = TRUE),
  median_p90 = median(abcd_percentiles$p90, na.rm = TRUE)
)

write_csv(abcd_percentiles, file.path(outputs_dir, "abcd-percentiles.csv"))
saveRDS(abcd_summary, file.path(outputs_dir, "abcd-summary.rds"))
