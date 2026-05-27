# LOAD PACKAGES ----------------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))

library(dplyr)
library(readr)

dir.create(outputs_dir, showWarnings = FALSE)

## LOAD FFCWS DATA ------------------------------------------------------------

ffcws_sleep <- read_csv(ffcws_sleep_path, show_col_types = FALSE)
ffcws_demo <- read_csv(ffcws_demo_path, show_col_types = FALSE)

## COMPUTE FFCWS PERCENTILES --------------------------------------------------

ffcws_person_sleep <- ffcws_sleep |>
  left_join(
    ffcws_demo |> select(nsrrid, nsrr_age),
    by = c("idnum" = "nsrrid")
  ) |>
  filter(
    a6_wavestatus ==
      "1 Non-missing; provided at least 1 valid day of actigraphy",
    !is.na(a6_nightsleepdur_mins_mn),
    !is.na(nsrr_age)
  ) |>
  mutate(
    mean_duration = a6_nightsleepdur_mins_mn / 60,
    age = nsrr_age
  )

ffcws_percentiles <- compute_age_percentiles(
  ffcws_person_sleep,
  age_col = "age",
  value_col = "mean_duration",
  bin_width = 0.5,
  min_n = 5
)

## SAVE OUTPUT ----------------------------------------------------------------

write_csv(ffcws_percentiles, file.path(outputs_dir, "ffcws-percentiles.csv"))
