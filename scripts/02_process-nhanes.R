# LOAD PACKAGES ----------------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))

library(dplyr)
library(haven)
library(readr)
library(survey)

dir.create(outputs_dir, showWarnings = FALSE)

## LOAD NHANES DATA -----------------------------------------------------------

nhanes_sleep <- read_csv(nhanes_sleep_path, show_col_types = FALSE)
nhanes_demo <- bind_rows(lapply(nhanes_demo_paths, read_xpt))

## COMPUTE NHANES PERCENTILES -------------------------------------------------

nhanes_person_sleep <- nhanes_sleep |>
  left_join(nhanes_demo |> select(SEQN, RIDAGEYR), by = "SEQN") |>
  filter(
    !is.na(dur_spt_min),
    dur_spt_min > 0,
    !is.na(RIDAGEYR),
    RIDAGEYR >= 0,
    RIDAGEYR <= 19,
    !is.na(mec4yr),
    mec4yr > 0
  ) |>
  mutate(
    sleep_duration_hours = dur_spt_min / 60,
    age = RIDAGEYR
  ) |>
  group_by(SEQN, age) |>
  summarise(
    n_nights = n(),
    mean_duration = mean(sleep_duration_hours, na.rm = TRUE),
    mec4yr = first(mec4yr),
    SDMVPSU = first(SDMVPSU),
    SDMVSTRA = first(SDMVSTRA),
    .groups = "drop"
  ) |>
  filter(n_nights >= 3)

nhanes_design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~mec4yr,
  nest = TRUE,
  data = nhanes_person_sleep
)

nhanes_percentiles <- bind_rows(lapply(3:19, function(age_val) {
  age_design <- subset(nhanes_design, age == age_val)
  n_obs <- nrow(age_design$variables)

  if (n_obs < 5) {
    return(NULL)
  }

  quantiles <- svyquantile(
    ~mean_duration,
    age_design,
    quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90),
    ci = FALSE
  )
  quant_vals <- as.vector(quantiles[[1]])

  tibble(
    age_mid = age_val,
    p10 = quant_vals[1],
    p25 = quant_vals[2],
    p50 = quant_vals[3],
    p75 = quant_vals[4],
    p90 = quant_vals[5],
    n = n_obs
  )
}))

## SAVE OUTPUT ----------------------------------------------------------------

write_csv(nhanes_percentiles, file.path(outputs_dir, "nhanes-percentiles.csv"))
