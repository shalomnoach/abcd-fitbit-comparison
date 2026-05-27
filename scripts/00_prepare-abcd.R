## SET PATHS ------------------------------------------------------------------
# Paths are relative to the repo root.

# install.packages(c("arrow", "dplyr", "here"))

library(arrow)
library(dplyr)
library(here)

FITBIT_PATH <- here(
  "..", "..", "Dissertation Data", "abcd", "concatenated",
  "novel_technologies", "fitbit", "fitbit_ss_sleep_day.parquet"
)
STC_PATH <- here(
  "..", "..", "Dissertation Data", "abcd", "rawdata",
  "phenotype", "ab_g_stc.parquet"
)
OUTPUT_PATH <- here("..", "..", "Dissertation Data", "sleep_data_complete_abcd70.rds")

## LOAD SLEEP DATA -------------------------------------------------------------

sleep_clean <- read_parquet(FITBIT_PATH) |>
  filter(qc_300min == TRUE) |>
  transmute(
    participant_id,
    session_id,
    dt,
    dt_wknd,
    sleep_duration_hours = min_total_slp / 60
  )

## LOAD DATE OF BIRTH ----------------------------------------------------------

dob_data <- read_parquet(STC_PATH) |>
  transmute(
    participant_id,
    dob = as.Date(ab_g_stc__cohort_dob)
  )

## COMPUTE SESSION AGE ---------------------------------------------------------

# Use the median Fitbit wear date within each session to compute age.
fitbit_session_midpoint <- sleep_clean |>
  group_by(participant_id, session_id) |>
  summarise(
    fitbit_mid_date = as.Date(
      median(as.numeric(dt), na.rm = TRUE),
      origin = "1970-01-01"
    ),
    .groups = "drop"
  )

## MERGE DATA ------------------------------------------------------------------

sleep_data_complete <- sleep_clean |>
  left_join(dob_data, by = "participant_id") |>
  left_join(fitbit_session_midpoint, by = c("participant_id", "session_id")) |>
  transmute(
    participant_id,
    session_id,
    dt_wknd,
    sleep_duration_hours,
    age = as.numeric(fitbit_mid_date - dob) / 365.25
  )

## SAVE OUTPUT -----------------------------------------------------------------

saveRDS(sleep_data_complete, OUTPUT_PATH)
