# PREPARE COHORTS --------------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))

library(arrow)
library(dplyr)
library(haven)
library(readr)

## ABCD ------------------------------------------------------------------------

abcd_sleep <- read_parquet(abcd_fitbit_path) |>
  filter(qc_300min) |>
  transmute(
    participant_id,
    session_id,
    dt,
    dt_wknd,
    sleep_duration_hours = min_total_slp / 60
  )

abcd_demo <- read_parquet(abcd_stc_path) |>
  transmute(
    participant_id,
    dob = as.Date(ab_g_stc__cohort_dob),
    sex = if_else(ab_g_stc__cohort_sex == 1, "Male", "Female")
  )

abcd_session_age <- abcd_sleep |>
  group_by(participant_id, session_id) |>
  summarise(
    fitbit_mid_date = as.Date(
      median(as.numeric(dt), na.rm = TRUE),
      origin = "1970-01-01"
    ),
    .groups = "drop"
  ) |>
  left_join(abcd_demo, by = "participant_id") |>
  mutate(age = as.numeric(fitbit_mid_date - dob) / 365.25)

abcd <- abcd_sleep |>
  left_join(abcd_session_age, by = c("participant_id", "session_id")) |>
  mutate(day_type = if_else(dt_wknd, "weekend", "weekday")) |>
  group_by(participant_id, session_id, age) |>
  summarise(
    sex = first(sex),
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
    sleep_hours = ((5 * mean_weekday) + (2 * mean_weekend)) / 7,
    .groups = "drop"
  ) |>
  filter(n_weekday >= 5, n_weekend >= 2, is.finite(sleep_hours)) |>
  transmute(
    dataset = "ABCD",
    participant_id = as.character(participant_id),
    age,
    age_center = floor(age + 0.5),
    sex,
    sleep_hours,
    weight = 1,
    psu = NA_real_,
    strata = NA_real_
  )

## NHANES ----------------------------------------------------------------------

nhanes_sleep <- read_csv(nhanes_sleep_path, show_col_types = FALSE)
nhanes_demo <- bind_rows(lapply(nhanes_demo_paths, read_xpt))

nhanes <- nhanes_sleep |>
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
  mutate(age = RIDAGEYR, nightly_sleep_hours = dur_spt_min / 60) |>
  filter(nightly_sleep_hours >= 3, nightly_sleep_hours <= 16) |>
  group_by(SEQN, age) |>
  summarise(
    n_nights = n(),
    sleep_hours = mean(nightly_sleep_hours),
    weight = first(mec4yr),
    psu = first(SDMVPSU),
    strata = first(SDMVSTRA),
    .groups = "drop"
  ) |>
  filter(n_nights >= 3) |>
  transmute(
    dataset = "NHANES",
    participant_id = as.character(SEQN),
    age,
    age_center = floor(age + 0.5),
    sex = NA_character_,
    sleep_hours,
    weight,
    psu,
    strata
  )

## FFCWS -----------------------------------------------------------------------

ffcws_sleep <- read_csv(ffcws_sleep_path, show_col_types = FALSE)
ffcws_demo <- read_csv(ffcws_demo_path, show_col_types = FALSE)

ffcws <- ffcws_sleep |>
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
  transmute(
    dataset = "FFCWS",
    participant_id = as.character(idnum),
    age = nsrr_age,
    age_center = floor(nsrr_age + 0.5),
    sex = NA_character_,
    sleep_hours = a6_nightsleepdur_mins_mn / 60,
    weight = 1,
    psu = NA_real_,
    strata = NA_real_
  )

## PATS ------------------------------------------------------------------------

pats <- read_csv(
  pats_path,
  col_select = c(
    public_subject_id,
    studyinfo_age_at_randomization,
    avgsleepperiodduration
  ),
  show_col_types = FALSE
) |>
  filter(
    !is.na(studyinfo_age_at_randomization),
    !is.na(avgsleepperiodduration)
  ) |>
  transmute(
    dataset = "PATS",
    participant_id = as.character(public_subject_id),
    age = studyinfo_age_at_randomization,
    age_center = floor(studyinfo_age_at_randomization + 0.5),
    sex = NA_character_,
    sleep_hours = avgsleepperiodduration / 60,
    weight = 1,
    psu = NA_real_,
    strata = NA_real_
  )

## COMBINE DATASETS ------------------------------------------------------------

cohorts <- bind_rows(abcd, nhanes, ffcws, pats) |>
  arrange(dataset, age, participant_id)

dir.create(data_sets_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(cohorts, file.path(data_sets_dir, "cohorts.csv"))

cohorts |>
  group_by(dataset) |>
  summarise(
    n_rows = n(),
    n_ids = n_distinct(participant_id),
    min_age = min(age),
    max_age = max(age),
    .groups = "drop"
  ) |>
  print()
