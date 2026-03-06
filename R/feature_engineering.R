library(tidyverse)
library(lubridate)
library(slider)
library(stringr)

source("R/parse_health.R")

health <- parse_health("data_raw/sample_export/export.xml")
records <- health$records
workouts <- health$workouts

runs <- workouts %>%
  filter(str_detect(type, "Running")) %>%
  mutate(
    pace = duration / distance,
    date = as_date(start)
  )

daily_hrv <- records %>%
  filter(type == "HKQuantityTypeIdentifierHeartRateVariabilitySDNN") %>%
  mutate(date = as_date(start)) %>%
  group_by(date) %>%
  summarize(hrv = mean(as.numeric(value), na.rm = TRUE))

daily_rhr <- records %>%
  filter(type == "HKQuantityTypeIdentifierRestingHeartRate") %>%
  mutate(date = as_date(start)) %>%
  group_by(date) %>%
  summarize(rhr = mean(as.numeric(value), na.rm = TRUE))

daily_vo2 <- records %>%
  filter(type == "HKQuantityTypeIdentifierVO2Max") %>%
  mutate(date = as_date(start)) %>%
  group_by(date) %>%
  summarize(vo2 = mean(as.numeric(value), na.rm = TRUE))

runs <- runs %>%
  left_join(daily_hrv, by = "date") %>%
  left_join(daily_rhr, by = "date") %>%
  left_join(daily_vo2, by = "date")

runs <- runs %>%
  arrange(date) %>%
  mutate(
    hrv_7d = slide_dbl(hrv, mean, .before = 6, .complete = TRUE),
    rhr_7d = slide_dbl(rhr, mean, .before = 6, .complete = TRUE),
    mileage_7d = slide_dbl(distance, sum, .before = 6, .complete = TRUE)
  )

runs <- runs %>%
  mutate(
    avg_hr = if (!"avg_hr" %in% colnames(.)) NA_real_ else avg_hr,
    max_hr = if (!"max_hr" %in% colnames(.)) NA_real_ else max_hr
  )

runs <- runs %>%
  mutate(
    running_economy = if_else(!is.na(avg_hr) & avg_hr > 0, pace / avg_hr, NA_real_),
    hr_drift = if_else(!is.na(avg_hr) & !is.na(max_hr), (max_hr - avg_hr) / duration, NA_real_),
    long_run_ratio = if_else(!is.na(mileage_7d) & mileage_7d > 0, distance / mileage_7d, NA_real_)
  )

saveRDS(runs, "data_processed/runs_features.rds")