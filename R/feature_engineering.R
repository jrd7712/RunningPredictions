library(tidyverse)
library(lubridate)
library(slider)
library(stringr)

source("R/parse_health.R")

health <- parse_health_fast("data_raw/apple_health_export/export.xml")

records <- health$records
workouts <- health$workouts

#----------------------------------
# Filter running workouts
#----------------------------------

runs <- workouts %>%
  filter(str_detect(type, "Running")) %>%
  mutate(
    date = as_date(start),
    pace = if_else(!is.na(distance) & distance > 0, duration / distance, NA_real_)
  )

#----------------------------------
# Heart rate samples from Apple Watch
#----------------------------------

hr_samples <- records %>%
  filter(type == "HKQuantityTypeIdentifierHeartRate") %>%
  mutate(
    start = as.POSIXct(start),
    value = as.numeric(value)
  )

#----------------------------------
# Compute avg_hr and max_hr for each run
#----------------------------------

runs <- runs %>%
  rowwise() %>%
  mutate(
    avg_hr = mean(
      hr_samples$value[
        hr_samples$start >= start &
          hr_samples$start <= end
      ],
      na.rm = TRUE
    ),
    max_hr = max(
      hr_samples$value[
        hr_samples$start >= start &
          hr_samples$start <= end
      ],
      na.rm = TRUE
    )
  ) %>%
  ungroup()

#----------------------------------
# Daily HRV
#----------------------------------

daily_hrv <- records %>%
  filter(type == "HKQuantityTypeIdentifierHeartRateVariabilitySDNN") %>%
  mutate(date = as_date(start)) %>%
  group_by(date) %>%
  summarize(hrv = mean(as.numeric(value), na.rm = TRUE), .groups = "drop")

#----------------------------------
# Daily Resting HR
#----------------------------------

daily_rhr <- records %>%
  filter(type == "HKQuantityTypeIdentifierRestingHeartRate") %>%
  mutate(date = as_date(start)) %>%
  group_by(date) %>%
  summarize(rhr = mean(as.numeric(value), na.rm = TRUE), .groups = "drop")

#----------------------------------
# Daily VO2 Max
#----------------------------------

daily_vo2 <- records %>%
  filter(type == "HKQuantityTypeIdentifierVO2Max") %>%
  mutate(date = as_date(start)) %>%
  group_by(date) %>%
  summarize(vo2 = mean(as.numeric(value), na.rm = TRUE), .groups = "drop")

#----------------------------------
# Join daily metrics
#----------------------------------

runs <- runs %>%
  left_join(daily_hrv, by = "date") %>%
  left_join(daily_rhr, by = "date") %>%
  left_join(daily_vo2, by = "date")

# Ensure columns exist (important for slider)

if (!"hrv" %in% names(runs)) runs$hrv <- NA_real_
if (!"rhr" %in% names(runs)) runs$rhr <- NA_real_
if (!"vo2" %in% names(runs)) runs$vo2 <- NA_real_

#----------------------------------
# Rolling metrics
#----------------------------------

runs <- runs %>%
  arrange(date) %>%
  mutate(
    hrv_7d = slide_dbl(hrv, mean, .before = 6, .complete = TRUE),
    rhr_7d = slide_dbl(rhr, mean, .before = 6, .complete = TRUE),
    mileage_7d = slide_dbl(distance, sum, .before = 6, .complete = TRUE)
  )

#----------------------------------
# Feature engineering
#----------------------------------

runs <- runs %>%
  mutate(
    running_economy = if_else(!is.na(avg_hr) & avg_hr > 0, pace / avg_hr, NA_real_),
    hr_drift = if_else(!is.na(avg_hr) & !is.na(max_hr), (max_hr - avg_hr) / duration, NA_real_),
    long_run_ratio = if_else(!is.na(mileage_7d) & mileage_7d > 0, distance / mileage_7d, NA_real_)
  )

#----------------------------------
# Save features
#----------------------------------

saveRDS(runs, "data_processed/runs_features.rds")

