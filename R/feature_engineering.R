source("R/parse_health.R")

data <- parse_health("data_raw/export.xml")
records <- data$records
workouts <- data$workouts

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
    running_economy = pace / avg_hr,
    hr_drift = (max_hr - avg_hr) / duration,
    long_run_ratio = distance / mileage_7d
  )

saveRDS(runs, "data_processed/runs_features.rds")