library(xml2)
library(data.table)
library(lubridate)
library(slider)

parse_health_fast <- function(xml_path) {
  
  xml <- read_xml(xml_path)
  
  #-----------------------------
  # Records
  #-----------------------------
  rec_nodes <- xml_find_all(xml, "//Record")
  
  records <- data.table(
    type  = xml_attr(rec_nodes, "type"),
    start = ymd_hms(xml_attr(rec_nodes, "startDate"), tz="America/New_York"),
    end   = ymd_hms(xml_attr(rec_nodes, "endDate"),   tz="America/New_York"),
    value = suppressWarnings(as.numeric(xml_attr(rec_nodes, "value")))
  )
  
  #-----------------------------
  # Workouts
  #-----------------------------
  wk_nodes <- xml_find_all(xml, "//Workout")
  
  workouts <- data.table(
    workout_id = seq_along(wk_nodes),
    type       = xml_attr(wk_nodes, "workoutActivityType"),
    start      = ymd_hms(xml_attr(wk_nodes, "startDate"), tz="America/New_York"),
    end        = ymd_hms(xml_attr(wk_nodes, "endDate"),   tz="America/New_York"),
    duration   = as.numeric(xml_attr(wk_nodes, "duration")),
    distance   = suppressWarnings(as.numeric(xml_attr(wk_nodes, "totalDistance"))),
    energy     = suppressWarnings(as.numeric(xml_attr(wk_nodes, "totalEnergyBurned")))
  )
  
  #-----------------------------
  # Heart Rate Interval Join (Corrected)
  #-----------------------------
  
  # HR samples
  hr <- records[type == "HKQuantityTypeIdentifierHeartRate" & !is.na(value)]
  hr[, hr_start := start]
  hr[, hr_end   := start]   # zero-length interval
  
  # Workout intervals
  wk_intervals <- workouts[, .(workout_id, wk_start = start, wk_end = end)]
  
  # Set keys for overlap
  setkey(hr, hr_start, hr_end)
  setkey(wk_intervals, wk_start, wk_end)
  
  # Interval join
  hr_join <- foverlaps(
    hr[, .(hr_start, hr_end, value)],
    wk_intervals,
    by.x = c("hr_start", "hr_end"),
    by.y = c("wk_start", "wk_end"),
    type = "within",
    nomatch = 0
  )
  
  # Aggregate HR stats
  hr_stats <- hr_join[
    ,
    .(
      avg_hr = mean(value, na.rm = TRUE),
      max_hr = max(value, na.rm = TRUE)
    ),
    by = workout_id
  ]
  
  # Merge back
  workouts <- merge(workouts, hr_stats, by = "workout_id", all.x = TRUE)
  
  #-----------------------------
  # Daily Metrics
  #-----------------------------
  records[, date := as_date(start)]
  
  daily_hrv <- records[
    type == "HKQuantityTypeIdentifierHeartRateVariabilitySDNN",
    .(hrv = mean(value, na.rm = TRUE)),
    by = date
  ]
  
  daily_rhr <- records[
    type == "HKQuantityTypeIdentifierRestingHeartRate",
    .(rhr = mean(value, na.rm = TRUE)),
    by = date
  ]
  
  daily_vo2 <- records[
    type == "HKQuantityTypeIdentifierVO2Max",
    .(vo2 = mean(value, na.rm = TRUE)),
    by = date
  ]
  
  workouts[, date := as_date(start)]
  
  workouts <- merge(workouts, daily_hrv, by="date", all.x=TRUE)
  workouts <- merge(workouts, daily_rhr, by="date", all.x=TRUE)
  workouts <- merge(workouts, daily_vo2, by="date", all.x=TRUE)
  
  # Ensure columns exist
  if (!"hrv" %in% names(workouts)) workouts[, hrv := NA_real_]
  if (!"rhr" %in% names(workouts)) workouts[, rhr := NA_real_]
  if (!"vo2" %in% names(workouts)) workouts[, vo2 := NA_real_]
  
  #-----------------------------
  # Rolling Metrics
  #-----------------------------
  setorder(workouts, date)
  
  if (nrow(workouts) > 0) {
    workouts[, hrv_7d := slide_dbl(hrv, mean, .before=6, .complete=TRUE)]
    workouts[, rhr_7d := slide_dbl(rhr, mean, .before=6, .complete=TRUE)]
    workouts[, vo2_7d := slide_dbl(vo2, mean, .before=6, .complete=TRUE)]
    workouts[, mileage_7d := slide_dbl(distance, sum, .before=6, .complete=TRUE)]
  } else {
    workouts[, `:=`(
      hrv_7d = numeric(),
      rhr_7d = numeric(),
      vo2_7d = numeric(),
      mileage_7d = numeric()
    )]
  }
  
  list(
    records = records,
    workouts = workouts
  )
}