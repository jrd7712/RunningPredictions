library(xml2)
library(tidyverse)
library(lubridate)

parse_health <- function(xml_path) {
  
  #-----------------------------
  # Load XML
  #-----------------------------
  xml <- read_xml(xml_path)
  
  #-----------------------------
  # Parse all <Record> entries
  #-----------------------------
  records <- xml %>%
    xml_find_all("//Record") %>%
    map_df(function(node) {
      tibble(
        type  = xml_attr(node, "type"),
        start = ymd_hms(xml_attr(node, "startDate"), tz = "America/New_York"),
        end   = ymd_hms(xml_attr(node, "endDate"),   tz = "America/New_York"),
        value = xml_attr(node, "value")
      )
    }) %>%
    mutate(value = suppressWarnings(as.numeric(value)))
  
  #-----------------------------
  # Parse all <Workout> entries
  #-----------------------------
  workouts <- xml %>%
    xml_find_all("//Workout") %>%
    map_df(function(node) {
      tibble(
        type     = xml_attr(node, "workoutActivityType"),
        start    = ymd_hms(xml_attr(node, "startDate"), tz = "America/New_York"),
        end      = ymd_hms(xml_attr(node, "endDate"),   tz = "America/New_York"),
        duration = as.numeric(xml_attr(node, "duration")),
        distance = as.numeric(xml_attr(node, "totalDistance")),
        energy   = as.numeric(xml_attr(node, "totalEnergyBurned"))
      )
    })
  
  #-----------------------------
  # Extract heart-rate samples
  #-----------------------------
  hr <- records %>%
    filter(type == "HKQuantityTypeIdentifierHeartRate") %>%
    drop_na(value)
  
  #-----------------------------
  # Compute avg_hr and max_hr per workout
  #-----------------------------
  workouts <- workouts %>%
    rowwise() %>%
    mutate(
      avg_hr = mean(hr$value[hr$start >= start & hr$start <= end], na.rm = TRUE),
      max_hr = max(hr$value[hr$start >= start & hr$start <= end], na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Replace NaN (from mean of empty set) with NA
  workouts <- workouts %>%
    mutate(
      avg_hr = ifelse(is.nan(avg_hr), NA, avg_hr),
      max_hr = ifelse(is.nan(max_hr), NA, max_hr)
    )
  
  #-----------------------------
  # Return parsed data
  #-----------------------------
  list(
    records = records,
    workouts = workouts
  )
}