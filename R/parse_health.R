library(xml2)
library(tidyverse)
library(lubridate)

parse_health <- function(xml_path) {
  xml <- read_xml(xml_path)
  
  records <- xml %>%
    xml_find_all("//Record") %>%
    map_df(function(node) {
      tibble(
        type = xml_attr(node, "type"),
        start = ymd_hms(xml_attr(node, "startDate")),
        end = ymd_hms(xml_attr(node, "endDate")),
        value = xml_attr(node, "value")
      )
    })
  
  workouts <- xml %>%
    xml_find_all("//Workout") %>%
    map_df(function(node) {
      tibble(
        type = xml_attr(node, "workoutActivityType"),
        start = ymd_hms(xml_attr(node, "startDate")),
        end = ymd_hms(xml_attr(node, "endDate")),
        duration = as.numeric(xml_attr(node, "duration")),
        distance = as.numeric(xml_attr(node, "totalDistance")),
        energy = as.numeric(xml_attr(node, "totalEnergyBurned"))
      )
    })
  
  list(records = records, workouts = workouts)
}