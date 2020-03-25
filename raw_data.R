# Required packages ####
library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)
library(data.table)
library(lubridate)

# Import data ####

raw <- read.csv("Tulsa Airport Hourly 2016-2020.csv", header = T, stringsAsFactors = F)

# Change date format

raw$DATE <- ymd_hms(raw$DATE, truncated = 3)

raw$DATE <- as.POSIXct(raw$DATE)

# Hourly data only

hourly.raw <- raw %>% 
  filter(REPORT_TYPE == "FM-15") %>% 
  select(DATE, HourlyDewPointTemperature, HourlyDryBulbTemperature, HourlyPrecipitation, HourlyRelativeHumidity, HourlySkyConditions, HourlyWetBulbTemperature, HourlyWindDirection, HourlyWindSpeed, ) %>% 
  rename(time = "DATE",
         dewpoint = "HourlyDewPointTemperature",
         dry.bulb = "HourlyDryBulbTemperature",
         precipitation = "HourlyPrecipitation",
         relative.humidity = "HourlyRelativeHumidity",
         sky.condition = "HourlySkyConditions",
         wet.bulb = "HourlyWetBulbTemperature",
         wind.direction = "HourlyWindDirection",
         wind.speed = "HourlyWindSpeed")


# Turn precipitation "T" into "0.001"

hourly.raw <- hourly.raw %>% 
  mutate(precipitation = replace(hourly.raw$precipitation, hourly.raw$precipitation == "T", "0.001"))

# Set columns that are numeric

num.columns <- c("dewpoint", "dry.bulb", "precipitation", "relative.humidity", "wet.bulb", "wind.direction", "wind.speed")

# Transform certain columns to numeric

hourly.raw <- mutate_each(hourly.raw, funs(as.numeric), all_of(num.columns))

# Exploratory Analysis ####

hourly.raw %>% 
  filter(between(wet.bulb, 65, 75)) %>% 
  count(sort = T)
