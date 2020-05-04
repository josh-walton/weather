# Required packages ####
library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)
library(data.table)
library(lubridate)
library(forecast)
library(stats)
library(forcats)

# Import data ####

raw <- read.csv("Tulsa Airport Hourly 2016-2020.csv", header = T, stringsAsFactors = F)

# Tidying data ####

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
         wind.speed = "HourlyWindSpeed") %>% 
  drop_na()

# Set sky condition variables

cloud.conditions <- c("CLR", "FEW", "BKN", "SCT", "OVC")

hourly.raw$sky.code <- str_sub(hourly.raw$sky.condition, 1, 3)
hourly.raw$oktas <- str_sub(hourly.raw$sky.condition, 5, 6)

# Remove codes, only need oktas per hour

hourly.raw <- hourly.raw %>% 
  select(time, dewpoint, dry.bulb, wet.bulb, precipitation, relative.humidity, oktas, wind.direction, wind.speed)

# Turn precipitation "T" into "0.001"

hourly.raw <- hourly.raw %>% 
  mutate(precipitation = replace(hourly.raw$precipitation, hourly.raw$precipitation == "T", "0.001"))

# Set columns that are numeric

num.columns <- c("dewpoint", "dry.bulb", "precipitation", "relative.humidity", "wet.bulb", "wind.direction", "wind.speed", "oktas")

# Transform certain columns to numeric

hourly.raw <- mutate_each(hourly.raw, funs(as.numeric), all_of(num.columns))

# Filter times to be daytime hours

active.hours <- hourly.raw %>% 
  filter(hour(time) >= 6 & hour(time) < 22)

# Summarize data by date with daytime hours

active.hours$date <- as.Date(active.hours$time)

day.summary <- active.hours %>% 
  group_by(date) %>% 
  summarise(temp.avg = mean(dry.bulb),
            dewpoint.avg = mean(dewpoint),
            precipitation.avg = mean(precipitation),
            oktas.avg = mean(oktas),
            windspeed.avg = mean(wind.speed))

# Exploratory Analysis ####

# Filter day.summary with parameters

perfect.days <- day.summary %>% 
  group_by(date) %>% 
  filter(between(temp.avg, 65, 75),
         dewpoint.avg < 60,
         precipitation.avg == 0,
         oktas.avg <= 6,
         windspeed.avg < 15)


# Plots ####

# Bar graph of perfect days per month

# Previous code with year ####
perf.days.count <- perfect.days %>% 
  group_by(month = floor_date(date, "month")) %>% 
  count(date, sort = T) %>% 
  summarise(days = sum(n))


perf.days.count %>% 
  ggplot(aes(x = month, y = days)) +
  stat_summary(fun = sum, geom = "bar") +
  facet_wrap(~year(month), scales = "free_x") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")


# With full months ####

perf.days.count <- perfect.days %>% 
  count(date, sort = T) %>% 
  summarise(count = sum(n))

perf.days.count <- perf.days.count %>% 
  mutate(month = month(date, label = T)) %>% 
  group_by(month) %>%
  summarise(perf.days = sum(count)) %>% 
  mutate(month = factor(month, levels = month.abb))
  
perf.days.count %>% 
  ggplot(aes(month, perf.days)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = month.abb)





