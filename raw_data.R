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
  filter(REPORT_TYPE == "FM-15")

