# Required packages ####
library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)
library(data.table)
library(lubridate)
library(forecast)
library(stats)

# Trial ####

t <- perf.days.count %>% 
  filter(month <= as.POSIXct("2017-01-01"))

t.plot <- t %>% 
  ggplot(aes(x = month, y = days)) +
  stat_summary(fun = sum, geom = "bar") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")

t.plot
