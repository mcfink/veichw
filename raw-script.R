## set working directory
setwd("~/Desktop/VEIC-II")

## load libraries
library(lubridate)  ## for date/time handling
library(ggplot2)  ## for plotting

## read in csv files
pressure_temperature <- read.csv("baseline-pressure-temp.csv", skip = 3)
load_power <- read.csv("baseline-load-power.csv")
exp_period <- read.csv("efficiency.csv", skip = 5)

## add a usable date column to each dataset
pressure_temperature$date <- mdy_hm(pressure_temperature$Date...Time)
load_power$date <- mdy_hm(load_power$Date...Time)
exp_period$date <- mdy_hm(exp_period$Date...Time)

## merge the pressure/temp dataset with the load power dataset
control_series <- merge(pressure_temperature, load_power, by="date")

## adding columns to control series:
## add a column with load in kW instead of tons
control_series$load.kW <- control_series$Load..tons. * 3.51685

## add a coefficient of performance
control_series$COP <- control_series$load.kW / control_series$Refr.compressor.power..kW.


## ANALYSIS
## identify times when condenser fans are not lowering the discharge pressure to 


