## set working directory
setwd("~/Desktop/VEIC-II")

## load libraries
library(lubridate)  ## for date/time handling
library(ggplot2)  ## for plotting
library(dplyr)  ## for groupby/summarize analysis

## read in csv files
pressure_temperature <- read.csv("baseline-pressure-temp.csv", skip = 3, 
                                 colClasses = c("character", "numeric", "numeric", "numeric"), 
                                 na.strings = c("#N/A", "<NoData>"))
load_power <- read.csv("baseline-load-power.csv",
                       colClasses = c("character", "numeric", "numeric", "numeric"))
exp_period <- read.csv("efficiency.csv", skip = 5)

## add a usable date column to each dataset
pressure_temperature$date <- mdy_hm(pressure_temperature$Date...Time)
load_power$date <- mdy_hm(load_power$Date...Time)
exp_period$date <- mdy_hm(exp_period$Date...Time)

## merge the pressure/temp dataset with the load power dataset
control_series <- merge(pressure_temperature, load_power, by="date")

## adding columns to control series:
## add a month column for easy grouping by month
control_series$month <- month(control_series$date)

## add a column with load in kW instead of tons so the coefficient of performance will be unitless
control_series$load.kW <- control_series$Load..tons. * 3.51685

## add a coefficient of performance
control_series$COP <- control_series$load.kW / control_series$Refr.compressor.power..kW.


## ANALYSIS
## STEP 1: how big a problem might high wet bulb temps present?
## identify occurrences when condenser fans are not lowering the discharge pressure to within 1 SD of target
## find standard deviation of discharge pressures over 2012
P_one_sd <- sd(control_series$Discharge.pressure..psig., na.rm=TRUE)
P_mean <- mean(control_series$Discharge.pressure..psig., na.rm=TRUE)

## flag points above 1 SD of discharge pressure
control_series$highP <- cut(control_series$Discharge.pressure..psig., 
                            breaks = c(0, P_mean + P_one_sd, 500), 
                            labels = c("normal", "high"))

## how many "high pressure" periods by month and what fraction do they represent:
P_by_month <- group_by(control_series, month, highP) %>%
                summarize(n = n()) %>%
                mutate(fraction = n / sum(n))
high_P_by_month <- filter(P_by_month, highP == "high")

## plot discharge pressure vs. wet bulb temp, highlighting those points more than 1 SD above target
g <- ggplot(control_series, aes(x=Twb..F., y=Discharge.pressure..psig.)) + 
    geom_point(aes(alpha=.12, color=highP), na.rm = TRUE) +
    coord_cartesian(xlim = c(-10, 80), ylim = c(70,180))

g + facet_wrap( ~ month)

## plot fraction of "high" pressure readings by month
ggplot(high_P_by_month, aes(x=factor(month), y=fraction)) + 
    geom_bar(stat="identity") +
    geom_text(aes(color="white", label=round(fraction, digits=2)), vjust=1.5)

## how does the COP vary with wet bulb temperature at different loads?
## cut the load index into different bins
control_series$load_category <- cut(control_series$load.kW,
                                    breaks = c(-1, 1, 200, 800, 1200, 2200),
                                    labels = c("No Load", "Very Low", "Low", "Medium", "High"))

## plot of all months and load categories (very low < 200kW < low < 800kW < medium < 1200kW < high < 2200kW)
h <- ggplot(filter(control_series, load_category != "No Load"), aes(x=Twb..F., y=COP)) +
    geom_point(aes(alpha = .12, color = Twb..F.), na.rm = TRUE) + 
    scale_color_gradient(low="blue", high="red")
h + facet_grid(month ~ load_category) + geom_rug(position="jitter", size = 0.1)

## example plot of COP suffering because condenser fans aren't removing enough heat before refrigerant returns to the compressor
## July 2012
ggplot(filter(control_series, load_category == "High", month == 7), aes(x=Twb..F., y=COP)) +
    geom_point(aes(alpha = .12, color = Twb..F.), na.rm = TRUE) + 
    scale_color_gradient(low="blue", high="red") +
    coord_cartesian(xlim = c(48, 80))

