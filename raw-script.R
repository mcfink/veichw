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

exp_period <- read.csv("efficiency.csv", skip = 5,
                       colClasses = c("character", "numeric", "numeric", "numeric"),
                       na.strings = c("#N/A", "<NoData>"))

## add a usable date column to each dataset
pressure_temperature$date <- mdy_hm(pressure_temperature$Date...Time)
load_power$date <- mdy_hm(load_power$Date...Time)
exp_period$date <- mdy_hm(exp_period$Date...Time)

## merge the pressure/temp dataset with the load power dataset
control_series <- merge(pressure_temperature, load_power, by="date")

## adding columns to control series:
## add a month column for easy grouping by month and a weekday column for grouping by day of the week
control_series$month <- month(control_series$date)
control_series$weekday <- wday(control_series$date)
exp_period$weekday <- wday(exp_period$date)

## add a column with load in kW instead of tons so the coefficient of performance will be unitless
control_series$load.kW <- control_series$Load..tons. * 3.51685
exp_period$load.kW <- exp_period$Load..tons. * 3.51685

## add a coefficient of performance
control_series$COP <- control_series$load.kW / control_series$Refr.compressor.power..kW.
exp_period$COP <- exp_period$load.kW / exp_period$Refr.compressor.power..kW.

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

control_series$month_name <- factor(month.name[control_series$month], levels= c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

## how many "high pressure" periods by month and what fraction do they represent:
P_by_month <- group_by(control_series, month, highP) %>%
                summarize(n = n()) %>%
                mutate(fraction = n / sum(n))
high_P_by_month <- filter(P_by_month, highP == "high")

## plot discharge pressure vs. wet bulb temp, highlighting those points more than 1 SD above target
g <- ggplot(control_series, aes(x=Twb..F., y=Discharge.pressure..psig.)) + 
    geom_point(aes(alpha=.12, color=highP), na.rm = TRUE) +
    coord_cartesian(xlim = c(-10, 80), ylim = c(70,180))

g + facet_wrap( ~ month_name, ncol=3)

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
    scale_color_gradient(limits=c(50, 77),low="blue", high="red") +
    coord_cartesian(xlim = c(48, 80))

## how much does the load depend on the day of the week?
load_by_month_wday <- group_by(control_series, month, weekday, load_category) %>%
    summarize(n = n()) %>%
    mutate(wday_fraction = n / sum(n))

i <- ggplot(load_by_month_wday, aes(x=weekday, y=wday_fraction, fill=load_category)) + geom_bar(stat="identity") + scale_fill_hue(c=45, l=80)
i + facet_wrap(~ month)

## bottom line: will wet bulb temps through april cause an apparent difference in coefficient of performance?
## group by month and compute a coefficient of performance for each group

## PART 2: THE EXPERIMENTAL DATA
## define intervals for the experimental data
exp_period$p_label <- cut(exp_period$date, breaks= as.POSIXct(c("2012-12-31", "2013-02-03", "2013-02-05", "2013-02-19", 
                                                                "2013-02-21", "2013-03-06", "2013-03-08", "2013-05-21")),
                                            labels = c("p145", "t1", "p140", "t2", "p135", "t3", "p130")) 

exp_period$load_category <- cut(exp_period$load.kW,
                                    breaks = c(-1, 1, 200, 800, 1200, 2200),
                                    labels = c("No Load", "Very Low", "Low", "Medium", "High"))

## lets make sure the load profile is similar for all the periods in question
load_by_period <- group_by(exp_period, p_label, load_category) %>%
    summarize(n = n()) %>%
    mutate(load_fraction = n / sum(n)) %>%
    filter(p_label != "t1" & p_label != "t2" & p_label != "t3")

## the load profiles reveal that the comparison isn't really all that fair:
ggplot(load_by_period, aes(x=p_label, y=load_fraction, fill=load_category)) + 
    geom_bar(stat="identity") + 
    scale_fill_hue(c=45, l=80)

## let's compare COPs for each load group independently among the 4 experimental groups
## straight average of all COP datapoints
straight_means <- group_by(exp_period, p_label, load_category) %>%
    summarize(n = mean(COP)) %>%
    filter(p_label != "t1" & p_label != "t2" & p_label != "t3" & load_category != "No Load") %>%
    mutate(cop_mean = ifelse(is.na(n), 0, n))

ggplot(str_means, aes(x=load_category, y=cop_mean, fill=p_label)) +
    geom_bar(stat="identity",position="dodge") + 
    scale_fill_hue(c=45, l=80)

## COP averages weighted by load
weighted_means <- group_by(exp_period, p_label, load_category) %>%
    summarize(n = n(), sum_load = sum(load.kW), sum_compressor_power = sum(Refr.compressor.power..kW.)) %>%
    mutate(w_mean = sum_load/sum_compressor_power) %>%
    filter(p_label != "t1" & p_label != "t2" & p_label != "t3" & load_category != "No Load")

ggplot(weighted_means, aes(x=load_category, y=w_mean, fill=p_label)) +
    geom_bar(stat="identity",position="dodge") + 
    scale_fill_hue(c=45, l=80)

## now, assuming a load distribution like 2012's, let's estimate an average COP for each target pressure setting
## first, what's the load distribution like for 2012?
year_long_load <- sum(control_series$load.kW)
control_load_fractions <- group_by(control_series, load_category) %>%
    summarize(number = n(), load_fraction = sum(load.kW)/year_long_load)

## now let's assume those load fractions will be similar for 2013 and calculate expected COPs for each load category - pressure combination:
year_long_predicted_COPs <- merge(weighted_means, control_load_fractions, by="load_category") %>%
    mutate(COP_contribution = load_fraction * w_mean) %>%
    group_by(p_label) %>%
    summarize(expected_COP = sum(COP_contribution))

## plot the expected year long COPs
ggplot(year_long_predicted_COPs, aes(x=p_label, y=expected_COP, fill=p_label)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_hue(c=45, l=80) +
    coord_cartesian(ylim = c(4.5,5.5))

## can we say anything about the condenser fans running more at lower target pressures?
## let's take just the "high" load times from each experimental group and see if there's any real difference
## in how much power is being used to see if we can detect extra use of the condenser fans

## first, get a "non-compressor" power consumption column
exp_period$other_power <- (exp_period$Utility.power..kW. - exp_period$Refr.compressor.power..kW.)

## now, we should see an increase in average "non-compressor" power at lower compressor target pressures
non_compressor_power_means <- group_by(exp_period, p_label, load_category) %>%
    summarize(cnpm = mean(other_power)) %>%
    filter(p_label != "t1" & p_label != "t2" & p_label != "t3" & load_category != "No Load")

ggplot(non_compressor_power_means, aes(x=load_category, y=cnpm, fill=p_label)) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_hue(c=45, l=80) 

## can we say anything about how the summer months will limit the savings of a lower target pressure
## because the fans can't lower the temp/pressure of the refrigerant enough anyway?
## not really -- there's too much other stuff in the "other" category to say much

## what's the bottom line on savings?
## here's the best case scenario:



## energy projected for 365 days of 2012
correction_factor <- (365*24*4)/nrow(control_series)

total_year_heat_pumped <- sum(control_series$load.kW) * .25 * correction_factor
projected_savings <- mutate(year_long_predicted_COPs, annual_predicted_energy = total_year_heat_pumped / expected_COP,  Ecost_per_year = annual_predicted_energy * .15)

## simple bar graph of the savings
ggplot(projected_savings, aes(x=p_label, y=Ecost_per_year, fill=p_label)) + 
    geom_bar(stat="identity") +
    coord_cartesian(ylim=c(200000, 250000))
