library(tidyverse)
library(TTR)

stress = read.csv("time_analysis_stress_df.csv")
stress$Time = as.POSIXct(stress$Time)
glimpse(stress)

BL.SV = read.csv("average_BL.SV_value_over_time.csv")
# filter out the time data
SV = stress %>%
  filter(Measurement == "SV")
SVtime = SV[3]

# make a time series analysis
SV.timeseries = ts(SVtime, start = 1, end = 16, frequency = 70)
SV.timeseries
# plot the time series

plot(SV.timeseries, xlab = "Time(Minutes)", ylab = "Reading")

SV.timeseries.SMA = SMA(SV.timeseries, n = 100)

plot(SV.timeseries.SMA, xlab = "Time(Minutes)", ylab = "Reading")


# try some ggplot options


ggplot(SV, aes(x = Time, y = Value, color = factor(Yoga.Practice..1.never..2.sometimes..3.regularly))) + 
  geom_smooth(se = FALSE) + 
  geom_smooth(aes(y = BL.SV$Avg.Value[1]), color = "Red", alpha = 0.5) + 
  geom_smooth(aes(y = BL.SV$Avg.Value[2]), color = "Blue", alpha = 0.5) + 
  geom_smooth(aes(y = BL.SV$Avg.Value[3]), color = "Green", alpha = 0.5)




BL.HR = read.csv("average_BL.HR_value_over_time.csv")

HR = stress %>%
  filter(Measurement == "HR")
ggplot(HR, aes(x = Time, y = Value, color = factor(Yoga.Practice..1.never..2.sometimes..3.regularly))) + 
  geom_smooth(se = FALSE) + 
  geom_smooth(aes(y = BL.HR$Avg.Value[1]), color = "Red", alpha = 0.5) + 
  geom_smooth(aes(y = BL.HR$Avg.Value[2]), color = "Blue", alpha = 0.5) + 
  geom_smooth(aes(y = BL.HR$Avg.Value[3]), color = "Green", alpha = 0.5)


mean(HR$Value)
how.many = SV %>%
  filter(Time == "2019-04-21 17:15:06 MDT")
