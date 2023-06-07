# Load required libraries
library(readr)
library(forecast)
library(tseries)
library(ggplot2)
library(tidyverse)
library(dplyr)

# read in data
data = read_csv("AAL.csv")

# understanding data
str(data)
head(data)
summary(data)
sum(is.na(data))

# Data Preprocessing
# Convert the date column to proper date format
data$Date = as.Date(data$Date)

#grouping data by month
month_data = data %>% group_by(month=lubridate::floor_date(Date, 'month'))

#boxplot of closing price by month
plot1 = month_data %>% ggplot(aes(x=month_data$month, y=month_data$Close, group=month)) + 
  geom_boxplot(color="dark green", 
               fill="light green", 
               alpha=0.2) + 
  ggtitle("Box Plot: Closing Price by Month") + 
  theme(plot.title=element_text(size=25),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15)) + 
  xlab("Month") +
  ylab("Closing Price (USD)") +
  scale_y_continuous(labels=unit_format(unit="K"))

plot1

# histogram of Closing period
hist(data$Close,
     xlab='Closing Volume (USD)',
     main='Histogram of American Airlines Closing Price (May 2022 - May 2023)', 
     col='dark green',
     density=25,
     angle=60,
     breaks=18,
     freq=F) #make density of histogram = 1 for density plot
abline(v = mean(data$Close), col='red', lwd = 3)
lines(density(data$Close), col='blue',lwd=3)

# Plotting ACF & PACF graphs
par(mfrow=c(1,2))
plot(acf(data$Close, plot=FALSE), main="Autocorrelation Plot (ACF)")
plot(pacf(data$Close, plot=FALSE), main="Partial Autocorrelation Plot (PACF)")

# Closing stock price over time
ggplot(data=data, aes(x = Date, y = Close)) + geom_line() +
  labs(title = 'American Airlines(AAL) Closing Stock Price Over Time',
       x = 'Date',
       y = 'Closing Stock Price')+
  geom_smooth()+
  geom_point() #make legend with line of best fit

#detrending with moving average filter
detrend_data = data %>% mutate(detrended = rollmean(Close, k=5, fill=NA, allign='center'))

#check for missing vals
sum(is.na(detrend_data$detrended))

#removing missing vals
detrend_data_clean = detrend_data[!is.na(detrend_data$detrended),] 

# Compute the periodogram
periodogram = spec.pgram(detrend_data_clean$detrended, 
                         taper = 0,
                         detrend = F, #because already detrended
                         log = "no",
                         main = "Spectral Analysis of Detrended Data")









