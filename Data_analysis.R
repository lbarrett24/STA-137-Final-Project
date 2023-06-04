setwd('/Users/aravindkotikelapudi/Desktop/School_Work/Spring_2023/Sta 137/Project')

#We are installing and/or loading the necessary graphing packages
install.packages('ggplot2')
library(ggplot2)
library(gridExtra)

aal_table <- read.csv('AAL.csv')
t <- 1:nrow(aal_table)
aal_low <- aal_table$Low
aal <- ts(aal_low)

#Plotting the time series
t <- 1:nrow(aal_table)
plot.ts(aal, xlab="Time since 2018",main='Low' )




#Smooth components: Trend

#Moving average filter
ma2 = filter(aal, sides=2, rep(1,5)/5)
ma10 = filter(aal, sides=2, rep(1,21)/21)
ma35 = filter(aal, sides=2, rep(1,71)/71)
plot(t, ma2, xlab="", ylab="",type="l", col='blue', main='Moving Average Filter')
lines(t,ma10, col='red'); lines(t,ma35, col='green')
#legend(1, 95, legend=c("Filter size: 2", "Filter size: 10", "Filter size: 35"),
       #col=c("blue", "red", "green"), lty=1:2, cex=0.8)

trend_moving_average = ma10[c(3:249)]

plot.ts(aal[c(3:249)]-trend_moving_average, xlab="Time since 2018",main='Low' )

#Difference operator method
d1 = diff(aal)
d2 = diff(d1)
par(mfrow=c(1,2))
plot.ts(d1, xlab="", ylab="")
plot.ts(d2, xlab="", ylab="")




#Removing Seasonality

#Applying a moving average filter with size q=d/2 (d is roughly 50), N=5
ma25 = filter(aal, sides=2, rep(1,51)/51)
plot(t, ma25, xlab="", ylab="",type="l", col='blue', main='Moving Average Filter for Seasonality')

A = matrix(aal_low, ncol=50, byrow="TRUE")
M = matrix(ma25, ncol=50, byrow="TRUE")
mu = array(0,50)
for (k in 1:25) mu[k] = sum(A[2:5,k]-M[2:5,k])/4
for (k in 26:50) mu[k] = sum(A[1:4,k]-M[1:4,k])/4
hats = rep(mu-mean(mu),5)

deseasonalized <- aal_low - hats
deseasonalized_ts <- ts(deseasonalized)
plot.ts(deseasonalized, xlab="",main='Deseasonalized Data' )




#Deseasonalized and detrended
stationary <- deseasonalized- as.numeric(ma25)
stationary <- stationary[26:226]  #Edge values are cut off
stationary_ts <- ts(stationary)
plot.ts(stationary_ts, xlab="",main='Stationary Data' )
  


  
#Analyzing residuals

#Sample ACF
acf(stationary, lag=50, xlab="", ylab="", main="Sample ACF Method")

#Portmanteau Test
Box.test(stationary, lag=20, type="Ljung")

#QQ Plot
qq <- qqnorm(stationary)
R2 <- sum((qq$y-mean(qq$y))*qq$x)^2/(sum((qq$y-mean(qq$y))^2)*sum(qq$x^2))
R2




#ARMA Models
acf(stationary_ts, lag=250)
pacf(stationary_ts, lag=250)







