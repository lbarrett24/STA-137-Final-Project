setwd('/Users/aravindkotikelapudi/Desktop/School_Work/Spring_2023/STA_137/Project')
setwd('STA-137-Final-Project')


aal_table <- read.csv('AAL.csv')
t <- 1:nrow(aal_table)
aal_low <- aal_table$Low
aal <- ts(aal_low)

#Plotting the time series
t <- 1:nrow(aal_table)
plot.ts(aal, main='Low' )






#Smooth components: Trend

#Moving average filter-trying out different filters
ma2 = filter(aal, sides=2, rep(1,5)/5)
ma10 = filter(aal, sides=2, rep(1,21)/21)
ma35 = filter(aal, sides=2, rep(1,71)/71)
plot(t, ma2, xlab="", ylab="",type="l", col='blue', main='Moving Average Filter')
lines(t,ma10, col='red'); lines(t,ma35, col='green')
trend_moving_average = ma10[c(3:249)]
plot.ts(aal[c(3:249)]-trend_moving_average, main='Low' )

#Difference operator method
d1 = diff(aal)
d2 = diff(d1)
par(mfrow=c(1,2))
plot.ts(d1, xlab="", ylab="")
plot.ts(d2, xlab="", ylab="")






#Removing Seasonality

#Applying a moving average filter with size q=d/2 (d is roughly 50), N=5
ma25 = filter(aal, sides=2, rep(1,51)/51)
plot(t, ma25, xlab="", ylab="",type="l", main='Moving Average Filter for Seasonality: q=25')

A = matrix(aal_low, ncol=50, byrow="TRUE")
M = matrix(ma25, ncol=50, byrow="TRUE")
mu = array(0,50)
for (k in 1:25) mu[k] = sum(A[2:5,k]-M[2:5,k])/4
for (k in 26:50) mu[k] = sum(A[1:4,k]-M[1:4,k])/4
hats = rep(mu-mean(mu),5)

plot.ts(ts(hats), xlab="",main='Seasonality Estimate: q=25' )

deseasonalized <- aal_low - hats
deseasonalized_ts <- ts(deseasonalized)
plot.ts(deseasonalized, xlab="",main='Deseasonalized Data: q=25' )






#Deseasonalized and detrended
stationary <- deseasonalized- as.numeric(ma25)
stationary <- stationary[26:226]  #Edge values are cut off
stationary_ts <- ts(stationary)
plot.ts(stationary_ts, xlab="",main='Stationary Data: q=25' )
  




  
#Analyzing residuals

#Sample ACF
acf(stationary, lag=50, xlab="", ylab="", main="Sample ACF Method: q=25")

#Portmanteau Test
Box.test(stationary, lag=20, type="Ljung")

#QQ Plot
qq <- qqnorm(stationary)
R2 <- sum((qq$y-mean(qq$y))*qq$x)^2/(sum((qq$y-mean(qq$y))^2)*sum(qq$x^2))
R2






#ARMA Models
mean(stationary_ts) #about zero

acf(stationary_ts, lag=250, main='ACF of Process')
pacf(stationary_ts, lag=400, main='PACF of Process')

#AR(1) process
fit.aal_1 = ar.ols(stationary_ts, aic=F, order.max=1, demean=F, intercept=T)
fit.aal_1
fit.aal_1$asy.se
#coefficients: 0.002463, 0.9151

#AR(2) process
fit.aal_2 = ar.ols(stationary_ts, aic=F, order.max=2, demean=F, intercept=T)
fit.aal_2
fit.aal_2$asy.se
#coefficients: -0.0003499, 1.1503, -0.2502

#Testing out both processes
#AR(1)
ar1.acf = ARMAacf(ar=c(0.9151), ma=0, 50)
ar1.pacf = ARMAacf(ar=c(0.9151), ma=0, 50, pacf=T)
ar1 = arima.sim(list(order=c(1,0,0), ar=0.9151), n=50)
plot.ts(ar1, main='Simulated AR(1) process: q=25' )
plot.ts(ts(ar1.acf), main='Simulated AR(1) acf: q=25' )
plot.ts(ts(ar1.pacf), main='Simulated AR(1) pacf: q=25' )
#AR(2)
ar2.acf = ARMAacf(ar=c(1.1503, -0.2502), ma=0, 50)
ar2.pacf = ARMAacf(ar=c(1.1503, -0.2502), ma=0, 50, pacf=T)
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.1503, -0.2502)), n=50)
plot.ts(ar2, main='Simulated AR(2) process: q=25' )
plot(ts(ar2.acf), main='Simulated AR(2) acf: q=25' )
plot(ts(ar2.pacf), main='Simulated AR(2) pacf: q=25' )

#PROPERTIES OF ARMA PROCESSES
#ACF is 1 then slowly oscillates around 0.2, PACF is 1 then drops drastically to around 0
#AR(1) or AR(2) process: ACF tails off, and PACF cuts off after lag 1

#Looks like mean is moving
#Linear process: mean of 0 and ACVH is h dependent
#AR(1) process: mean of zero and ACF rh0(h) = psi^h (coefficients)
#Exponential decay of psi coefficients typical of ARMA processes

#AR(p): ACF tails off, PACF cuts off after lag p
#MA(q): ACF cuts off after lag q, PACF tails off
#ARMA(p,q): Both tail off

#Since PACF doesn't actually cut off, PACF isn't exactly AR(1) process


#Seeing if residuals conform to white noise
#AR(1)
ar1 = arima.sim(list(order=c(1,0,0), ar=0.9151), n=201)
final_residuals_ar1 <- stationary_ts - ar1
plot.ts(final_residuals_ar1, main='Final Residuals with AR(1) Process Removed: q=25' )
acf(final_residuals_ar1, lag=250, main='ACF of Final Residuals from AR(1): q=25')
pacf(final_residuals_ar1, lag=250, main='PACF of Final Residuals from AR(1): q=25')
#AR(2)
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.1503, -0.2502)), n=201)
final_residuals_ar2 <- stationary_ts - ar2
plot.ts(final_residuals_ar2, main='Final Residuals with AR(2) Process Removed: q=25' )
acf(final_residuals_ar2, lag=250, main='ACF of Final Residuals from AR(2): q=25')
pacf(final_residuals_ar2, lag=250, main='PACF of Final Residuals from AR(2): q=25')






#Forecasting

#Trend component
#Will fit a 3rd order polynomial to ma25 trend estimation:
t = 1:length(ma25)
t2 <- t^2
t3 <- t^3

thirdfit = lm(ma25 ~ (t + t2 + t3))
plot.ts(ma25, main="Ma25 trend estimate vs 3rd-order Fit")
lines(t, (1.735*10) - (1.233*10^(-1))*t + (1.09*10^(-3))*t^2 - (2.69*10^(-6))*t^3, col='blue')
thirdfit$coefficients

intercept <- 1.735*10
a1 <- -1.233*10^(-1)
a2 <- 1.09*10^(-3)
a3 <- -2.69*10^(-6)

cubic = function(a,b,c,d,x){
  a*x^3 + b*x^2 + c*x + d
}

t_forecast = 200:260

#Trend forecast
trend_forecast <- cubic(a3, a2, a1, intercept, t_forecast)
trend_forecast

#Seasonality forecast
#Clear section of seasonality that has same behavior as if starting at 200
seasonality_forecast <-hats[50:110] 

#Rough component forecast
#AR(1) process
rough_forecast_ar1 <-as.numeric((predict(fit.aal_1, n.ahead=61)$pred))

#AR(2) process
rough_forecast_ar2 <-as.numeric((predict(fit.aal_2, n.ahead=61)$pred))

#AR(1) total forecast
forecast_ar1 <- trend_forecast + seasonality_forecast + rough_forecast_ar1
forecast_ar1 <- ts(forecast_ar1, start = 200)
plot.ts(forecast_ar1, main = 'Forecast estimate for AR(1) process past 200: q=25')

#AR(2) total forecast
forecast_ar2 <- trend_forecast + seasonality_forecast + rough_forecast_ar2
forecast_ar2 <- ts(forecast_ar2, start = 200)
plot.ts(forecast_ar2, main = 'Forecast estimate for AR(2) process past 200: q=25')
