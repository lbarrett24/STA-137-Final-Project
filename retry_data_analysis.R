setwd('/Users/aravindkotikelapudi/Desktop/School_Work/Spring_2023/STA_137/Project')
setwd('STA-137-Final-Project')

aal_table <- read.csv('AAL.csv')
t <- 1:nrow(aal_table)
aal_low <- aal_table$Low
aal <- ts(aal_low)

#Plotting the time series
t <- 1:nrow(aal_table)
plot.ts(aal, xlab="Time since 2018",main='Low' )






#Smooth components: Trend and Seasonality


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

ma10 = filter(deseasonalized, sides=2, rep(1,21)/21)
plot(t, ma10, xlab="", ylab="",type="l", main='Moving Average Filter for Deseasonalized: q=10')

stationary <- deseasonalized- as.numeric(ma10)
stationary <- stationary[26:226]  #Edge values are cut off
stationary_ts <- ts(stationary)
plot.ts(stationary_ts, xlab="",main='Stationary Data: ma10 trend' )






#Analyzing residuals

#Sample ACF
acf(stationary, lag=50, xlab="", ylab="", main="Sample ACF Method: 2nd Try")

#Portmanteau Test
Box.test(stationary, lag=20, type="Ljung")

#QQ Plot
qq <- qqnorm(stationary)
R2 <- sum((qq$y-mean(qq$y))*qq$x)^2/(sum((qq$y-mean(qq$y))^2)*sum(qq$x^2))
R2






#ARMA Models
mean(stationary_ts) #about zero

acf(stationary_ts, lag=250, main='ACF of Residuals: 2nd Try')
pacf(stationary_ts, lag=400, main='PACF of Residuals: 2nd Try')

#AR(1) process
fit.aal_1 = ar.ols(stationary_ts, aic=F, order.max=1, demean=F, intercept=T)
fit.aal_1
fit.aal_1$asy.se


#AR(2) process
fit.aal_2 = ar.ols(stationary_ts, aic=F, order.max=2, demean=F, intercept=T)
fit.aal_2
fit.aal_2$asy.se


#Testing out both processes
#AR(1)
ar1.acf = ARMAacf(ar=c(0.8216), ma=0, 50)
ar1.pacf = ARMAacf(ar=c(0.8216), ma=0, 50, pacf=T)
ar1 = arima.sim(list(order=c(1,0,0), ar=0.8216), n=50)
plot.ts(ar1, main='Simulated AR(1) process: 2nd Try' )
plot.ts(ts(ar1.acf), main='Simulated AR(1) acf: 2nd Try' )
plot.ts(ts(ar1.pacf), main='Simulated AR(1) pacf: 2nd Try' )
#AR(2)
ar2.acf = ARMAacf(ar=c(1.0567, -0.2872), ma=0, 50)
ar2.pacf = ARMAacf(ar=c(1.0567, -0.2872), ma=0, 50, pacf=T)
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.0567, -0.2872)), n=50)
plot.ts(ar2, main='Simulated AR(2) process: 2nd Try' )
plot(ts(ar2.acf), main='Simulated AR(2) acf: 2nd Try' )
plot(ts(ar2.pacf), main='Simulated AR(2) pacf: 2nd Try')

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
ar1 = arima.sim(list(order=c(1,0,0), ar=0.8216), n=201)
final_residuals_ar1 <- stationary_ts - ar1
plot.ts(final_residuals_ar1, main='Final Residuals with AR(1) Process Removed: 2nd Try' )
acf(final_residuals_ar1, lag=250, main='ACF of Final Residuals from AR(1): 2nd Try')
pacf(final_residuals_ar1, lag=250, main='PACF of Final Residuals from AR(1): 2nd Try')
#AR(2)
ar2 = arima.sim(list(order=c(2,0,0), ar=c(1.0567, -0.2872)), n=201)
final_residuals_ar2 <- stationary_ts - ar2
plot.ts(final_residuals_ar2, main='Final Residuals with AR(2) Process Removed: 2nd Try' )
acf(final_residuals_ar2, lag=250, main='ACF of Final Residuals from AR(2): 2nd Try')
pacf(final_residuals_ar2, lag=250, main='PACF of Final Residuals from AR(2): 2nd Try')

