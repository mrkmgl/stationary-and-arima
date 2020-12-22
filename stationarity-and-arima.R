library(psych)
library(lmtest)
library(quantreg)
library(tseries)
library(forecast)
library(xts)
library(urca)

#STATIONARY SERIES
SOI=read.csv("/Users/marco/Desktop/Education/Skema/Semester 1/Econometrics/Individual Project/SOI.csv")
View(SOI)
plot(SOI, type = "l") #visualize the data
adf.test(SOI$Value) #No trend
acf(SOI$Value) #just for personal interest

DWJ=read.csv("/Users/marco/Desktop/Education/Skema/Semester 1/Econometrics/Individual Project/DWJ.csv")
View(DWJ)
plot(DWJ, type = "l") #visualize the data
adf.test(DWJ$returns) #No trend
acf(DWJ$returns) #just for personal interest

#NONSTATIONARY SERIES
temp=read.csv("/Users/marco/Desktop/Education/Skema/Semester 1/Econometrics/Individual Project/global_temp.csv")
View(temp)
temp_ts = ts(data = temp[-1], start = 1880, end = 2016)
plot(temp_ts, type = "l")
abline(reg=lm(temp_ts~time(temp_ts))) #insert a trend line
adf.test(temp_ts) #non-stationary
acf(temp_ts) #just for personal interest

CO2_emission=read.csv("/Users/marco/Desktop/Education/Skema/Semester 1/Econometrics/Individual Project/CO2_emissions.csv")
View(CO2_emission)
co2_ts = ts(data = CO2_emission[-1], start = 1880, end = 2016)
plot(co2_ts, type = "l")
abline(reg=lm(co2_ts~time(co2_ts))) #insert a trend line
adf.test(co2_ts) #non-stationary
acf(co2_ts) #just for personal interest

#LINEAR REGRESSION
SOI_annual=read.csv("/Users/marco/Desktop/Education/Skema/Semester 1/Econometrics/Individual Project/SOI_annual.csv")
SOI_ts = ts(data = SOI_annual[-1], start = 1956, end = 2016)
co2_ts2 = ts(data = CO2_emission[-1], start = 1956, end = 2016)
temp_ts2 = ts(data = temp[-1], start = 1956, end = 2016)

regr = lm(temp_ts~co2_ts) #chosen regression
regr1 = lm(temp_ts2~co2_ts2+SOI_ts) #not significant
regr2 = lm(temp_ts2~co2_ts2) #not significant
regr3 = lm(temp_ts2~SOI_ts) #not significant

summary(regr)
summary(regr1)
summary(regr2)
summary(regr3)

bptest(regr) #homoskedasticity

par(mfrow = c(2, 2))
plot(regr)
plot(regr$residuals)
adf.test(regr$residuals)

par(mfrow = c(3, 1))
plot(temp_ts2)
plot(co2_ts2)
plot(SOI_ts)

jotest=ca.jo(data.frame(temp_ts,co2_ts), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest) #cointegration test

#ESTIMATING THE MODEL
par(mfrow=c(1,1))
plot(aggregate(temp_ts, FUN = "mean"))
boxplot(temp_ts~cycle(temp_ts)) #no cycle
plot(diff(temp_ts))
adf.test(diff(temp_ts)) #stationary
acf(diff(temp_ts)) #to guess the ARIMA 
pacf(diff(temp_ts)) #to guess the ARIMA

ARIMAtemp = auto.arima(temp_ts) #ARIMA(1,1,3)
summary(ARIMAtemp)
ARIMAtempforecast = forecast(ARIMAtemp,h=1) #forecasitng the next year
plot(ARIMAtempforecast) 
