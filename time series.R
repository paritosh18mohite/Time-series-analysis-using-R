library(eurostat) # package used for querrying EUROSTAT
library(dplyr) # package used for data-manipulation
library(ggplot2) # package used for plotting
library(fpp2) # package used for forecasting
library(forecast)
# set working directory, if needed
# setwd("C:/Users/Linnart/Desktop")

# clear old variables from memory
rm(list=ls())

# search the EUROSTAT database for the desired dataset 
#search_df <- search_eurostat("retail")
#View(search_df)

# download the data for which to forecast; from EUROSTAT  
# filter German values only
data_df <- get_eurostat("sts_trtu_q",
                        stringsAsFactors = FALSE) %>% 
  filter(geo=="DE")

# filter EUROSTAT for real values, without inflation
data_df <- filter(data_df,
                  indic_bt == "TOVV")

# filter EUROSTAT data for retail sale of beverages, food and tobacco
data_df <- filter(data_df,
                  nace_r2==unique(data_df$nace_r2)[30])

# sort historically by date
data_df <- arrange(data_df,
                   (time))

# filter EUROSTAT data to not be seasonally adjusted
data_df <- filter(data_df,
                  s_adj=="NSA")

# filter EUROSTAT data with 2010 = 100 index only
data_df <- filter(data_df,
                  unit=="I10")

View(data_df)
library(tidyverse)
df<- select(data_df,-c(1:5))
View(df)
class(df)

# plot EURO stat data, with a smoothed trend line
ggplot(df) + geom_line(mapping = aes(x=time,y=values),color="black") +
  geom_smooth(mapping=aes(x=time,y=values),color="green")+
  labs(title = "Time series analysis of German beverages, food and tobacco sales",
       x = "Time"+
       y= "values, 2015 = 100")

#df <- as.Date(df$time, format = "%y")
#View(df)
#declare the data as time series object ; this will ease forecasting
timeseries_df <- ts(df$values, start =c(1994,1),frequency = 4 ) #ts declares something as a time series

timeseries_df
plot(timeseries_df)
# take the first difference of the time series data
# this will eliminate the growth trend in the data
# this step is required by some forecasting models
differenced_df <- diff(timeseries_df)
plot(differenced_df)
autoplot(differenced_df) +
  ggtitle(" Retail sales of food, beverage and tobacco retail in Germany") +
  ylab(" retail sales index, 2010 = 100") +
  xlab("Time")
ndiffs(differenced_df)
## Lag plot of time series
gglagplot(timeseries_df)+
  labs(title = "Lag plot of quarterly Retail sales of food, beverage and tobacco retail in Germany")
# Autocorellation plot
ggAcf(timeseries_df)+
  ggtitle("Autocorellation function plot")

# visualize seasonality in the timeseries 
ggseasonplot(timeseries_df)+ggtitle("Seasonal plot")+ylab("Change in retail sales index, 2010 = 100")  

# seasonal subseries plot
ggsubseriesplot(timeseries_df)+ylab('values')+ggtitle('seasonal subseries plot: Retail sale of food, beverages and tobacco ')
monthplot(timeseries_df)

# exponential smoothing forecasting model
fit_ets <- ets(timeseries_df) # model calibration
checkresiduals(fit_ets) # visualize model performance

# smoothing time series models
ggtsdisplay(timeseries_df)

# simple moving average
plot(ma(timeseries_df,6))

# seasonal decomposition multiplicative
fit.decmult <- decompose(timeseries_df, type = 'multiplicative' )
fit.decmult$seasonal
autoplot(fit.decmult)+xlab("Year")+ggtitle('multiplicative decomposition
                                           of Retail sales of food, beverages and tobacco')
# seasonal decompoaition by loess smoothing stl()
fit <- stl(timeseries_df , s.window = 'periodic')
fit$time.series
plot(fit)
exp(fit$time.series)
# X11 decomposition
library(seasonal)
timeseries_df %>% seas(x11="") -> xfit
autoplot(xfit) +xlab('Year')
  ggtitle("X11 decomposition of German beverages, food and tobacco sales ")

  
autoplot(timeseries_df, series="Data") +
  autolayer(trendcycle(xfit), series="Trend") +
  autolayer(seasadj(xfit), series="Seasonally Adjusted") +
  xlab("time") + ylab("index 2010=100") +
  ggtitle(" German beverages, food and tobacco sales") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

monthplot(timeseries_df)
library(fpp2)

#Comparing with 'Basic' models
#rwf
fcast.rwf<- rwf(timeseries_df,h=8,drift = 'TRUE')
summary(fcast.rwf)
plot(forecast(fcast.rwf))

#mean model
fcast.mean<-meanf(timeseries_df,h=8)
summary(fcast.mean)
plot(forecast(fcast.mean))

#naive model
fcast.naive<-naive(timeseries_df,h=8)
summary(fcast.naive)
plot(forecast(fcast.naive))

#seasonal naive model
fcast.seasonalnaive<-snaive(timeseries_df,h=8)
summary(fcast.seasonalnaive)
timeseries_df
plot(fcast.seasonalnaive)
beer2 <- window(timeseries_df,start=c(1993,1),end=c(2015,4),frequency=4)
# Plot some forecasts
autoplot(timeseries_df) +
  autolayer(meanf(timeseries_df, h=8),
            series="Mean", PI=FALSE) +
  autolayer(naive(timeseries_df, h=8),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(timeseries_df, h=8),
            series="Seasonal naïve", PI=FALSE)+
  autolayer(rwf(timeseries_df=TRUE,h=8),
            series="Random walk drift method", PI=FALSE)
beer3 <- window(timeseries_df, start=c(1995,1),end=c(2015,4),frequency = 4)
accuracy(fcast.mean)
accuracy(fcast.naive)
accuracy(fcast.rwf)
accuracy(fcast.seasonalnaive)
#############################################################

#simple exponential smoothing model
mfit <- ses(timeseries_df, seasonal='multiplicative',h=12 )
summary(mfit)
round(accuracy(mfit),2)
autoplot(mfit)+autolayer(fitted(mfit),series = 'Fitted')+xlab('Year')+ylab('Values')

autoplot(timeseries_df, series="Data") +
  autolayer(ma(timeseries_df,5), series="12-MA") +
  xlab("Time") + ylab("timeseries_df") +
  ggtitle("German beverages, food and tobacco sales") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
                      breaks=c("Data","12-MA"))


# holt winter model
wsales <- window(timeseries_df, start = c(1995,1))
hfit1 <- hw(timeseries_df, seasonal = 'additive')
hfit2 <- hw(timeseries_df, seasonal = 'multiplicative')
summary(hfit2)
autoplot(timeseries_df)+autolayer(hfit1, series = 'HW additive forecast', PI=FALSE)+
                autolayer(hfit2, series = 'HW multiplicative forecast', PI=FALSE)+
                xlab('Time')+ylab('index 2010=100')+ggtitle('Holt winter model')+
                guides(colour=guide_legend(title = 'forecast'))
Ts_Train = window(timeseries_df, start = c(1994,1), end = c(2015,4), frequency = 4)
Ts_Train
Ts_Test = window(timeseries_df, start = c(2016,1),  frequency = 4)
Ts_Test
train_hw<-hw(Ts_Train,seasonal = 'additive')
plot(train_hw)
train_hw
fcast.hw<-forecast(train_hw,h=8)

autoplot(fcast.hw)+xlab('Year')+ylab('Values')+ggtitle('forecast holt winter retail sales of food,beverages and tobacco ')

vec<- cbind(Ts_Test,as.data.frame(fcast.hw)[,1])
ts.plot(vec,col=c('blue','red'),xlab('Year')+ylab('Values'), main='retail sales of food.beverages and tobacco actual vs forecast')

####
# automated model
afit <- ets(timeseries_df, model = 'ZZZ')
summary(afit)
plot(afit)
#################

#arima model

#Check the order of differencing required
ndiffs(timeseries_df)
#Plot the differenced Time Series
sdiff <- diff(timeseries_df)
plot(sdiff)

#Assess stationarity of the differenced series
library(tseries)
adf.test(sdiff)


S#ACF/PACF plots. Choosing p and q
library(astsa)

acf2(timeseries_df)
acf(timeseries_df)
pacf(timeseries_df)
#Fitting an ARIMA model
arfit <- arima(timeseries_df, order=c(3,1,2))
arfit

#Evaluating Model Fit
qqnorm(arfit$residuals)
qqline(arfit$residuals)
Box.test(arfit$residuals, type="Ljung-Box")
checkresiduals(arfit)
accuracy(arfit)
ggtsdisplay(timeseries_df)
plot(forecast(arfit,h=8))
# auto arima model

autofit <- auto.arima(timeseries_df)
autofit
acf2(timeseries_df)

qqnorm(autofit$residuals)
qqline(autofit$residuals)

Box.test(autofit$residuals, type="Ljung-Box")
checkresiduals(autofit)
accuracy(autofit)
forecast(autofit, h=8)


# train test
Ts_Train = window(timeseries_df, start = c(1994,1), end = c(2015,4), frequency = 4)
Ts_Train
Ts_Test = window(timeseries_df, start = c(2016,1),  frequency = 4)
Ts_Test
autoplot(Ts_Train, series="Train")+autolayer(Ts_Test,series = 'Test')+xlab('Year')+ylab('Sales')+ggtitle(" German beverages, food and tobacco sales")

#random walk with drift
decompose_train<-stl(Ts_Train,s.window = 'p')
decompose_train
train_stl<- forecast(decompose_train,method = 'rwdrift',h=12)
train_stl
plot(train_stl)
accuracy(Ts_Test)
#accuracy measure : rmse and mape rwd
log_test<- log10(Ts_Test)
Vec2<- 10^(cbind(log10(Ts_Test)),as.data.frame(forecast(decompose_train,method = 'rwdrift',h=12))[,1]))
ts.plot(Vec2, col=c('blue','red'),main='retails sales of food, beverages and tobacco actual vs forecast')
vec2<-10^(cbind(log10(Ts_Test)),as.data.frame(forecast(decompose_train))
          
          