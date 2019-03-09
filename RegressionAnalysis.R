#regression of ice data using co2 data
complete_ts <- readRDS('SeaIceCompleteTS.rds')
co2_ts <- readRDS('clean_co2.rds')
train <- window(complete_ts,c(1989,1),c(2016,52))
test <- window(complete_ts,c(2017,1))
plot(co2_ts_test)
autoplot(test+400)+
  autolayer(co2_ts_test)
co2_ts_train <- window(co2_ts,c(1989,1),c(2016,52))
co2_ts_test <- window(co2_ts,c(2017,1))
library(forecast)
#add scaling for ice extent
scaled_train <- scale(train)
scaled_train_ts <- ts(scaled_train,start=c(1989,1),frequency = 12)
scaled_test <- scale(test, center= attr(scaled_train,"scaled:center"),
                           scale = attr(scaled_train,"scaled:scale"))
scaled_test_ts <- ts(scaled_test_ts,start=c(2017,1),frequency=12)

#add scaling for co2
scaled_co2<- scale(co2_ts_train)
scaled_co2_ts <- ts(scaled_co2,start=c(1989,1),frequency=12)
scaled_co2_test <- scale(co2_ts_test,center=attr(scaled_co2,"scaled:center"),
                                                 scale = attr(scaled_co2,"scaled:scale"))

scaled_co2_test_ts <- ts(scaled_co2_test,start=c(2017,1),frequency=12) 

#run w/ scaled data
fit_reg_scaled <- auto.arima(scaled_train,xreg=scaled_co2_ts,allowdrift = F)
summary(fit_reg_scaled)

#run correlation analysis to pick best lag
#

fit_reg <- auto.arima(train,xreg=co2_ts_train,allowdrift=F)
#fit <- auto.arima(train)
summary(fit_reg)
autoplot(fit_reg)
mean(co2_ts)
fcast <- forecast(fit_reg,xreg=c(rep(377.8236,52)),h=52)
autoplot(fcast)
#lag the data some and see if it's closer
lag(as.vector(co2_ts_test),2)
length(as.vector(train))
train_vec <- as.vector(train)
length(lag(as.vector(co2_ts_train),2))

cor(lag(as.vector(co2_ts_train),2),train_vec[c(2:length(train_vec))])
library(stats)
stats::lag.plot(train,lags=5)
