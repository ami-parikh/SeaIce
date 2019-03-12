library(forecast)
library(expsmooth)
library(tseries) 
library(fpp) 
library(ggplot2) 
library(forecast)
library(tidyverse)
library(tsibble)
library(TSA)


complete_ts <- readRDS('SeaIceCompleteTS.rds')
train <- window(complete_ts,c(1989,1),c(2016,52))
test <- window(complete_ts,c(2017,1))

autoplot(train)

#additive model for Holt Winters
mod_additive =HoltWinters(train, seasonal = "additive")

test_forecast_additive = forecast(mod,104)
test_forecast_additive

residuals_additive = test_forecast_additive$mean-test

autoplot(test_forecast_additive)

checkresiduals(mod_additive)

(mape = 100* sum(abs(test-test_forecast_additive$mean)/test)/104)


#multiplicative model for Holt Winters

mod_multiplicative = HoltWinters(train, seasonal = "multiplicative")

test_forecast_multiplicative = forecast(mod,104)
test_forecast_multiplicative

residuals_multiplicative = test_forecast_multiplicative$mean-test

autoplot(test_forecast_multiplicative)

checkresiduals(mod_multiplicative)

(mape = 100* sum(abs(test-test_forecast_multiplicative$mean)/test)/104)
