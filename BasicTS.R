###Time Series work###
complete_ts <- readRDS('SeaIceCompleteTS.rds')
train <- window(complete_ts,start = c(1989,1),end = c(2016,52))
test <- window(complete_ts,start=c(2017,1))

plot(train)
train_lambda <- BoxCox.lambda(train)
#train lambda = 1.999924, not worth it to make the change
train_lambda
autoplot(BoxCox(train,train_lambda))
train_diff1_12 <- diff(train,lag = 52)
plot(train_diff1_12)


plot(test)

library(forecast)
m1 <- auto.arima(train,D=1,seasonal = T)
summary(m1)