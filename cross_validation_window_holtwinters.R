##################
#cross validation#
##################

#load the sea ice time series
complete_ts <- readRDS('SeaIceCompleteTS.rds')
#load the CO2 time series
co2_ts <- readRDS('clean_co2.rds')

library(stats)
#lag the CO2 ts
co2_ts_34 <- lag(co2_ts,k=34)
length(complete_ts)
length(co2_ts_34)
#correct the lagging
co2_ts_34_corrected <- window(co2_ts_34,start=c(1989,1))
#take the sea ice data and drop from c(2018,19) onwards to align the data
complete_ts_cut <- window(complete_ts,end = c(2018,18))

tail(co2_ts_34_corrected)
tail(complete_ts_cut)


n <- length(complete_ts_cut) # Number of data points

p <- 52 ### Period
H <- 104 # Forecast Horiz

file_name = 'year_MAPE_matrix.rds'

for(j in 2:10){
  k <- j*p
  st <- tsp(complete_ts)[1]+(k-2)/p
  #matrix to store mape's as we find them
  mape_err <- matrix(NA,n-k,1)
  
  for(i in 1:(n-k)){
    #define test window
    #   for sea ice data
    test <- window(complete_ts_cut, start=st + (i+1)/p, end=st + (i+H)/p)
    #   for co2 data
    xreg_test <- window(co2_ts_34_corrected, start=st + (i+1)/p, end=st + (i+H)/p)
    #define train window
    #   for sea ice data
    train <- window(complete_ts_cut,start = st + (i-k+1)/p, end=st + i/p)
    #   for CO2 data
    xreg_train <- window(co2_ts_34_corrected,start = st + (i-k+1)/p, end=st + i/p)
    #ARIMA(2,1,0)(1,1,0)[52]
    fit_arima_err <- Arima(train,order=c(2,1,0),
                           seasonal = list(order=c(1,1,0),period=p),
                           xreg = xreg_train,
                           include.drift=TRUE, 
                           method="ML")
    
    fcast_arima_err <- forecast(fit_arima_err,xreg=xreg_test,h=H)
    res = fcast_arima_err$mean - test
    AE = abs(res,na.rm=T)
    MAPE = 100* sum(AE/test)/length(test)
    mape_err[i,1]=MAPE
  }
  saveRDS(mape_err,file = paste(j,file_name,sep="_"))
}


year3 =readRDS("3_year_MAPE_matrix.rds")
year4 =readRDS("4_year_MAPE_matrix.rds")
year5 =readRDS("5_year_MAPE_matrix.rds")
year6 =readRDS("6_year_MAPE_matrix.rds")
year7 =readRDS("7_year_MAPE_matrix.rds")
year8 =readRDS("8_year_MAPE_matrix.rds")
year9 =readRDS("9_year_MAPE_matrix.rds")
year10 =readRDS("10_year_MAPE_matrix.rds")


par(mfrow=c(1,1))


plot(1:length(year3[,1]), year3[,1], type="l",col=1,xlab="horizon", ylab="MAPE")
plot(1:length(year4[,1]), year4[,1], type="l",col=1,xlab="horizon", ylab="MAPE")
plot(1:length(year5[,1]), year5[,1], type="l",col=1,xlab="horizon", ylab="MAPE")
plot(1:length(year6[,1]), year6[,1], type="l",col=1,xlab="horizon", ylab="MAPE")
plot(1:length(year7[,1]), year7[,1], type="l",col=1,xlab="horizon", ylab="MAPE")
plot(1:length(year8[,1]), year8[,1], type="l",col=1,xlab="horizon", ylab="MAPE")
plot(1:length(year9[,1]), year9[,1], type="l",col=1,xlab="horizon", ylab="MAPE")
plot(1:length(year10[,1]), year10[,1], type="l",col=1,xlab="horizon", ylab="MAPE")

