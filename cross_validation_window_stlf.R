##################
#cross validation#
##################

#load the sea ice time series
complete_ts <- readRDS('SeaIceCompleteTS.rds')
#load the CO2 time series
library(forecast)
library(stats)
library(ggplot2)

n <- length(complete_ts) # Number of data points
n
p <- 52 ### Period
H <- 104 # Forecast Horiz
n-10*p-104
file_name = 'year_MAPE_matrix_baseline.rds'

for(j in 3:20){
  k <- j*p
  st <- tsp(complete_ts)[1]+(k-2)/p
  #matrix to store mape's as we find them
  mape_err <- matrix(NA,n-k-H,1)
  
  for(i in 1:(n-k-H)){
    print(paste('iteration',i))
    #define test window
    #   for sea ice data
    test <- window(complete_ts, start=st + (i+1)/p, end=st + (i+H)/p)
      #   for sea ice data
    train <- window(complete_ts,start = st + (i-k+1)/p, end=st + i/p)
    #print(length(train))
    fit_hw <- stlf(train,etsmodel="AAN",damped=FALSE)
    fcast_hw_err <- forecast(fit_hw,h=H)
    res = fcast_hw_err$mean - test
    AE = abs(res)
    MAPE = 100* sum(AE/test)/length(test)
    print(MAPE)
    mape_err[i,1]=MAPE
  }
  saveRDS(mape_err,file = paste(j,file_name,sep="_"))
}
n-3*p-104
25-11
M <- matrix(0,1500,18)
M
for(i in 3:20){
  file_name = paste0(i,'_year_MAPE_matrix_baseline.rds')
  MAPE <- readRDS(file_name)
  missing <- 1500 - length(MAPE)
  MAPE <- c(as.vector(MAPE),rep(NA,abs(missing)))
  M[,i-2] <- MAPE
}

M


df <- data.frame(M)
names <- paste0(seq(3,20),'_Year_Window')
x <- seq(1,1500)
df['x']<- x
head(df)
colnames(df) <- c(names,'x_axis')
head(df)
dfreduced = df[,-c(1:10)]
head(dfreduced)
df_melted <- reshape2::melt(dfreduced, id.var='x_axis')
head(df_melted)
ggplot(df_melted,aes(x=x_axis,y=value))+
  facet_grid(rows = vars(variable))+
  ylim(c(0,100))+
  geom_line()+
  xlab("Iteration")+
  ylab("MAPE")
