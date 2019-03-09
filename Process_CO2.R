library(lubridate)
library(dbplyr)
library(tidyverse)
folder <- "C:/Users/Bob/Desktop/Time_Series/Final"
fname <- "co2_weekly_mlo.txt"
df_co2 <- read.table(file=paste(folder,fname,sep="/"),sep="",header = F)
head(df_co2)
df_co2=df_co2[,1:5]
cnames = c('Year','Month','Day','Year_Fraction','CO2ppm')
colnames(df_co2) <-cnames
head(df_co2)
df_co2_t <- as_tibble(df_co2)
head(df_co2_t)
df_co2_week <- df_co2_t %>%
  #build a date
  mutate(Date = lubridate::ymd(paste(Year,Month,Day,sep = "-"))) %>%
  mutate(WeekOfYear = if_else(week(Date)>52,52,week(Date))) %>%
  filter(Year < 2019)%>%
  group_by(Year,WeekOfYear)%>%
  select(Year,WeekOfYear,CO2ppm) %>%
  #turn extent into the mean of the weekly data
  summarise(CO2ppm = mean(CO2ppm))
glimpse(df_co2_week)
df_co2_week %>%
  filter(Year==2008 & WeekOfYear>20 & WeekOfYear<35) %>%
  view()
#replace negatives with NaNs
df_co2_week <- df_co2_week %>%
  mutate_at(c("CO2ppm"),~if_else(.x<0,NaN,.x))
library(forecast)
clean_co2 <- na.interp(ts(df_co2_week$CO2ppm,start = c(1989,1),frequency = 52))
saveRDS(clean_co2,"clean_co2.rds")
window(clean_co2,c(2008,26),c(2008,28))
window(clean_co2,c(2005,42),c(2005,42))
