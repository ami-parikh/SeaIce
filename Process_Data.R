#convert daily data into weekly data
#assume weekly averaging
library(lubridate)
library(dbplyr)
library(tidyverse)
seaIceclean <- read.csv("~/SeaIce/seaIceclean.csv")
weekly_seaice <- seaIceclean %>%
  mutate(year = year(date))%>%
  #handle week of year 53 as 52 (counts by full week)
  mutate(week_of_year = if_else(week(date)>52,52,week(date))) %>%
  group_by(year,week_of_year) %>%
  filter(year > 1978 & year < 2019)%>%
  select(year,week_of_year,Extent) %>%
  summarise(Extent = mean(Extent))

#missing data from the end of 1987 to beginning 1988

gen_years <- function(start_year,end_year){
  n = end_year-start_year
  output = c()
  while(start_year<=end_year){
    repped = rep(start_year,52)
    output = c(output,repped)
    start_year = start_year + 1
  }
  return(output)
}

summary(weekly_seaice)
df <- tibble(
  year=gen_years(1979,2018),
  week_of_year=rep(1:52,2018-1978),
  ExtentNA = -1
)
df
joined <- left_join(df,weekly_seaice,by=c('year','week_of_year'))
joined %>%
  filter(is.na(Extent))
complete_weekly_seaice <- joined %>%
  select(year,week_of_year,Extent)
complete_weekly_seaice
complete_ts <- ts(complete_weekly_seaice[complete_weekly_seaice$year>1988,3],start=c(1989,1),frequency=52)
###
saveRDS(complete_ts,'SeaIceCompleteTS.rds')