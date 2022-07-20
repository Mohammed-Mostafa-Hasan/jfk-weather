#import libray we need for this project 
library(tidymodels)
library(tidyverse)
library(gridExtra)
library(rlang)
library(tidyr)
library(magrittr)
library(ggplot2)
library(coefplot)
library(boot)
library(rsample)
library(useful)
library(glmnet)
library(parallel)
library(doParallel)
#import the data for analysis
jfk_data <-read.table("noaa-weather-sample-data/jfk_weather_sample.csv",header = TRUE,sep = ",")
class(jfk_data)
head(jfk_data)
#select some columns for our analysis
sub_col <- jfk_data %>%select(HOURLYRelativeHumidity,
                              HOURLYDRYBULBTEMPF,
                              HOURLYPrecip,
                              HOURLYWindSpeed,
                              HOURLYStationPressure)
#find out unique value for HOURLYPrecip columns 
unique(sub_col$HOURLYPrecip)
#make some cleaing for this column {first remove all NA values}
sub_col <- sub_col[!is.na(sub_col$HOURLYPrecip),]
sum(is.na(sub_col))
#after some exploration for na at other columns to remove all na we find four value in HOURLYStationPressure
sub_col <- sub_col[!is.na(sub_col$HOURLYStationPressure),]
sum(is.na(sub_col))
 #by cheking unique HOURLYPrecip
#Having characters in values (like the "T" and "s" that you see in the unique values) in HOURLYPrecip
#when you create a model will cause problems
unique(sub_col$HOURLYPrecip)
#removing these diagnostics from this column and update the column with mutate function 
sub_col %<>%mutate(HOURLYPrecip=str_remove(HOURLYPrecip,pattern = "s$"),
                   HOURLYPrecip = str_replace(HOURLYPrecip,pattern = "T","0.0"))

#cheking data type for each column
glimpse(sub_col)
# convert HOURLYPrecip into numeric column
sub_col$HOURLYPrecip <- as.numeric(sub_col$HOURLYPrecip)
glimpse(sub_col)
# renamming data columns
names(sub_col)<- c('relative_humidity','dry_bulb_temp_f','precip','wind_speed','station_pressure')
#make some visualization to identify distributin of the data 
p1 <-ggplot(data =sub_col)+geom_histogram(aes(x=relative_humidity),bins = 30)
p2 <-ggplot(data =sub_col)+geom_histogram(aes(x=dry_bulb_temp_f),bins = 30)
p3 <-ggplot(data =sub_col)+geom_histogram(aes(x=precip),bins = 30) 
p4 <-ggplot(data =sub_col)+geom_histogram(aes(x=wind_speed),bins = 30) 
p5 <-ggplot(data =sub_col)+geom_histogram(aes(x=station_pressure),bins = 30)  
grid.arrange(p1,p2,p3,p4,p5,ncol=1)
summary(sub_col$precip) 
count(sub_col$precip=="0.0")