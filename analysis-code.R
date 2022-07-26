#import libray we need for this project 
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(tidymodels)
library(rlang)
library(tidyr)
library(magrittr)
library(coefplot)
library(boot)
library(rsample)
library(useful)
library(glmnet)
library(parallel)
library(doParallel)
library(reshape2)
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
p3 <-ggplot(data =sub_col)+geom_histogram(aes(x=(precip)**.2),bins = 30)
p3+scale_x_continuous(breaks=seq(0,3,by=0.2))
p3 <-ggplot(data =sub_col)+geom_histogram(aes(x=precip),bins = 30) 
p4 <-ggplot(data =sub_col)+geom_histogram(aes(x=wind_speed),bins = 30) 
p5 <-ggplot(data =sub_col)+geom_histogram(aes(x=station_pressure),bins = 30)  
grid.arrange(p1,p2,p3,p4,p5,ncol=1)
summary(sub_col$precip) 
#create logical response for our model 
#sub_col$precip <- with(sub_col,precip>0)
#dividing data into training and test set with the same random variable
set.seed(1234)
case_data <- initial_split(data = sub_col, prop = 0.8)
traind_data <- training(case_data)
test_data   <- testing(case_data)
glimpse(sub_col)
#build diffferent models with different interation between feature and choose the best of them

#glm_1 <-glm(precip~I(dry_bulb_temp_f+relative_humidity*wind_speed*station_pressure)^2,
        #  traind_data,family=binomial(link="logit"))
#glm_2 <-glm(precip~dry_bulb_temp_f*relative_humidity*wind_speed*station_pressure
          # ,traind_data,family=binomial(link="logit"))
#create new lm model
new_model <- lm(precip~dry_bulb_temp_f+relative_humidity+wind_speed+station_pressure,data = traind_data)
pred <- predict(new_model,newdata =  test_data,interval = "confidence")
head(test_data)
summary(new_model)
summary(pred)
model_1<-lm(precip~wind_speed+station_pressure+dry_bulb_temp_f+relative_humidity,data = sub_col)
#asset the model visually by cheking assumption about linear model
#first useing fitted value against residuals {by using fortifies to gain .fitted&.resid variables}
#to check linearity and homoscedasticity 
plot(model_1,which = 1)
#to check normality of the model usign q-q plot
plot(model_1,which = 2)
#to check homoscedasticity
plot(model_1,which = 3)
# check outlier influential in linear regression analysis
plot(model_1,which = 5)


#ggplot(traind_data,aes(x=relative_humidity, y=precip))+geom_point()+geom_smooth(method = "lm",col="red",formula = y~x)

#to asset the model numerically {find how close the data is to fitted regression line}
#find out percentage of variation of the target variable 
summary(model_1)
traind_data$predicted<-predict(model_1,traind_data)
head(traind_data)
#steps for comparing actual value and predicted value visually
# 1-using ggplot with data and two variable of comparson 
# 2- plot acual values useing this command geom_point()
# 3-plot regression line using geom_smooth(method = "lm",se=FALSE,color,formula)
# 4-plot predicted value with also geom_point() but now with aes(y= prediced values)
# 5-finally connect all predicted value and actual value using geom_segment(aes(xend,yend))
ggplot(traind_data, aes(x=relative_humidity,y=precip))+
#plot the actual points  
geom_point()+
#plot regression line
geom_smooth(method = "lm",se=FALSE,color="red",formula = y~x) +
#add the predicted values
geom_point(aes(y=predicted),color = "green")+  
#connect the actual data point to their corresponding predicted value
geom_segment(aes(xend=relative_humidity,yend = predicted))
 colnames(sub_col)
# gather data into one column include all columns name and another columns for gather all values inside these columns 
gathring_data <- sub_col %>%  gather(key = Type,value = Measurement,relative_humidity,dry_bulb_temp_f,wind_speed,station_pressure)
colnames(gathring_data)
head(gathring_data)
workf
