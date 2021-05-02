
#Attribute Information:
  
#1.date: Date in format dd/mm/yyyy
#2.time: time in format hh:mm:ss
#3.global_active_power: household global minute-averaged active power (in kilowatt)
#4.global_reactive_power: household global minute-averaged reactive power (in kilowatt)
#5.voltage: minute-averaged voltage (in volt)
#6.global_intensity: household global minute-averaged current intensity (in ampere)
#7.sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
#It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#8.sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). 
#It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#9.sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
#It corresponds to an electric water-heater and an air-conditioner.
# https://brianward1428.medium.com/introduction-to-tidyverse-7b3dbf2337d5 

#=================================================================
#load libraries 
#=================================================================

library(RMariaDB) 
library(caret)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)
library(GGally)
library(psych)
library(reshape)
library(dplyr)


#=================================================================
#Connection
#=================================================================
con = dbConnect(MariaDB(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
#list the tables 
dbListTables(con)
#List the features 
dbListFields(con,'iris')
#query database to download data
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
#select features and download data 
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#Using the dbListFields function learn the attributes associated with the yr_2006 table.
dbListFields(con,'yr_2006')
#select data and downlaod
yr_2006 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2006")
yr_2007 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2007")
yr_2008 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2008")
yr_2009 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2009")
yr_2010 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2010")

UD_finder_2006 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2006")
View(UD_finder_2006)
UD_finder_2007 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2007")
View(UD_finder_2007)
UD_finder_2008 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2008")
View(UD_finder_2008)
UD_finder_2009 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2009")
View(UD_finder_2009)
UD_finder_2010 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2010")
View(UD_finder_2010)




str(yr_2006) 
summary(yr_2006)
head(yr_2006) 
tail(yr_2006) # covers only December

str(yr_2007)
summary(yr_2007)
head(yr_2007) 
tail(yr_2007) # covers till December
View(yr_2007)
str(yr_2008)
summary(yr_2008)
head(yr_2008) 
tail(yr_2008) # covers till December

str(yr_2009)
summary(yr_2009)
head(yr_2009) 
tail(yr_2009)  # covers till December

str(yr_2010)
summary(yr_2010)
head(yr_2010) 
tail(yr_2010) # does not cover till December 


#=================================================================
#Data selected 
#=================================================================
## Combine tables into one dataframe using dplyr
newDF <- rbind(yr_2007, yr_2008, yr_2009)

View(newDF)

str(newDF)
summary(newDF)
head(newDF) 
tail(newDF)

## Combine Date and Time attribute values in a new attribute column
newDF <-cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 

colnames(newDF)[6] <-"DateTime"
## Move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)

## Convert DateTime from character to POSIXct 
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
## Add the time zone
#attr(newDF$DateTime, "tzone") <- "Europe/Paris"
attr(newDF$DateTime, "tzone") <- "GMT"

## Inspect the data types
str(newDF)


#=================================================================
#lubridate
#=================================================================

## Create "year" attribute with lubridate
newDF$year <- year(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)
newDF$week <- week(newDF$DateTime)

## Move the attributes - 6 times
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]


df_pc <- newDF
#=================================================================
#Remove DateTime and Date 
#=================================================================
df_pc$DateTime <- NULL
df_pc$Date <- NULL
df_pc$Time <- NULL
View(df_pc)

str(df_pc)
summary(df_pc)
head(df_pc) 
tail(df_pc)
#=================================================================
#Analyzing Summary Statistics
#=================================================================
psych::describe(df_pc)

#=================================================================
#correlation 
#=================================================================
corrData <- cor(df_pc)
corrData
corrplot(corrData)
#observation of the plot - sub meter 1 has positive correlation with hour, sub meter 2 and 3 
#sub meter 2 is negatively correlated to year, positive  with hour, sub meter 1 and 3
# sub meter 3 is positive with year, hour,sub meter 1 and 2.
# hour and year are important 
# Week and Month are highly correlated and week is positive with day. 
#=================================================================
#Checking Outliers Using Boxplots
#=================================================================
meltData <- melt(df_pc)
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")

plot(df_pc$Sub_metering_1)
plot(df_pc$Sub_metering_2)
plot(df_pc$Sub_metering_3)


#playing with dplyr 

filter(df_pc, month==7, year==2008)

# chaining method
df_pc %>%
  select(Sub_metering_1) 

df_pc %>%
  group_by(year) %>%
  summarise_each(list(mean), Sub_metering_1, Sub_metering_2,Sub_metering_3)


df_pc %>%
  group_by(month) %>%
  filter(between(month,3,8)) %>%
summarise_each(list(mean), Sub_metering_1, Sub_metering_2,Sub_metering_3)

df_pc %>%
group_by(month, year) %>%
  filter(year==2009) %>%
  filter(between(month,3,8)) %>%
  summarise_each(list(mean), Sub_metering_1, Sub_metering_2,Sub_metering_3)


