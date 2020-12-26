#Part A Data Reading:
#loading of Data Files:
data_1808 <- read.csv(file.choose())
data_1809 <- read.csv(file.choose())
data_1810 <-read.csv(file.choose())
data_1811 <-read.csv(file.choose())
data_1812 <-read.csv(file.choose())
data_1901 <-read.csv(file.choose())
data_1902 <-read.csv(file.choose())
data_1903 <-read.csv(file.choose())
data_1904 <-read.csv(file.choose())
data_1905 <-read.csv(file.choose())
data_1906 <-read.csv(file.choose())
data_1907 <-read.csv(file.choose())
data_1908 <-read.csv(file.choose())
data_1909 <-read.csv(file.choose())
data_1910 <-read.csv(file.choose())
data_1911 <-read.csv(file.choose())
data_1912 <-read.csv(file.choose())
data_2001 <-read.csv(file.choose())
data_2002 <-read.csv(file.choose())
# appending the files using the tidyverse package:
install.packages("Rcpp")
install.packages("dplyr")
install.packages("purrr")
install.packages("readr")
library(dplyr)
library(ggplot2)
library(tidyverse)
library(purrr)
library(readr)
all_csv_files <-list.files(path = "C:\\Users\\DELL\\Downloads\\Assignment1 data files", pattern = "csv$", full.names = TRUE)
all_csv_files
reading_data <- map_df(all_csv_files,read.csv)
reading_data
#second as per the assignment:
setwd("C:\\Users\\DELL\\Downloads\\Assignment1 data files")
files <- c("C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201808.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201809.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201810.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201811.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201812.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201901.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201902.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201903.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201904.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201905.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201906.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201907.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201908.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201909.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201910.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201911.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\201912.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\202001.csv","C:\\Users\\DELL\\Downloads\\Assignment1 data files\\202002.csv")
files
data <- data.frame()
for (i in 1:length(files)){
  temp <- read_csv(files[i], skip = 7)
  data <- rbind(data, temp)
}
View(data)
structure(data)
dim(data)
assertthat::assert_that(nrow(problems(data))==0, msg = "There are still some problems in the data you need to fix first")

#Part B:
install.packages("lubridate")
library(lubridate)
is.na(data)
na.omit(data)
head(data)
colnames(data)
colnames(data)<- c("Date","month","year","min","max","Rainfall_(mm)","Evaporation_(mm)","Sunshine_(hours)","Direction_max_wind_gust","Max_wind_gust_speed","max_wind_gust_time","temp_9am","humid_9am","cloud_amt_okt_9am","wind_dir_9am","9am_wind_speed","9am_pressure","3pm_rel_pressure","3p_temp","humid_3pm","cloud_amt_3pm","wind_dir_3pm","wind_speed_3pm","pressure_3pm")
colnames(data)
data$Date <-as.Date(as.character(data$Date))
typeof(data$Date)
View(data)
data$Date
data$Date <- data$month
data$month
data$Date <- data$year
data$year
as.ordered(data$Date)
na.omit(data$Date)
median(data$Date)
na.omit(reading_data)
is.na(data$month)
is.na(data$year)
na.omit(data$year)
#Part C:
library(ggplot2)
summary(data)
summary(data$Date)
summary(data$year)
summary(data$min)
summary(data$temp_9am)
summary(data$Max_wind_gust_speed)
mean(data$month/data$min)
mean(data$month/data$max)
mean(data$year/data$min)
mean(data$year/data$max)
mean(data$Direction_max_wind_gust)
summary(data$`Rainfall_(mm)`)
data$humid_9am
summary(data$`Evaporation_(mm)`)
summary(data$humid_3pm)
summary(data_1901)
mean(data$temp_9am)
min(data$temp_9am)
max(data$cloud_amt_okt_9am)
ggplot(data,aes(x=min))+geom_histogram()
ggplot(data,aes(x=wind_dir_9am))+geom_histogram()
ggplot(data,aes(x=month))+geom_histogram()



