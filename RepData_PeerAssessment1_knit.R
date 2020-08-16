#install.packages('rmarkdown')

# Reproducible Research: Peer Assessment 1



## Load and process the data
##### 1. Load the data 


# Introduction

#It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

#This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#The data for this assignment can be downloaded from the course web site:
  
 # Dataset: Activity monitoring data [52K]

# The variables included in this dataset are:
#   
#   steps: Number of steps taking in a 5-minute interval (missing values are coded as NA\color{red}{\verb|NA|}NA)
# date: The date on which the measurement was taken in YYYY-MM-DD format
# interval: Identifier for the 5-minute interval in which measurement was taken
# 
# The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
# 
# start:
  
#load libraries



echo = TRUE

library(knitr)
library(ggplot2)
library(data.table)
library(dplyr)

if(!file.exists('activity.csv')){
  temp <- tempfile()
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl, temp)
  unzip(temp, 'activity.csv')
  unlink(temp)
}

activity <- read.csv("activity.csv", header=T, sep =",")
head(activity)
tail(activity)
str(activity)

echo = TRUE

su <- tapply(activity$steps, activity$date, sum, na.rm=T)

echo = TRUE
hist(su, xlab = "sum of steps per day", main = "histogram of steps per day")


echo = TRUE
mean_su <- round(mean(su))
median_su <- round(median(su))

print(c("The mean is",mean_su))
print(c("The median is",median_su))

echo = TRUE
mn_int <- tapply(activity$steps, activity$interval, mean, na.rm=T)
plot(mn_int ~ unique(activity$interval), type="l", xlab = "5-min interval")

echo = TRUE
mn_int[which.max(mn_int)]

echo = TRUE
table(is.na(activity) == TRUE)
summary(activity)

echo = TRUE
su2 <- tapply(activity$steps, activity$date, sum, na.rm=T)
hist(su2, xlab = "sum of steps per day", main = "histogram of steps per day")
mean_su2 <- round(mean(su2))
median_su2 <- round(median(su2))

echo = TRUE
print(c("The mean is",mean_su2))
print(c("The median is",median_su2))

