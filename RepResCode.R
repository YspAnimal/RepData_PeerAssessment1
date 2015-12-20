#Set working directory and then unzip data-file if it does not exist
setwd("C:/RProjects/RepData_PeerAssessment1")
Sys.setlocale("LC_ALL", "English_United Kingdom")
DataActivity <- read.csv("activity.csv")

#What is mean total number of steps taken per day?
library(plyr)
library(dplyr)
library(ggplot2)
library(lattice)

#
#1.Calculate the total number of steps taken per day
DataActivityDAY <- ddply(DataActivity, .(date), summarise, steps = sum(steps, na.rm = T))
#2. Make a histogram of the total number of steps
ggplot(DataActivityDAY, aes(steps)) + 
  geom_histogram(colour = "darkgreen", fill = "white", binwidth = 2000) +
  geom_vline(aes(xintercept = median(steps), color="#dfc27d")) + 
  geom_vline(aes(xintercept = mean(steps), color="#80cdc1"))

median(DataActivityDAY$steps)
mean(DataActivityDAY$steps)
#hist(DataActivityDAY$steps, xlab = "Steps per day", main = "Total number of steps taken per day")


#What is the average daily activity pattern?

DataActivityAVG <- ddply(DataActivity, .(interval), summarise, AvgSteps = mean(steps, na.rm = T))
#1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
ggplot(DataActivityAVG, aes(interval, AvgSteps)) + 
  geom_line() +
  ggtitle("Average daily activity") +
  xlab("5-minute intervals") + 
  ylab("Averaged steps") + 
  geom_hline(aes(yintercept = max(AvgSteps), color="#dfc27d"))
#2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  
MaxSteps <- summarise(DataActivityAVG, max(AvgSteps))
DataActivityAVG[DataActivityAVG$AvgSteps==MaxSteps$`max(AvgSteps)`, ]$interval

#
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(DataActivity$steps))

#Devise a strategy for filling in all of the missing values in the dataset.
#Use DataActivityAVG to find mean of 5 minute interval
DataActivityNew <- DataActivity %>% 
  group_by(interval) %>%
    mutate(steps = replace(steps, is.na(steps), mean(steps, na.rm=TRUE)))

#Create a new dataset that is equal to the original dataset but with the
#missing data filled in.
DataActivityNew <- ddply(DataActivityNew, .(date), summarise, steps = sum(steps, na.rm = T))
#Make a histogram of the total number of steps taken each day and Calculate
#and report the mean and median total number of steps taken per day.
ggplot(DataActivityNew, aes(steps)) + 
  geom_histogram(colour = "darkgreen", fill = "white", binwidth = 2000) +
  geom_vline(aes(xintercept = median(steps), color="#dfc27d")) + 
  geom_vline(aes(xintercept = mean(steps), color="#80cdc1"))

median(DataActivityNew$steps)
mean(DataActivityNew$steps)

#Are there differences in activity patterns between weekdays and weekends?

Weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
DataActivity$date <- as.Date(DataActivity$date)
DataActivityW <- DataActivity %>% 
  mutate(date = ifelse(weekdays(date) %in% Weekdays, "Weekday", "Weekend"))
DataActivityW$date <- as.factor(DataActivityW$date)

DataActivityW <- aggregate(steps ~ interval + date, data = DataActivityW, mean)
xyplot(steps ~ interval | date, DataActivityW, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
