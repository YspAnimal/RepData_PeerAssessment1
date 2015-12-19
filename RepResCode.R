#Set working directory and then download file, unzip it if it does not exist
setwd("C:/Users/soloveynv/Documents/R Scripts/Coursera/RepResearch/RepData_PeerAssessment1")

URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destFile <- "RepdataDataActivity.zip"
if (!file.exists(destFile)){
  download.file(URL, destfile = destFile, mode='wb')
  unzip(destFile)
}

DataActivity <- read.csv("activity.csv")

#What is mean total number of steps taken per day?
library(plyr)
library(ggplot2)
#1.Calculate the total number of steps taken per day
DataActivityDAY <- ddply(DataActivity, .(date), summarise, steps = sum(steps, na.rm = T))
#2. Make a histogram of the total number of steps
ggplot(DataActivityDAY, aes(steps)) + 
  geom_histogram(colour = "darkgreen", fill = "white", binwidth = 2000) +
  geom_vline(aes(xintercept = median(steps), color="#dfc27d")) + 
  geom_vline(aes(xintercept = mean(steps), color="#80cdc1"))

median(DataActivityDAY$steps)
mean(DataActivityDAY$steps)


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
summarise(DataActivityAVG, max(AvgSteps))




#qplot(steps, data = DataActivityDAY, binwidth = 2000)
  
hist(DataActivityDAY$steps, xlab = "Steps per day")
#3. Calculate and report the mean and median of the total number of steps taken per day
summary(DataActivityDAY$steps, na.rm = T)
mean(DataActivityDAY$steps, na.rm = T)
median(DataActivityDAY$steps, na.rm = T)

library(ggplot2)
library(dplyr)


qplot(interval, steps, data = DataActivity)

plot(DataActivity$interval, DataActivity$steps, type = "l")


CleanDataSet <- DataActivity[complete.cases(DataActivity), ]


