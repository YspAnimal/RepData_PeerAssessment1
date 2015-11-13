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

hist(DataActivity$steps, xlab = "Steps per day", ylim = c(0, 2000))
summary(DataActivity$steps, na.rm = T)

#mean(DataActivity$steps, na.rm = T)
#median(DataActivity$steps, na.rm = T)

library(ggplot2)
qplot(interval, steps, data = DataActivity)

plot(DataActivity$interval, DataActivity$steps, type = "l")




CleanDataSet <- DataActivity[complete.cases(DataActivity), ]


