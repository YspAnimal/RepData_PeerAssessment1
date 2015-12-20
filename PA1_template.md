#Reproducible Research: Peer Assessment 1

###For data processing we will use next parameters and libraries

```r
Sys.setlocale("LC_ALL", "English_United Kingdom")
library(plyr)
library(dplyr)
library(ggplot2)
library(lattice)
```


##Loading and preprocessing the data
###We work in specific working directory

```r
setwd("C:/RProjects/RepData_PeerAssessment1")
DataActivity <- read.csv("activity.csv")
head(DataActivity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## First question: What is mean total number of steps taken per day?

###1.Calculate the total number of steps taken per day

```r
DataActivityDAY <- ddply(DataActivity, .(date), summarise, steps = sum(steps, na.rm = T))
head(DataActivityDAY)
```

```
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

###2. Make a histogram of the total number of steps

```r
ggplot(DataActivityDAY, aes(steps)) + 
  geom_histogram(colour = "darkgreen", fill = "white", binwidth = 2000) +
  geom_vline(aes(xintercept = median(steps), color="#dfc27d")) + 
  geom_vline(aes(xintercept = mean(steps), color="#80cdc1"))
```

![plot of chunk unnamed-chunk-4](figure/Fig1.png) 

###3. Calculate and report the median and mean total number of steps taken per day

```r
median(DataActivityDAY$steps)
```

```
## [1] 10395
```

```r
mean(DataActivityDAY$steps)
```

```
## [1] 9354.23
```

## Second question:What is the average daily activity pattern?
### In first, create a data frame with the intervals and the mean of steps per interval across al days

```r
DataActivityAVG <- ddply(DataActivity, .(interval), summarise, AvgSteps = mean(steps, na.rm = T))
```

###1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
ggplot(DataActivityAVG, aes(interval, AvgSteps)) + 
  geom_line() +
  ggtitle("Average daily activity") +
  xlab("5-minute intervals") + 
  ylab("Averaged steps") + 
  geom_hline(aes(yintercept = max(AvgSteps), color="#dfc27d"))
```

![plot of chunk unnamed-chunk-7](figure/Fig2.png)  

###2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
MaxSteps <- summarise(DataActivityAVG, max(AvgSteps))
DataActivityAVG[DataActivityAVG$AvgSteps==MaxSteps$`max(AvgSteps)`, ]$interval
```

```
## [1] 835
```

## Third question: Imputing missing values
####Note that there are a number of days/intervals where there are missing values (coded as NA).

###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(DataActivity$steps))
```

```
## [1] 2304
```

###2. Devise a strategy for filling in all of the missing values in the dataset.

```r
DataActivityNew <- DataActivity %>% 
  group_by(interval) %>%
    mutate(steps = replace(steps, is.na(steps), mean(steps, na.rm=TRUE)))
```

###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
DataActivityNew <- ddply(DataActivityNew, .(date), summarise, steps = sum(steps, na.rm = T))
```

###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
ggplot(DataActivityNew, aes(steps)) + 
  geom_histogram(colour = "darkgreen", fill = "white", binwidth = 2000) +
  geom_vline(aes(xintercept = median(steps), color="#dfc27d")) + 
  geom_vline(aes(xintercept = mean(steps), color="#80cdc1"))
```

![plot of chunk unnamed-chunk-12](figure/Fig3.png) 


```r
median(DataActivityNew$steps)
```

```
## [1] 10766.19
```

```r
mean(DataActivityNew$steps)
```

```
## [1] 10766.19
```

## Fourth question: Are there differences in activity patterns between weekdays and weekends?

###1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
Weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
DataActivity$date <- as.Date(DataActivity$date)
DataActivityW <- DataActivity %>% 
  mutate(date = ifelse(weekdays(date) %in% Weekdays, "Weekday", "Weekend"))
DataActivityW$date <- as.factor(DataActivityW$date)
```

```r
str(DataActivityW)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

###2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.

```r
DataActivityW <- aggregate(steps ~ interval + date, data = DataActivityW, mean)
xyplot(steps ~ interval | date, DataActivityW, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-16](figure/Fig4.png) 
