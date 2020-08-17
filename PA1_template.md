---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## A. Loading and preprocessing the data

```r
if(!file.exists("./C5_data/Activity_monitoring_data.zip")){
  dir.create("./C5_data")
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl, destfile="./C5_data/Activity_monitoring_data.zip")
  unzip("./C5_data/Activity_monitoring_data.zip", exdir="./C5_data")
}

# Check extracted content in zip file
list.files("./C5_data", recursive=TRUE)
```

```
## [1] "activity.csv"                 "Activity_monitoring_data.zip"
```

```r
# Load data
activitydata <- read.csv("./C5_data/activity.csv")

# Explore data
str(activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activitydata)
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


## B. What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day. 

```r
totalstep <- with(activitydata, aggregate(steps~date, FUN=sum, na.rm=TRUE))
head(totalstep)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. Make histogram of the total number of steps taken each day.

```r
hist(totalstep$steps, xlab="Number of Steps per day", main="", breaks=25)
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

3. Calculate mean and median of the total number of steps taken per day

```r
mean(totalstep$steps)
```

```
## [1] 10766.19
```

```r
median(totalstep$steps)
```

```
## [1] 10765
```


## C. What is the average daily activity pattern?
1. Make a time series plot of the 5 minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(dplyr)
average <- with(activitydata, aggregate(steps~interval, FUN=mean, na.rm=TRUE))
with(average, plot(interval, steps, type="l", main="Average daily activity pattern"))
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxstep <- average[which.max(average$steps),]
maxstep$interval
```

```
## [1] 835
```


## D. Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nasum <- sum(is.na(activitydata))
nasum
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
meanstepsperinterval <- mean(average$steps)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activitydata2 <- activitydata
stepNA <- is.na(activitydata$steps)
activitydata2[stepNA,1] <- meanstepsperinterval
head(activitydata2)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalstepperday <- with(activitydata2, aggregate(steps~date, FUN=sum))
hist(totalstepperday$steps, xlab="Number of Steps per Day", ylab="Number of Days", main="Number of steps per day")
```

![](PA1_template_files/figure-html/hist2-1.png)<!-- -->

```r
mean(totalstepperday$steps)
```

```
## [1] 10766.19
```

```r
median(totalstepperday$steps)
```

```
## [1] 10766.19
```


## E. Are there differences in activity patterns between weekdays and weekends? 
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
library(dplyr)
activitydata2$date <- as.Date(activitydata2$date, format="%Y-%m-%d")
activitydata2 <- mutate(activitydata2, day=ifelse(weekdays(activitydata2$date)=="Saturday"| weekdays(activitydata2$date)=="Sunday", "Weekend", "Weekday"))
head(activitydata2)
```

```
##     steps       date interval     day
## 1 37.3826 2012-10-01        0 Weekday
## 2 37.3826 2012-10-01        5 Weekday
## 3 37.3826 2012-10-01       10 Weekday
## 4 37.3826 2012-10-01       15 Weekday
## 5 37.3826 2012-10-01       20 Weekday
## 6 37.3826 2012-10-01       25 Weekday
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
xyplot(steps~interval|day, activitydata2, type="l", ylab="Number of Steps")
```

![](PA1_template_files/figure-html/xyplot-1.png)<!-- -->

