---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
library(dplyr)
library(lattice)
```

## Loading and preprocessing the data
1. Load the data

```r
data <- read.csv("activity.csv")
```

2.Process/transform the data 

```r
doc <- tbl_df(na.omit(data)) 
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
stepsSum <- tapply(doc$steps, doc$date, sum)
stepsSum
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

2. Histogram of the total number of steps taken each day

```r
hist(stepsSum, main = "Total number of steps taken each day",
     xlab = "Total number of steps per day", ylab = "Frequency") 
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

3. Mean and median of the total number of steps taken per day

```r
mean1 <- mean(stepsSum, na.rm = TRUE)
mean1
```

```
## [1] 10766.19
```


```r
median1 <- median(stepsSum, na.rm = TRUE)
median1
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averDaily <- tapply(doc$steps, doc$interval, mean)
intervals <- unique(doc$interval)
plot(intervals, averDaily, type = "l",
     main = "Average daily activity pattern",
     xlab = "5-minute interval",
     ylab = "Average daily steps") 
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
df1 <- as.data.frame(cbind(averDaily, intervals))
maxSteps <- df1[df1$averDaily == max(df1$averDaily), ]
max <- maxSteps$intervals
max
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
naNum <- sum(is.na(data$steps))
naNum
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. Filling in all of the missing values, and creating a new dataset 'df2' that is equal to the original dataset but with the missing data filled in.

```r
df2 <- data
for(i in 1:length(df2$steps)){
    if(is.na(df2$steps[i])){
        df2$steps[i] <- df1$averDaily[which(row.names(df1)== df2$interval[i])]
    }
}
```

3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
stepsSum2 <- tapply(df2$steps, df2$date, sum)
hist(stepsSum2,
     main = "Total number of steps taken each day",
     xlab = "Total number of steps per day",
     ylab = "Frequency") 
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

Mean total number of steps taken per day

```r
mean2 <- mean(stepsSum2)
mean2
```

```
## [1] 10766.19
```

Median total number of steps taken per day

```r
median2 <- median(stepsSum2)
median2
```

```
## [1] 10766.19
```

If the mean differs from the estimates from the first part of the assignment

```r
mean1 == mean2
```

```
## [1] TRUE
```

If the median differs from the estimates from the first part of the assignment

```r
median1 == median2
```

```
## [1] FALSE
```

What is the impact of imputing missing data on the estimates of the total daily number of steps?
-In this project, the missing values were filled with the 'mean' of the total number of steps taken per day, so that the 'mean' remained unchanged after imputing the missing data. However, imputing missing data changed the 'median' from the first part of the assignment.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
df2 <- tbl_df(df2)
df2 <- mutate(df2, Day = weekdays(as.Date(date)))
wday <- c("Monday", "Tuesday","Wednesday", "Thursday","Friday")
for(i in 1:length(df2$Day)){
    if(df2$Day[i] %in% wday){
        df2$Day[i] = "Weekdays"
    }
    else{
        df2$Day[i] = "Weekends"
    }
}
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
df2d <- filter(df2, Day == "Weekdays") 
averDaily2 <- tapply(df2d$steps, df2d$interval, mean)
interval2 <- unique(df2d$interval)
df2d <- tbl_df(as.data.frame(cbind(averDaily2,interval2)))
df2d <- mutate(df2d, Day = "Weekdays")

df2e <- filter(df2, Day == "Weekends") 
averDaily2 <- tapply(df2e$steps, df2e$interval, mean)
interval2 <- unique(df2e$interval)
df2e <- tbl_df(as.data.frame(cbind(averDaily2,interval2)))
df2e <- mutate(df2e, Day = "Weekends")

df3 <- bind_rows(df2d, df2e)
xyplot(averDaily2~interval2 | df3$Day, data = df3, 
       type = "l", layout = c(1,2),
       main = "Activity patterns between weekdays and weekends",
       xlab = "5-minute interval",
       ylab = "Average number of steps") 
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 



