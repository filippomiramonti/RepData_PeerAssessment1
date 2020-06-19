---
title: "Reproducible Research: Peer Assessment 1"
author: "Filippo Miramonti"
date: "19/06/2020"

output: 
  html_document:
    keep_md: true
---


## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰) </br>
- date: The date on which the measurement was taken in YYYY-MM-DD format </br>
- interval: Identifier for the 5-minute interval in which measurement was taken </br>

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. 


## Loading and preprocessing the data
- Downloading and unzipping the file from a url and installing ang loading ggplot2 package


```r
if(!file.exists("activity.csv")){
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url, "activity.zip", method = "curl", mode = "wd")
    unzip(zipfile = "activity.zip")
}

if (!"ggplot2" %in% installed.packages()) {
    warning("Package ggplot2 required for this script. Installing ggplot2 now.")
    install.packages("ggplot2")
}
library(ggplot2)
```

- Reading csv file and setting the "date" column in date format

```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
head(activity, 10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

## What is mean total number of steps taken per day?

- Calculate the total number of steps taken per day

```r
aggrAct <- aggregate(activity$steps, by=list(date = activity$date), FUN=sum)
colnames(aggrAct)[2] <- "steps"
head(aggrAct, 10)
```

```
##          date steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
```

- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
g <- ggplot(aggrAct, aes(steps, na.rm = TRUE))
g + geom_histogram(fill = "steelblue", binwidth = 1000) + 
    labs(title = "Daily steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

-Calculate and report the mean and median of the total number of steps taken per day

```r
meanAndMedian <- c(meanSteps = mean(aggrAct$steps, na.rm = TRUE), 
                   medianSteps =  median(aggrAct$steps, na.rm = TRUE))
print(meanAndMedian)
```

```
##   meanSteps medianSteps 
##    10766.19    10765.00
```

## What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
aggrInt <- aggregate(activity$steps, by=list(interval = activity$interval), 
                     FUN=mean, na.rm = TRUE)
colnames(aggrInt)[2] <- "steps"
head(aggrInt, 10)
```

```
##    interval     steps
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
```

```r
g <- ggplot(aggrInt, aes(interval, steps))
g + geom_line(size = 1, colour = "steelblue") + 
    labs(title = "Average daily steps by interval", x = "Interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxInt <- aggrInt$interval[which.max(aggrInt$steps)]
print(maxInt)
```

```
## [1] 835
```

## Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
## Filling in missing values with the mean for that 5-minute interval
for (i in 1:nrow(activity)) {
    if (is.na(activity$steps[i])) {
        activity$steps[i] <- aggrInt$steps[aggrInt$interval == activity$interval[i]]
    }
}
head(activity, 10)
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
```

- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
write.csv(activity, "tidyActivity.csv", row.names = FALSE)
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
meanAndMedianTidy <- c(meanSteps = mean(aggrAct$steps, na.rm = TRUE), 
                   medianSteps =  median(aggrAct$steps, na.rm = TRUE))
print(meanAndMedianTidy)
```

```
##   meanSteps medianSteps 
##    10766.19    10765.00
```

```r
aggrAct <- aggregate(activity$steps, by=list(date = activity$date), FUN=sum)
colnames(aggrAct)[2] <- "steps"

g <- ggplot(aggrAct, aes(steps, na.rm = TRUE))
g + geom_histogram(fill = "steelblue", binwidth = 1000) + 
    labs(title = "Daily steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Type of Estimate | meanSteps | medianSteps
--- | --- | ---
With na | 10766.19 | 10765.00
Filling in na with mean) | 10766.19 | 10765.00

## Are there differences in activity patterns between weekdays and weekends?

-Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
## "domenica" and "sabato" are the italian words for "sunday" and "saturday"
for (i in 1:nrow(activity)) {
    if (weekdays(activity$date[i]) == "domenica" | weekdays(activity$date[i]) == "sabato") {
        activity$day[i] <- "weekend"
    }
    else activity$day[i] <- "weekday"
}
activity$day <- as.factor(activity$day)
head(activity, 10)
```

```
##        steps       date interval     day
## 1  1.7169811 2012-10-01        0 weekday
## 2  0.3396226 2012-10-01        5 weekday
## 3  0.1320755 2012-10-01       10 weekday
## 4  0.1509434 2012-10-01       15 weekday
## 5  0.0754717 2012-10-01       20 weekday
## 6  2.0943396 2012-10-01       25 weekday
## 7  0.5283019 2012-10-01       30 weekday
## 8  0.8679245 2012-10-01       35 weekday
## 9  0.0000000 2012-10-01       40 weekday
## 10 1.4716981 2012-10-01       45 weekday
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
aggrIntDay <- aggregate(activity$steps, by=list(interval = activity$interval, day = activity$day), 
                     FUN=mean, na.rm = TRUE)
colnames(aggrIntDay)[3] <- "steps"

g <- ggplot(aggrIntDay, aes(interval, steps))
g + geom_line(size = 1, show.legend = FALSE, aes(colour = factor(day))) + labs(title = "Average daily steps by weekday", x = "Interval", y = "Steps") +
    scale_color_manual(values = c("weekday" = "orangered", "weekend" = "steelblue")) +
    facet_grid(day ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
