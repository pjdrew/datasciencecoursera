---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

Show any code that is needed to:

1. Load the data (i.e. read.csv())

1. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity <- read.csv("activity.csv")
activity$n_date = as.Date(activity$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day


```r
day_steps <- ddply(activity, .(n_date), summarize, steps = sum(steps, na.rm = TRUE))
```

Make a histogram of the total number of steps taken each day


```r
qplot(day_steps$steps, binwidth=2000, geo="histogram", xlab="Steps", ylab="Frequency")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Calculate and report the mean and median of the total number of steps taken per day


```r
mean(day_steps$steps)
```

```
## [1] 9354.23
```

```r
median(day_steps$steps)
```

```
## [1] 10395
```
## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of:

1. the 5-minute interval (x-axis) 

1. and the average number of steps taken, averaged across all days (y-axis)


```r
interval_averages <- ddply(activity, .(interval), summarize, steps = mean(steps, na.rm = TRUE))
with(interval_averages,plot(interval,steps, type = "l", xlab = "5 min interval", ylab = "Avg Steps"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_averages[which.max(interval_averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Inputting missing values

Note that there are a number of days/intervals where there are missing values (coded as NA).

The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing <- sum(is.na(activity))
missing
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. 

The strategy does not need to be sophisticated. 
For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
act2 <- activity
for (i in 1:length(act2$steps))
    {if (is.na(act2[i,1]))
        {act2$steps[i] <- interval_averages$steps[which(interval_averages$interval == act2$interval[i])]
        }
    }
```
Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 

Do these values differ from the estimates from the first part of the assignment? 

What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
day2_steps <- ddply(act2,  .(n_date), summarize, steps = sum(steps, na.rm = TRUE))
qplot(day2_steps$steps, binwidth=2000, geo="histogram", xlab="Steps", ylab="Frequency")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. 

Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekends <- c('Saturday', 'Sunday')
day2_steps$dow <- factor((weekdays(day2_steps$n_date) %in% weekends), 
                          levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))
```

Make a panel plot containing:

1. a time series plot (i.e. type = "l") of the 5-minute interval (x-axis), and

1. the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
act2$dow <- factor((weekdays(act2$n_date) %in% weekends),levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))
by_dow <- ddply(act2, .(interval, dow), summarize, avg_steps = mean(steps))
qplot(interval, avg_steps, data = by_dow, geom = "line") + facet_grid(. ~ dow)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 