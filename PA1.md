---
title: "Reproducible Research: Peer Assessment 1"
author: "Mike Liu"
date: "6/18/2020"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
1. Reading in Dataset

```r
df = read.csv("activity.csv")
df$date = as.Date(df$date)
```

2. Histogram of total numbers of steps taken each day
![](A1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
## What is mean total number of steps taken per day?
3. Mean and median number of steps taken each day:

```r
print(paste("Mean steps taken each day:", mean(tot_steps_per_day$x[!is.na(tot_steps_per_day$x)])))
```

```
## [1] "Mean steps taken each day: 10766.1886792453"
```

```r
print(paste("Median steps taken each day:", median(tot_steps_per_day$x[!is.na(tot_steps_per_day$x)])))
```

```
## [1] "Median steps taken each day: 10765"
```
## What is the average daily activity pattern?
4. Time-series plot of average number of steps taken

```r
plotdf = df[!is.na(df$steps),]
avg_steps_per_day <- aggregate(plotdf$steps, by=list(plotdf$date), FUN=mean)
plot(x=avg_steps_per_day$Group.1, y=avg_steps_per_day$x, 
     main = "Avg Steps per Day Over Time",
     xlab = "Date", ylab = "Avg Steps",
     type="l")
```

![](A1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

5. 5-min interval that contains the max number of steps

```r
five_min_int_steps <- aggregate(plotdf$steps, by=list(plotdf$interval), FUN=mean)
max_int <- with(five_min_int_steps, Group.1[x=max(x)])
print(paste("5-min interval that contains the max number of steps:", max_int))
```

```
## [1] "5-min interval that contains the max number of steps: 1705"
```

## Imputing missing values
6. Code to describe and show a strategy for imputing missing data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# Using a interval-mean imputation strategy for missing steps
df_imp <- df %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE),steps))
print(head(df_imp))
```

```
## # A tibble: 6 x 3
## # Groups:   interval [6]
##    steps date       interval
##    <dbl> <date>        <int>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```

7. Histogram of total number of steps taken each day after missing values are imputed

```r
imp_tot_steps_per_day <- aggregate(df_imp$steps, by=list(df_imp$date), FUN=sum)
hist(imp_tot_steps_per_day$x, main="Histogram of Total Number of Steps per Day", xlab="Sum of Steps Per Day")
```

![](A1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
8. Panel plot comparing avg number of steps taken per 5-min inteval across weekdays and weekends

```r
# Add in boolean mask for weekend or not
df_imp$Weekday <- ifelse(weekdays(df_imp$date) %in% c("Saturday", "Sunday"), FALSE, TRUE)

# Use aggregate to aggregate weekday and weekend steps
avg_steps_weekday <- with(df_imp[df_imp$Weekday,], aggregate(steps, by=list(interval), FUN=mean))
avg_steps_weekend <- with(df_imp[!df_imp$Weekday,], aggregate(steps, by=list(interval), FUN=mean))

# Set up panels, plot line charts for weekdays and weekends
par(mfrow = c(1,2))
plot(x=avg_steps_weekday$Group.1, 
     y=avg_steps_weekday$x, 
     xlab = "5-Min Interval", ylab = "Avg Steps",
     main="Avg Steps per Interval, Weekdays", type="l")
plot(x=avg_steps_weekend$Group.1, 
     y=avg_steps_weekend$x, 
     xlab = "5-Min Interval", ylab = "Avg Steps",
     main="Avg Steps per Interval, Weekends", type="l")
```

![](A1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


