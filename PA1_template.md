# Reproducible Research: Peer Assessment 1
# Project 1
# By MAVC 18/04/2015

## Loading and preprocessing the data

```r
library(ggplot2)
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
activity$interval <- (activity$interval%/%100)*60 + activity$interval%%100
```

## What is mean total number of steps taken per day?

```r
total_steps_per_day <-sapply(split(activity$steps, f=activity$date, drop = FALSE), sum, na.rm = TRUE)
hist(total_steps_per_day, breaks = 20, main = "Total number of steps per Day", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean_steps_per_day <-sapply(split(activity$steps, f=activity$date, drop = FALSE), mean, na.rm = TRUE)
median_steps_per_day <-sapply(split(activity$steps, f=activity$date, drop = FALSE), median, na.rm = TRUE)
summary_analysis <- data.frame(mean_steps_per_day,median_steps_per_day)
summary_analysis
```

```
##            mean_steps_per_day median_steps_per_day
## 2012-10-01                NaN                   NA
## 2012-10-02          0.4375000                    0
## 2012-10-03         39.4166667                    0
## 2012-10-04         42.0694444                    0
## 2012-10-05         46.1597222                    0
## 2012-10-06         53.5416667                    0
## 2012-10-07         38.2465278                    0
## 2012-10-08                NaN                   NA
## 2012-10-09         44.4826389                    0
## 2012-10-10         34.3750000                    0
## 2012-10-11         35.7777778                    0
## 2012-10-12         60.3541667                    0
## 2012-10-13         43.1458333                    0
## 2012-10-14         52.4236111                    0
## 2012-10-15         35.2048611                    0
## 2012-10-16         52.3750000                    0
## 2012-10-17         46.7083333                    0
## 2012-10-18         34.9166667                    0
## 2012-10-19         41.0729167                    0
## 2012-10-20         36.0937500                    0
## 2012-10-21         30.6284722                    0
## 2012-10-22         46.7361111                    0
## 2012-10-23         30.9652778                    0
## 2012-10-24         29.0104167                    0
## 2012-10-25          8.6527778                    0
## 2012-10-26         23.5347222                    0
## 2012-10-27         35.1354167                    0
## 2012-10-28         39.7847222                    0
## 2012-10-29         17.4236111                    0
## 2012-10-30         34.0937500                    0
## 2012-10-31         53.5208333                    0
## 2012-11-01                NaN                   NA
## 2012-11-02         36.8055556                    0
## 2012-11-03         36.7048611                    0
## 2012-11-04                NaN                   NA
## 2012-11-05         36.2465278                    0
## 2012-11-06         28.9375000                    0
## 2012-11-07         44.7326389                    0
## 2012-11-08         11.1770833                    0
## 2012-11-09                NaN                   NA
## 2012-11-10                NaN                   NA
## 2012-11-11         43.7777778                    0
## 2012-11-12         37.3784722                    0
## 2012-11-13         25.4722222                    0
## 2012-11-14                NaN                   NA
## 2012-11-15          0.1423611                    0
## 2012-11-16         18.8923611                    0
## 2012-11-17         49.7881944                    0
## 2012-11-18         52.4652778                    0
## 2012-11-19         30.6979167                    0
## 2012-11-20         15.5277778                    0
## 2012-11-21         44.3993056                    0
## 2012-11-22         70.9270833                    0
## 2012-11-23         73.5902778                    0
## 2012-11-24         50.2708333                    0
## 2012-11-25         41.0902778                    0
## 2012-11-26         38.7569444                    0
## 2012-11-27         47.3819444                    0
## 2012-11-28         35.3576389                    0
## 2012-11-29         24.4687500                    0
## 2012-11-30                NaN                   NA
```

## What is the average daily activity pattern?

```r
mean_daily_activity_pattern <-sapply(split(activity$steps, f=activity$interval, drop = FALSE), mean, na.rm = TRUE)
plot(names(mean_daily_activity_pattern), mean_daily_activity_pattern, type = "l", main = "Mean Daily Activity Pattern", xlab = "Day in minutes", ylab = "Average steps across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maximun_activity_interval <- which.max(mean_daily_activity_pattern)
maximun_activity_interval
```

```
## 515 
## 104
```

## Imputing missing values

```r
number_NA <- sum(is.na(activity))
number_NA
```

```
## [1] 2304
```

```r
#New Data Frame
original_activity <- activity
#Method to replace NAs values by the most common number of steps recorded
most_common_number_steps <- which.max(table(activity$steps))
activity[is.na(activity)] <- as.numeric(names(most_common_number_steps))
total_steps_per_day_noNA <-sapply(split(activity$steps, f=activity$date, drop = FALSE), sum, na.rm = TRUE)
hist(total_steps_per_day_noNA, breaks = 20, main = "Total number of steps per Day", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean_steps_per_day_noNA <-sapply(split(activity$steps, f=activity$date, drop = FALSE), mean, na.rm = TRUE)
median_steps_per_day_noNA <-sapply(split(activity$steps, f=activity$date, drop = FALSE), median, na.rm = TRUE)
summary_analysis <- data.frame(mean_steps_per_day_noNA, median_steps_per_day_noNA)
summary_analysis
```

```
##            mean_steps_per_day_noNA median_steps_per_day_noNA
## 2012-10-01               0.0000000                         0
## 2012-10-02               0.4375000                         0
## 2012-10-03              39.4166667                         0
## 2012-10-04              42.0694444                         0
## 2012-10-05              46.1597222                         0
## 2012-10-06              53.5416667                         0
## 2012-10-07              38.2465278                         0
## 2012-10-08               0.0000000                         0
## 2012-10-09              44.4826389                         0
## 2012-10-10              34.3750000                         0
## 2012-10-11              35.7777778                         0
## 2012-10-12              60.3541667                         0
## 2012-10-13              43.1458333                         0
## 2012-10-14              52.4236111                         0
## 2012-10-15              35.2048611                         0
## 2012-10-16              52.3750000                         0
## 2012-10-17              46.7083333                         0
## 2012-10-18              34.9166667                         0
## 2012-10-19              41.0729167                         0
## 2012-10-20              36.0937500                         0
## 2012-10-21              30.6284722                         0
## 2012-10-22              46.7361111                         0
## 2012-10-23              30.9652778                         0
## 2012-10-24              29.0104167                         0
## 2012-10-25               8.6527778                         0
## 2012-10-26              23.5347222                         0
## 2012-10-27              35.1354167                         0
## 2012-10-28              39.7847222                         0
## 2012-10-29              17.4236111                         0
## 2012-10-30              34.0937500                         0
## 2012-10-31              53.5208333                         0
## 2012-11-01               0.0000000                         0
## 2012-11-02              36.8055556                         0
## 2012-11-03              36.7048611                         0
## 2012-11-04               0.0000000                         0
## 2012-11-05              36.2465278                         0
## 2012-11-06              28.9375000                         0
## 2012-11-07              44.7326389                         0
## 2012-11-08              11.1770833                         0
## 2012-11-09               0.0000000                         0
## 2012-11-10               0.0000000                         0
## 2012-11-11              43.7777778                         0
## 2012-11-12              37.3784722                         0
## 2012-11-13              25.4722222                         0
## 2012-11-14               0.0000000                         0
## 2012-11-15               0.1423611                         0
## 2012-11-16              18.8923611                         0
## 2012-11-17              49.7881944                         0
## 2012-11-18              52.4652778                         0
## 2012-11-19              30.6979167                         0
## 2012-11-20              15.5277778                         0
## 2012-11-21              44.3993056                         0
## 2012-11-22              70.9270833                         0
## 2012-11-23              73.5902778                         0
## 2012-11-24              50.2708333                         0
## 2012-11-25              41.0902778                         0
## 2012-11-26              38.7569444                         0
## 2012-11-27              47.3819444                         0
## 2012-11-28              35.3576389                         0
## 2012-11-29              24.4687500                         0
## 2012-11-30               0.0000000                         0
```

## Are there differences in activity patterns between weekdays and weekends?

```r
activity$day <- weekdays(activity$date)
activity[activity$day == "Monday",4] <- "weekday"
activity[activity$day == "Tuesday",4] <- "weekday"
activity[activity$day == "Wednesday",4] <- "weekday"
activity[activity$day == "Thursday",4] <- "weekday"
activity[activity$day == "Friday",4] <- "weekday"
activity[activity$day == "Saturday",4] <- "weekend"
activity[activity$day == "Sunday",4] <- "weekend"
activity$day <- as.factor(activity$day)
weekends <- activity[which(activity$day == 'weekday'),]
weekdays <- activity[which(activity$day == 'weekend'),]
mean_weekdays_activity_pattern <-sapply(split(weekdays$steps, f=weekdays$interval, drop = FALSE), mean, na.rm = TRUE)
mean_weekends_activity_pattern <-sapply(split(weekends$steps, f=weekends$interval, drop = FALSE), mean, na.rm = TRUE)
par(mfrow = c(1, 2))
plot(names(mean_weekdays_activity_pattern), mean_weekdays_activity_pattern, type = "l", main = "Mean Daily Activity Pattern", xlab = "Day in minutes", ylab = "Average steps across all days")
plot(names(mean_weekends_activity_pattern), mean_weekends_activity_pattern, type = "l", main = "Mean Daily Activity Pattern", xlab = "Day in minutes", ylab = "Average steps across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
