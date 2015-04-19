# Reproducible Research
# By MAVC 18/04/2015

library(ggplot2)
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
activity$interval <- (activity$interval%/%100)*60 + activity$interval%%100

total_steps_per_day <-sapply(split(activity$steps, f=activity$date, drop = FALSE), sum, na.rm = TRUE)
hist(total_steps_per_day, breaks = 20, main = "Total number of steps per Day", xlab = "Number of Steps")
mean_steps_per_day <-sapply(split(activity$steps, f=activity$date, drop = FALSE), mean, na.rm = TRUE)
median_steps_per_day <-sapply(split(activity$steps, f=activity$date, drop = FALSE), median, na.rm = TRUE)
summary_analysis <- data.frame(names(total_steps_per_day),total_steps_per_day, mean_steps_per_day,median_steps_per_day)
summary_analysis[,1] <- as.Date(summary_analysis[,1])
colnames(summary_analysis)[1] <- "date"

mean_daily_activity_pattern <-sapply(split(activity$steps, f=activity$interval, drop = FALSE), mean, na.rm = TRUE)
plot(names(mean_daily_activity_pattern), mean_daily_activity_pattern, type = "l", main = "Mean Daily Activity Pattern", xlab = "Day in minutes", ylab = "Average steps across all days")
maximun_activity_interval <- which.max(mean_daily_activity_pattern)

number_NA <- sum(is.na(activity))
original_activity <- activity
most_common_number_steps <- which.max(table(activity$steps))
activity[is.na(activity)] <- as.numeric(names(most_common_number_steps))
total_steps_per_day_noNA <-sapply(split(activity$steps, f=activity$date, drop = FALSE), sum, na.rm = TRUE)
hist(total_steps_per_day_noNA, breaks = 20, main = "Total number of steps per Day", xlab = "Number of Steps")
mean_steps_per_day_noNA <-sapply(split(activity$steps, f=activity$date, drop = FALSE), mean, na.rm = TRUE)
median_steps_per_day_noNA <-sapply(split(activity$steps, f=activity$date, drop = FALSE), median, na.rm = TRUE)

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