unzip("activity.zip")
data <- read.csv("activity.csv")
#data <- data[complete.cases(data), 1:2]
#data <- data[, 1:2]
library(reshape2)
melted_data <- melt(data, na.rm = TRUE, id.vars="date")
tdset <- dcast(melted_data, date ~ variable, sum)
tdset$date <- as.Date(tdset$date)
library("ggplot2")
#p <- ggplot(tdset, aes(x=date, y=steps)) + geom_bar(stat="identity", colour="white")
p <- ggplot(tdset, aes(x=steps)) + geom_histogram(colour="white")
p + labs(title='Histogram of number of steps taken daily\n',
       y='Count', x='Steps')
mean_steps <- mean(tdset$steps)
median_steps <- median(tdset$steps)

melted_data2 <- melt(data[, c("steps", "interval")], na.rm = TRUE, id.vars="interval")
tdset2 <- dcast(melted_data2, interval ~ variable, mean)
p1 <- ggplot(tdset2, aes(x=interval, y=steps)) + geom_line()
p1 + labs(title='Time series plot of the 5-minute interval',
         y='Average number of steps taken', x='Interval')

maxsteps <- tdset2[which.max(tdset2$steps), ]$interval
totalmissing <- sum(is.na(data))

# Merge the replacement values
# Use previously calculated means
tdset2$imputed_steps <- floor(tdset2$steps)

imputed_data <- merge(data, tdset2[,c('interval', 'imputed_steps')], by='interval')

imputed_data$steps <- ifelse(is.na(imputed_data$steps),
                                 imputed_data$imputed_steps,
                                 imputed_data$steps)
imputed_data$imputed_steps <- NULL
sum(is.na(imputed_data))
imputed_data$date <- as.Date(imputed_data$date)

melted_data3 <- melt(imputed_data, id.vars="date")
daily_imputed_data <- dcast(melted_data3, date ~ variable, sum)

mean_steps_imputed <- mean(daily_imputed_data$steps)
median_steps_imputed <- median(daily_imputed_data$steps)

# Replace the data in the original histogram with the imputed data
p %+% daily_imputed_data +
  labs(title='Number of steps taken each day,\nafter imputing missing values')

# Label each date as weekday/weekend 
imputed_data$wday <- weekdays(as.Date(imputed_data$date))
# Replacing the names of week day with 'Weekday' and 'Weekend':
imputed_data$wday[imputed_data$wday == "Friday"] <- "Weekday"
imputed_data$wday[imputed_data$wday == "Monday"] <- "Weekday"
imputed_data$wday[imputed_data$wday == "Thursday"] <- "Weekday"
imputed_data$wday[imputed_data$wday == "Tuesday"] <- "Weekday"
imputed_data$wday[imputed_data$wday == "Wednesday"] <- "Weekday"
imputed_data$wday[imputed_data$wday == "Saturday"] <- "Weekend"
imputed_data$wday[imputed_data$wday == "Sunday"] <- "Weekend"
table(imputed_data$wday)
imputed_data$date <- NULL
melted_data4 <- melt(imputed_data, id.vars=c("interval", "wday"))
imputed_data2 <- dcast(melted_data4, interval+wday ~ variable, mean)

# Plot the average steps per interval, given the week_part
p1 %+% imputed_data2 + facet_grid(wday~.) +
  labs(title='Time series plot of the 5-minute interval, \nafter imputing missing values')

