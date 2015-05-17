# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
analysis <- read.csv("activity.csv", colClasses = c("numeric", "character","numeric"))
head(analysis)
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

```r
library(lattice)
analysis$date <- as.Date(analysis$date, "%Y-%m-%d") ##make it a date type!
```

## What is mean total number of steps taken per day?



```r
TotalSteps <- aggregate(steps ~ date, data = analysis, sum,
na.rm = TRUE)
```

The histogram of the total number of steps taken each day is:


```r
hist(TotalSteps$steps, main = "Total steps by day", xlab =
"day", col = "red")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The mean is:

```r
mean(TotalSteps$steps)
```

```
## [1] 10766.19
```

The median is:

```r
median(TotalSteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5minute interval (xaxis) and the average
number of steps taken, averaged across all days (yaxis)

Get the mean of steps and time series plot

```r
timeSeries <- tapply(analysis$steps, analysis$interval,
mean, na.rm = TRUE)
```

The time series plot is:

```r
plot(row.names(timeSeries), timeSeries, type = "l", xlab= "5-min interval",
ylab = "Average across all Days", main = "Average number of steps taken", col = "red")
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

Which 5minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxSteps <- which.max(timeSeries)
names(maxSteps)
```

```
## [1] "835"
```
## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
activityNA <- sum(is.na(analysis))
activityNA
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

NA is replaced by the mean in 5 min interval

```r
stepsAverage <- aggregate(steps ~ interval, data = analysis, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(analysis)) {
obs <- analysis[i, ]
if (is.na(obs$steps)) {
steps <- subset(stepsAverage, interval ==
obs$interval)$steps
} else {
steps <- obs$steps
}
fillNA <- c(fillNA, steps)
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newWithNA <- analysis
newWithNA$steps <- fillNA
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsTotalNA <- aggregate(steps ~ date, data = newWithNA,sum, na.rm = TRUE)
```
The Histogram is

```r
hist(stepsTotalNA$steps, main = "Total steps by day", xlab ="day", col = "blue")
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

And the mean and median this time are:

```r
mean(stepsTotalNA$steps)
```

```
## [1] 10766.19
```

```r
median(stepsTotalNA$steps)
```

```
## [1] 10766.19
```
After replacing the NA values with the mean, the values (obviously) has differed
## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
day <- weekdays(analysis$date)
daylevel <- vector()

for (i in 1:nrow(analysis)) 
{
    if (day[i] == "Saturday") 
    {
        daylevel[i] <- "Weekend"
    } 
    else if (day[i] == "Sunday") 
    {
        daylevel[i] <- "Weekend"
    } 
    else 
    {
        daylevel[i] <- "Weekday"
    }
} 
analysis$daylevel <- daylevel
analysis$daylevel <- factor(analysis$daylevel)
dayType <- aggregate(steps ~ interval + daylevel, data = analysis, mean)
names(dayType) <- c("interval", "daylevel", "steps")
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
xyplot(steps ~ interval | daylevel, dayType, type = "l",layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-16-1.png) 











