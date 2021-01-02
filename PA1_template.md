---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
The data we're going to analyze is included in the repository. First, we are going to unzip the file using the following code:

```r
unzip("activity.zip")
```
Then, we are going to read de resulting csv file. We've chosen to assign the result to a new variable ("data") to its better management.

```r
data<- read.csv("activity.csv")
```
Cheking the dataset...

```r
head(data)
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
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Ok!

## What is mean total number of steps taken per day?
First, we need to calculate the total number of steps per day (specific date).

```r
 stepsday <- aggregate(steps ~ date, data, sum, na.rm=TRUE)
```
We can plot the frequency of steps per day with the following code for a histogram:

```r
hist(stepsday$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Then, we'd like to get the mean and the median of steps per day. We've created a new variable for each one and printed them using the following code:

```r
meanstepsday<-mean(stepsday$steps)
meanstepsday
```

```
## [1] 10766.19
```

```r
medianstepsday<-median(stepsday$steps)
medianstepsday
```

```
## [1] 10765
```

## What is the average daily activity pattern?
To get the average daily activity pattern we are going to aggregate the steps and interval variables into a new variable stepsinterval, then plot their relation.

```r
stepsinterval<-aggregate(steps~interval, data=data, mean, na.rm=TRUE)
plot(steps~interval, data=stepsinterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

To learn which 5-minute interval on average across all the days in the dataset contains the maximum number of steps we use:

```r
intervalmaxsteps <- stepsinterval[which.max(stepsinterval$steps),]$interval
intervalmaxsteps
```

```
## [1] 835
```

## Imputing missing values
The code to count the number of missing values (NAs) in the variable steps is:

```r
totalNA <- sum(is.na(data$steps))
totalNA
```

```
## [1] 2304
```
One way of filling in the missing values of the dataset is to use the mean of the interval. We've created a function that gives the mean given the interval.

```r
meanstepsinterval<-function(interval){
    stepsinterval[stepsinterval$interval==interval,]$steps
}
```
Then, we use it on the dataset to fill the missing values.

```r
data2<-data
for(i in 1:nrow(data2)){
    if(is.na(data2[i,]$steps)){
        data2[i,]$steps <- meanstepsinterval(data2[i,]$interval)
    }
}
```
Checking the new dataset...

```r
head(data2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
str(data2)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
It worked!

With the new dataset, we can recreate the plot showing the frequency of steps per day.

```r
stepsday2 <- aggregate(steps ~ date, data=data2, sum)
hist(stepsday2$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Then, we can recalculate the mean and median.

```r
meanstepsday2<-mean(stepsday2$steps)
meanstepsday2
```

```
## [1] 10766.19
```

```r
medianstepsday2<-median(stepsday2$steps)
medianstepsday2
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?
To discern the differences in activity patterns between weekdays and weekends, we are going to separate the data in those new two different levels and record it on a new variable stepsweekpattern.

```r
data2$date <- as.Date(strptime(data2$date, format="%Y-%m-%d"))
data2$day <- weekdays(data2$date)
for (i in 1:nrow(data2)) {
    if (data2[i,]$day %in% c("Saturday","Sunday")) {
        data2[i,]$day<-"weekend"
    }
    else{
        data2[i,]$day<-"weekday"
    }
}
stepsweekpattern <- aggregate(data2$steps ~ data2$interval + data2$day, data2, mean)
```
We are going to assing names to the new columns and plot the relation between steps and interval for both weekend days and weekend days. We are able to plot them side by side using the lattice package.

```r
names(stepsweekpattern) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsweekpattern, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
