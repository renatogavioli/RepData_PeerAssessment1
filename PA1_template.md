---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
The data is loaded from the file contained in the .zip file, using the *unzip* then the *read.csv* functions. 


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

The total number of steps per day is determined using the *aggregate* function, and an histogram is shown for the total number of steps taken per day.


```r
stepsPerDay <- aggregate(data$steps, by=data[2], sum)
names(stepsPerDay)[2]<- "sumSteps"
hist(stepsPerDay$sumSteps, xlab= "Total number of steps per day", breaks=8)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

The mean and median total number of steps per day is determined using the *summary* function.


```r
summary(stepsPerDay$sumSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

## What is the average daily activity pattern?

The daily activity pattern is determined by the mean number of steps per time interval.


```r
stepsPerInterval <- aggregate(data$steps, by=data[3], sum, na.rm=TRUE)
stepsPerInterval[[2]]<- stepsPerInterval[[2]]/length(stepsPerDay$date)
names(stepsPerInterval)[2]<- "meanSteps"
plot(stepsPerInterval$interval, stepsPerInterval$meanSteps, type="l", xlab="5-minute interval", ylab="Mean number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

## Imputing missing values

There are some NA values in the original dataset, whose quantity can be determined:


```r
table(complete.cases(data))
```

```
## 
## FALSE  TRUE 
##  2304 15264
```

The mean number of steps per interval will be used to fill in the missing values.


```r
missingValues<- merge(data[which(!complete.cases(data)),], stepsPerInterval)
missingValues<- data.frame("steps"=missingValues$meanSteps, "date"=missingValues$date, "interval"=missingValues$interval)
dataFixed<- rbind(data[complete.cases(data),], missingValues)
dataFixed<- dataFixed[order(dataFixed$date, dataFixed$interval),]
```

The additional data causes some changes to the analytic data, shown in the histogram and the summary below.


```r
stepsPerDayFixed <- aggregate(dataFixed$steps, by=dataFixed[2], sum)
names(stepsPerDayFixed)[2]<- "sumSteps"
hist(stepsPerDayFixed$sumSteps, xlab= "Total number of steps per day", breaks=8)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
summary(stepsPerDayFixed$sumSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9354   10400   10580   12810   21190
```

## Are there differences in activity patterns between weekdays and weekends?

First, the dataset is processed - a factor variable is created discerning "weekends" and "weekdays".


```r
dataFixed$date<- as.Date(dataFixed$date)
dataFixed$weekday<- strftime(dataFixed$date, format="%u")
dataFixed$isWeekday<- dataFixed$weekday %in% c(6,7)
dataFixed$isWeekday<- factor(dataFixed$isWeekday, levels=c(TRUE, FALSE), labels=c("weekend", "weekday"))
```

Then, the activity pattern (mean number of steps as function of the time interval) is plotted according to the following code.


```r
stepsPerIntervalWeekday <- aggregate(dataFixed$steps, by=c(dataFixed[3], dataFixed[5]), sum)
stepsPerIntervalWeekday[[3]]<- stepsPerIntervalWeekday[[3]]/length(stepsPerDay$date)
names(stepsPerIntervalWeekday)[3]<- "meanSteps"
par("mfcol"=c(2,1))
plot(stepsPerIntervalWeekday[stepsPerIntervalWeekday$isWeekday %in% "weekend", 1], stepsPerIntervalWeekday[stepsPerIntervalWeekday$isWeekday %in% "weekend",3], type="l", xlab="5-minute interval", ylab="Mean number of steps", main="Mean step pattern - Weekends")
plot(stepsPerIntervalWeekday[stepsPerIntervalWeekday$isWeekday %in% "weekday", 1], stepsPerIntervalWeekday[stepsPerIntervalWeekday$isWeekday %in% "weekday",3], type="l", xlab="5-minute interval", ylab="Mean number of steps", main="Mean step pattern - Weekdays")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

Results show that usually during weekdays, the number of steps tend to be higher, while during weekends the rest periods are probably also longer, thus reflecting a smaller number of steps.
