# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Download the data if it doesn't exist already

```r
setwd("/Users/eldon/ownCloud/School/Reproducible Research/project1")

if (!file.exists("activity.csv")) {
     download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile="repdata_data_activity.zip", method="curl")
     unzip("repdata_data_activity.zip")
}
```

Read in the data


```r
activitydata <- read.csv("activity.csv")
```

Document date and time of download as well as dimensions and summary of the dataset in case there are future changes

```r
date()
```

```
## [1] "Sun Aug 17 10:49:10 2014"
```

```r
dim(activitydata)
```

```
## [1] 17568     3
```

```r
summary(activitydata)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```


## What is mean total number of steps taken per day?
Mean steps per day

```r
mean(aggregate(steps ~ date, data=activitydata, FUN=sum)$steps)
```

```
## [1] 10766
```
Median steps per day

```r
median(aggregate(steps ~ date, data=activitydata, FUN=sum)$steps)
```

```
## [1] 10765
```
Histogram of total number of steps per day

```r
hist(aggregate(steps ~ date, data=activitydata, FUN=sum)$steps, main="Total Steps per Day", xlab='')
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

## What is the average daily activity pattern?
Average steps per interval averaged across all days:

```r
plot(aggregate(steps~interval, data=activitydata, FUN=mean)$interval, aggregate(steps~interval, data=activitydata, FUN=mean)$steps, type="l", xlab="Interval", ylab="Average Steps")  
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Which 5 minute interval on average contains the max number of steps?  

```r
activitydata$interval[which.max(aggregate(steps~interval, data=activitydata, FUN=mean)$steps)]
```

```
## [1] 835
```


## Imputing missing values
Total missing values in dataset: 

```r
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```

Create new dataset and fill missing steps with the mean of the interval

```r
ad2 <- activitydata
intervalmeans <- aggregate(steps~interval, data=activitydata, FUN=mean)

for (i in 1:length(ad2$steps)) {
     if (is.na(ad2$steps[i])) {
          ad2$steps[i] <- intervalmeans[(intervalmeans$interval == ad2$interval[i]), 2]
     }     
}     
```
Compare the new dataset to the original
Mean steps per day after imputing missing values

```r
mean(aggregate(steps ~ date, data=ad2, FUN=sum)$steps)
```

```
## [1] 10766
```
Median steps per day after imputing missing values

```r
median(aggregate(steps ~ date, data=ad2, FUN=sum)$steps)
```

```
## [1] 10766
```
Histogram of total number of steps per day after imputing missing values

```r
hist(aggregate(steps ~ date, data=ad2, FUN=sum)$steps, main="Total Steps per Day", xlab='')
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 


## Are there differences in activity patterns between weekdays and weekends?
Create a new factor to subset weekend from weekday and plot them together

```r
ad2$weekday <- ifelse(!weekdays(strptime(ad2$date, format="%Y-%m-%d")) %in% c('Saturday','Sunday'), "Weekday","Weekend")

table(ad2$weekday)
```

```
## 
## Weekday Weekend 
##   12960    4608
```

```r
par(mfrow=c(2,1))

plot(aggregate(steps~interval, data=ad2[(ad2$weekday=="Weekday"),], FUN=mean)$interval, aggregate(steps~interval, data=ad2[(ad2$weekday=="Weekday"),], FUN=mean)$steps, type="l", xlab="Interval", ylab="Average Steps", main="Weekday")

plot(aggregate(steps~interval, data=ad2[(ad2$weekday=="Weekend"),], FUN=mean)$interval, aggregate(steps~interval, data=ad2[(ad2$weekday=="Weekend"),], FUN=mean)$steps, type="l", xlab="Interval", ylab="Average Steps", main="Weekend")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 
