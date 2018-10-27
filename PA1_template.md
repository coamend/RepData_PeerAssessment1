---
title: "Step Monitor Activity Analysis"
author: "Corey Amend"
date: "October 22, 2018"
output: 
    html_document:
        keep_md: yes
---



## Loading and preprocessing the data

First we will download the activity data, unzip it, and read the data in as a 
list of comma-separated values.


```r
zipurl<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(zipurl, 'activity.zip')

unzip("activity.zip", overwrite=TRUE)
raws<-read.csv("activity.csv")
```


## What is mean total number of steps taken per day?


```r
total_steps<-tapply(raws$steps, raws$date, sum, na.rm=TRUE)
total_steps<-total_steps[complete.cases(total_steps)]
total_steps<-data.frame(total_steps)
hist(total_steps$total_steps, xlab="Total Steps", breaks=10, 
     main="Total steps per day frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


### What is the mean/median steps per day?


```r
med_steps<-median(total_steps$total_steps, na.rm=TRUE)
mean_steps<-mean(total_steps$total_steps, na.rm=TRUE)
sum_steps<-sum(total_steps$total_steps, na.rm=TRUE)
```

The median number of steps per day is: 10395

The mean number of steps per day is: 9354.2295082

Total steps: 570608


## What is the average daily activity pattern?


```r
interval_steps<-aggregate(raws$steps, by=list(raws$interval), 
                          mean, na.rm=TRUE, simplify = FALSE)
names(interval_steps)<-c("interval", "steps")
interval_steps$steps<-as.numeric(interval_steps$steps)
qplot(x=interval,
      y=steps,
      data=interval_steps, 
      ylab="Mean steps", xlab="Interval", geom="line")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
max_step_interval<-interval_steps[
    interval_steps$steps==max(interval_steps$steps),1]
```

The interval with the maximum average number of steps is: 835


## Fill in missing measurements

Now replace the missing measurements in the data with the average values.


```r
complete_data<-raws
missing_data<-sum(is.na(raws))
complete_data[is.na(complete_data$steps),"steps"]<-mean(raws$steps, na.rm=TRUE)
total_complete_steps<-aggregate(complete_data$steps, 
                                by=list(complete_data$date), sum)
total_complete_steps<-data.frame(total_complete_steps)
names(total_complete_steps)<-c("date", "steps")
hist(total_complete_steps$steps, xlab="Total Steps", breaks=10, 
     main="Total steps per day frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

The number of observations missing a value are: 2304


### What is the mean/median steps per day with the imputed values?


```r
complete_med_steps<-median(total_complete_steps$steps, na.rm=TRUE)
complete_mean_steps<-mean(total_complete_steps$steps, na.rm=TRUE)
complete_sum_steps<-sum(total_complete_steps$steps, na.rm=TRUE)
```

In the imputed data set, the median number of steps per day is: 
1.0766189\times 10^{4}

The difference in the imputed versus originally calculated median number of 
steps per day is: 371.1886792

In the imputed data set, the mean number of steps per day is: 
1.0766189\times 10^{4}

The difference in the imputed versus originally calculated mean number of steps
per day is: 1411.959171

Total imputed steps: 6.5673751\times 10^{5}

The difference in the imputed versus originally calculated total number of steps
per day is: 8.6129509\times 10^{4}


### Examine weekday/weekend differences


```r
weekday_complete_steps<-cbind(complete_data, 
                            weekday=weekdays(
                                as.POSIXlt(complete_data$date)))
my_ts<-qplot(interval, steps, data=weekday_complete_steps, geom="line")
my_ts + facet_wrap(~factor(
    ifelse(grepl("S(at|un)", weekday), "Weekend", "Weekday")), 
    ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
