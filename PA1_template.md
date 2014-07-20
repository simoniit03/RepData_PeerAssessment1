# Reproducible Research: Peer Assessment 1
by Touhidur


## Loading and preprocessing the data


```r
# Unziping the souce file
unzip("activity.zip")
# Reading and loading data from source file
data.activity<-read.csv("Activity.csv")
```


## What is mean total number of steps taken per day?


```r
# Calculating the total number of steps taken per day
total.steps.per.day<-aggregate(steps~date, data=data.activity, sum)
# Making a histogram of total number of steps taken per day
hist(total.steps.per.day$steps,xlab="Total number of steps per day", main="Histogram of total number of steps taken per day")
```

![plot of chunk histogram](figure/histogram.png) 

```r
# Calculating the mean and median for total number of steps taken per day
mean(total.steps.per.day$steps)
```

```
## [1] 10766
```

```r
median(total.steps.per.day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
# Calculating the average number of steps taken per 5-minute interval accross all day
interval.mean<-aggregate(steps~interval, data=data.activity, FUN=mean)

# Making a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
plot(interval.mean,type="l",xlab="5-minute interval",ylab="Average number of steps taken",
     main="Time series plot of the 5-minute interval & average no of steps taken")
```

![plot of chunk timeseries_plot](figure/timeseries_plot.png) 

```r
# 5-minute interval that on average across all the days in the dataset, 
# contains the maximum number of steps
interval.mean$interval[which.max(interval.mean$steps)]
```

```
## [1] 835
```

## Imputing missing values
#### 1. Calculating the total number of missing values in the dataset

```r
nrow(subset(data.activity,is.na(data.activity)))
```

```
## [1] 2304
```

#### 2. My strategy is to fill in all of the missing values in the dataset using the mean for that 5-minute interval

#### 3. Creating a new dataset that is equal to the original dataset but with the missing data filled in

```r
# Merging original dataset with 5-minute interval average
data.activity.imputed<-merge(data.activity,interval.mean, by="interval",suffixes=c("",".y"))

# Finding the index for NA's
index.nas<-  is.na(data.activity.imputed$steps)

# Filling NA's with mean for that 5-minute interval
data.activity.imputed$steps[index.nas]<-data.activity.imputed$steps.y[index.nas]

# Removing additional variables / columns
data.activity.imputed<- data.activity.imputed[,c(1:3)]      
```

#### 4.a. Making a histogram of the total number of steps taken each day (using the imputed data frame). 


```r
# Calculating the total number of steps taken each day
imputed.total.steps.per.day<-aggregate(steps~date, data=data.activity.imputed, FUN=sum)

# Making the histogram
plot(imputed.total.steps.per.day$steps, type="h", xlab="Total no of steps each day", ylab="Frequency",
     main="Histogram of total number of steps taken each day")
```

![plot of chunk histogram_with_imputed_data](figure/histogram_with_imputed_data.png) 

#### 4.b. Calculating the mean and median for the total number of steps taken per day (imputed data vs original data).


```r
#imputed datasets
mean(imputed.total.steps.per.day$steps)
```

```
## [1] 10766
```

```r
median(imputed.total.steps.per.day$steps)
```

```
## [1] 10766
```

```r
#original datasets
mean(total.steps.per.day$steps)
```

```
## [1] 10766
```

```r
median(total.steps.per.day$steps)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# calculating the type of the day using the following user defined function. i.e, weekday and weekend
days <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        return("weekend")
  } 
  else {
        return("weekday")
  }
}
   
# Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.
data.activity.imputed$days <- as.factor(sapply(data.activity.imputed$date,days))

# Subsetting data for weekdays and weekend
weekdays.interval.mean<-aggregate(steps~interval, data=data.activity.imputed,
                                    subset=(data.activity.imputed$days=="weekday"), FUN=mean)
weekend.interval.mean<-aggregate(steps~interval, data=data.activity.imputed,
                                    subset=(data.activity.imputed$days=="weekend") ,FUN=mean)

# Making a panel plot containing a time series plot
par(mfrow=c(2,1))
plot(weekdays.interval.mean,type="l", main="Weekdays", ylab="Average number of steps taken" ,xlab="5-minute interval") 
plot(weekend.interval.mean,type="l", main="Weekend" , ylab="Average number of steps taken" ,xlab="5-minute interval") 
```

![plot of chunk timeseries_plot_with_imputed_data](figure/timeseries_plot_with_imputed_data.png) 

