# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First we read the data in and assign it to a data frame called data:



```r
data <- read.csv("C:\\Users\\gilbert\\Documents\\activity.csv")
```



## What is mean total number of steps taken per day?

Next, we want a histogram of the total number of steps taken each day.  To get this we first must get a count of the total number of steps taken for each day in the data set.  This can be done using tapply to sum the steps for each day (unique date) in the data frame.  Results are saved in the variable daySteps:



```r
daySteps <- tapply(data$steps,data$date,sum,na.rm=T)
```


Next we make a histogram of the steps taken per day.  The number of breaks is set to 10 since the default of 5 gave bins that were a little wide:


```r
hist(daySteps,breaks = 10,xlab="Steps per Day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Now we calculate the mean and median steps taken per day:


```r
mean(daySteps)
```

```
## [1] 9354.23
```

```r
median(daySteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

To get the average number of steps taken in each interval, across all days, we again use tapply to take the mean for each interval and store the results in avgSteps:


```r
 avgSteps <- tapply(data$steps,data$interval,mean,na.rm=T)
```

Here is the time series plot:


```r
interval <- names(avgSteps)
plot(interval,avgSteps,type="l")
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

To find which interval contains the maximum number of steps on average, we combine the which function and max function:


```r
avgSteps[which(avgSteps==max(avgSteps))]
```

```
##      835 
## 206.1698
```

## Imputing missing values

The number of rows with NA values can be found using the complete.cases function:


```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

We will impute the mean of the interval for any missing values of steps.  This will be done using function from the dplyr library, which we load first.  Then we write a function, impMean, to replace missing values with means. Finally, we create a new dataset using this function and tools from dplyr:


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked _by_ '.GlobalEnv':
## 
##     count
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
impMean <- function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x)
data2 <- data %>% group_by(interval) %>% mutate_each(funs(impMean),steps)
head(data2)
```

```
## Source: local data frame [6 x 3]
## Groups: interval
## 
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Here is the histogram for the new dataset:

```r
daySteps2 <- tapply(data2$steps,data$date,sum,na.rm=T)
hist(daySteps2,breaks = 10,xlab="Steps per Day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

and the mean and median:


```r
mean(daySteps2)
```

```
## [1] 10766.19
```

```r
median(daySteps2)
```

```
## [1] 10766.19
```

Clearly, the histogram and the mean and median values are different for the two datasets.  Both the mean and median increased by replacing the missing values with the average of the interval.  Also, the histogram is much more centered, with more mass around the average value, as would be expected with this imputation scheme.
## Are there differences in activity patterns between weekdays and weekends?

For the following we will use the isWeekday function from the library timeDate. We create a variable day that is a factor, being FALSE for weekend days and TRUE for weekdays.  The levels are then changed to weekend and weekday:


```r
library(timeDate)
```

```
## Warning: package 'timeDate' was built under R version 3.1.2
```

```r
data2$day <- as.factor(isWeekday(as.Date(data$date)))
levels(data2$day) <- c("weekend","weekday")
head(data2)
```

```
## Source: local data frame [6 x 4]
## Groups: interval
## 
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

Next, for the panel plot, we use the lattice library as well as some dplyr commands.  We first create a new dataset from data2 which summarizes the average number of steps by interval and day.  This new dataset is fed into xyplot to create the panel plot.


```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.2
```

```r
data3 <- data2 %>% group_by(interval,day) %>% summarise(avg = mean(steps))
xyplot(data3$avg~data3$interval|data3$day,type="l")
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

