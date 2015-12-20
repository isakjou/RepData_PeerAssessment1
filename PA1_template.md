Reproducible Research
=====================

# Peer Assessment 1



## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

Download the data in a Windows 10 computer.


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileZip <- "data_activity.zip"
file <- "activity.csv"
#Download file if not exists in the work directory
if(!file.exists(fileZip)
   & !file.exists(file)){
  #In windows 10, mode="wb" for avoid problems in the download
  download.file(fileUrl, fileZip, mode = "wb")
}
#Unzip file if it is zipped
if(!file.exists(file) 
   & file.exists(fileZip)){
  unzip(fileZip)
}
```

## Assignment

This assignment will be described in multiple parts. You will need to
write a report that answers the questions detailed below. Ultimately,
you will need to complete the entire assignment in a **single R
markdown** document that can be processed by **knitr** and be
transformed into an HTML file.

Throughout your report make sure you always include the code that you
used to generate the output you present. When writing code chunks in
the R markdown document, always use `echo = TRUE` so that someone else
will be able to read the code. **This assignment will be evaluated via
peer assessment so it is essential that your peer evaluators be able
to review the code for your analysis**.

For the plotting aspects of this assignment, feel free to use any
plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the [GitHub repository created for this
assignment](http://github.com/rdpeng/RepData_PeerAssessment1). You
will submit this assignment by pushing your completed files into your
forked repository on GitHub. The assignment submission will consist of
the URL to your GitHub repository and the SHA-1 commit ID for your
repository state.

NOTE: The GitHub repository also contains the dataset for the
assignment so you do not have to download the data separately.



### Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. `read.csv()`)

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
datos <- read.csv(file, header=TRUE,na.strings="NA")
```

## What is mean total number of steps taken per day

For this part of the assignment, you can ignore the missing values in
the dataset.

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
totalStepsPerDay <- aggregate(steps ~ date, data = datos, FUN = sum)

hist(totalStepsPerDay$steps,breaks=10,main="Total Steps Per Day", xlab=NA, col="gray")
```

![plot of chunk histrogramSteps](figure/histrogramSteps-1.png) 


```r
meanTSPD <- prettyNum(mean(totalStepsPerDay$steps))
```

**Mean** of total steps per day **10766.19**


```r
medianTSPD <- prettyNum(median(totalStepsPerDay$steps))
```

**Median** of total steps per day **10765**

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageStepsPerInterval <- aggregate(steps ~ interval, data = datos, FUN = mean)

plot(averageStepsPerInterval$interval,averageStepsPerInterval$steps,type="l", main="Average Steps per Interval of 5 minutes",xlab ="Interval",ylab="Average")
```

![plot of chunk timeSeriesAverageSteps](figure/timeSeriesAverageSteps-1.png) 


```r
maxInterval <- averageStepsPerInterval[which(averageStepsPerInterval$steps ==  max(averageStepsPerInterval$steps)),1]
```

The 5-minute interval with the **maximum average** number of steps is **835**

## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

2. Devise a strategy for filling in all of the missing values in the dataset.
The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate
and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
rowsWithNA <- sum(is.na(datos$steps))
```

**Total** number of rows with `NA`s is **2304**

My strategy for filling in all of the missing values in the dataset will use the
average between the means of the day and of the interval.


```r
datosWithoutNA = datos

for (i in 1:dim(datosWithoutNA)[1]){
  if( is.na(datosWithoutNA[i,1])){
    perDayValue <- totalStepsPerDay[which(totalStepsPerDay$date == as.character(datosWithoutNA[i,2])),2]
    if(length(perDayValue) == 0){
      perDayValue = 0
    }else{
      perDayValue = perDayValue[1]
    }
    
  perIntervalValue <- averageStepsPerInterval[ which(averageStepsPerInterval$interval == as.character(datosWithoutNA[i,3])),2]
    if(length(perIntervalValue) == 0){
      perIntervalValue = 0
    }else{
      perIntervalValue = perIntervalValue[1]
    }
  
    datosWithoutNA[i,1] = mean(c(perDayValue, perIntervalValue))
  }
}
```


```r
totalStepsPerDayWithoutNA <- aggregate(steps ~ date, data = datosWithoutNA,
                                       FUN = sum)

hist(totalStepsPerDayWithoutNA$steps,breaks=10,main="Total Steps Per Day *", xlab=NA, col="gray")
```

![plot of chunk histrogramStepsWithoutNA](figure/histrogramStepsWithoutNA-1.png) 


```r
meanTSPDWithoutNA <- prettyNum(mean(totalStepsPerDayWithoutNA$steps))
meanDifference <- prettyNum(as.numeric(meanTSPDWithoutNA) -
                              as.numeric(meanTSPD))
```

**Mean** of total steps per day **10060.21**  
The difference with the mean of data with `NA` is -705.98


```r
medianTSPDWithoutNA <- prettyNum(median(totalStepsPerDayWithoutNA$steps))
medianDifference <- prettyNum(as.numeric(medianTSPDWithoutNA) -
                                as.numeric(medianTSPD))
```

**Median** of total steps per day **10395**  
The difference with the median of data with `NA` is -370

## Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis).


```r
isWeekend <- function(x){
  retorno <- "weekday"
  if(x == "Sunday" | x == "Saturday") {
    retorno = "weekend"
  }
  return(retorno)
}

datosWithoutNA$day <- as.factor(apply(datosWithoutNA, 1, FUN = function(x) isWeekend(weekdays(as.Date(x[2])))))
```


```r
intervalWeekday <- aggregate(steps ~ interval, data = datosWithoutNA[ which(datosWithoutNA$day == "weekday"),1:3], FUN = mean)
intervalWeekday$day <- "Weekday"

intervalWeekend <- aggregate(steps ~ interval, data = datosWithoutNA[ which(datosWithoutNA$day == "weekend"),1:3], FUN = mean)
intervalWeekend$day <- "Weekend"

all <- rbind(intervalWeekend, intervalWeekday)

library(ggplot2)
qplot(interval,steps,data=all,facets=day~.,geom="line",xlab="5-minute interval",ylab="Average steps")
```

![plot of chunk plotWeekday](figure/plotWeekday-1.png) 
