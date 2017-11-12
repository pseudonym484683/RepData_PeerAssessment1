Read CSV File

    library(lattice)
    #Read CSV
    monitor_activity <- read.csv ("activity.csv", header = T, sep = ",",na.strings = "NA" ,stringsAsFactors = F)

What is mean total number of steps taken per day?
=================================================

    #Calculate the total number of steps taken per day and make plot
    monitor_activity$date <- as.Date(monitor_activity$date, "%Y-%m-%d")
    monitor_activity_nafilter <- monitor_activity[!is.na(as.character(monitor_activity$steps)),]
    perdaysteps <- aggregate(steps ~ date, data = monitor_activity_nafilter, sum)
    colnames(perdaysteps) <- c("date", "no_of_steps")

    hist(as.numeric(perdaysteps$no_of_steps),breaks = 30,  xlab = "Number of Steps", main= "Total number of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Calculate the mean and median

    mean(perdaysteps$no_of_steps)

    ## [1] 10766.19

    median(perdaysteps$no_of_steps)

    ## [1] 10765

What is the average daily activity pattern?
===========================================

    perdaysteps_mean <- aggregate(steps ~ interval, data = monitor_activity_nafilter, mean)
    colnames(perdaysteps_mean) <- c("interval", "mean_steps")
    plot(mean_steps ~ interval, data = perdaysteps_mean, type = "l", xlab = "Intervals", ylab = "Mean number ", main = "Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    max_steps <- max(perdaysteps_mean$mean_steps)
    max_steps

    ## [1] 206.1698

Imputing missing values
=======================

    #total number of missing values in the dataset-steps
    sum(is.na(as.character(monitor_activity$steps)))

    ## [1] 2304

    #total number of missing values in the dataset-date
    sum(is.na(as.character(monitor_activity$date)))

    ## [1] 0

    #total number of missing values in the dataset-intervals
    sum(is.na(as.character(monitor_activity$interval)))

    ## [1] 0

    na_fill_activity <- monitor_activity

    # missing na fill with mean
    flag = 0
    for (i in 1:nrow(na_fill_activity)) {
      if (is.na(na_fill_activity[i,"steps"])) {
        intv <- na_fill_activity[i,"interval"]
        #value_mean <- subset(perdaysteps_mean, interval==index)
        value_mean <- perdaysteps_mean[perdaysteps_mean$interval==intv,"mean_steps"]
        na_fill_activity[i,"steps"] <- value_mean
        flag = flag + 1
      }
    }

    perdaysteps1 <- aggregate(steps ~ date, data = na_fill_activity, sum)
    colnames(perdaysteps1) <- c("date", "no_of_steps")
    hist(as.numeric(perdaysteps1$no_of_steps),breaks = 30,  xlab = "Number of Steps", main= "Total number of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    mean(perdaysteps1$no_of_steps)

    ## [1] 10766.19

    median(perdaysteps1$no_of_steps)

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

    for (i in 1:nrow(na_fill_activity))
    {
      if(as.POSIXlt(as.Date(na_fill_activity[i,"date"]))$wday%%6 == 0)
      {
        na_fill_activity[i,"day"] <- "weekday"
      }
      else
      {
        na_fill_activity[i,"day"] <- "weekend"
      }
      
    }

    na_fill_activity$day <- factor(na_fill_activity$day, levels = c("weekday", "weekend"))
    weekday_weekday = aggregate(steps ~ interval + day, na_fill_activity, mean)

    xyplot(steps ~ interval | factor(day), data = weekday_weekday, aspect = 1/2, type = "l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)
