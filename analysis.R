library(ggplot2)

ensureActivityData <- function(srcFileName = "activity.csv", archiveName = "activity.zip", baseDir = ".") {
  activitySrcPath <- file.path(baseDir, srcFileName);
  if (!file.exists(activitySrcPath)) {
    unzip(zipfile = file.path(baseDir, archiveName), overwrite = TRUE, exdir = baseDir);
  }
  
  return (read.csv(activitySrcPath, stringsAsFactors = FALSE))
}

main <- function() {
  raw_df <- ensureActivityData();
  
  df <- data.frame(raw_df, date_time = strptime(paste(raw_df$date, sapply(raw_df$interval, zeroPadInterval)), "%F %H%M"));
  
  cleanDf <- df[(which(!is.na(df$steps))), ]
  nonZeroClean <- df[(cleanDf$steps != 0), ]
  # histogram of daily steps taken

  dailySum <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = sum);
  names(dailySum) <- c("Date", "TotalSteps");
  
  dailySumHist <- ggplot(dailySum) + geom_histogram(aes(TotalSteps), binwidth=500)
  print(dailySumHist);
  
  meanPerDay <- mean(dailySum$TotalSteps, na.rm=T);
  
  dailyMedian <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = median);
  names(dailyMedian) <- c("Date", "MedianSteps")
  dailyMean <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = mean);
  names(dailyMean) <- c("Date", "MeanSteps");
  
  

  # At the interval level, we'll need to inject some data:
  
  # Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
  totalMissing = length(which(is.na(df$steps)));
  
  # Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
  # For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
  
  # all NA's get the daily average
  withMean <- data.frame(df, with_mean = sapply(df$date, addAverageForRespectiveDay, averages=dailyMean));
  
  imputed_activity <- data.frame(withMean, imputed_steps = mapply(FUN=chooseMeanOrValueOnNA, steps=withMean$steps, mean=withMean$with_mean));
  # Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
  # number of steps taken, averaged across all days (y-axis)
  aggregatedByInterval <- aggregate(cleanDf$steps, by=list(interval = cleanDf$interval), FUN = mean);
  names(aggregatedByInterval) <- c("interval", "AvgSteps")
  
  avgIntervalTsPlot <- ggplot(aggregatedByInterval) + geom_line(aes(interval, AvgSteps))
  print(avgIntervalTsPlot);
  maxAvgStepsInterval <- aggregatedByInterval[(aggregatedByInterval$AvgSteps == max(aggregatedByInterval$AvgSteps)), ]
  
  # Make a histogram of the total number of steps taken each day and Calculate and report the mean and 
  # median total number of steps taken per day. Do these values differ from the estimates from the first 
  # part of the assignment? What is the impact of imputing missing data on the estimates of the total
  # daily number of steps?
  imputedDailyTotal <- aggregate(imputed_activity$imputed_steps, by=list(date = imputed_activity$date), FUN = sum)
  names(imputedDailyTotal) <- c("date", "total");
  imputedDailyTotalHist <- ggplot(imputedDailyTotal) + geom_histogram(aes(total), binwidth=500);
  print(imputedDailyTotalHist);
  
  imputedDailyMedian <- aggregate(imputed_activity$imputed_steps, by=list(date = imputed_activity$date), FUN = median);
  names(imputedDailyMedian) <- c("date", "median")
  imputedDailyMean <- aggregate(imputed_activity$steps, by=list(date = imputed_activity$date), FUN = mean);
  names(imputedDailyMean) <- c("date", "mean");
  
  # Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
  # indicating whether a given date is a weekday or weekend day.
  dayType <- factor(c("Weekend", "Weekday"));
  calendarizedAdjustedActivity <- data.frame(imputed_activity, type = sapply(imputed_activity$date, dayTypeFunc));
  
  weekendData <- calendarizedAdjustedActivity[calendarizedAdjustedActivity$type == "Weekend", c("interval", "imputed_steps")];
  weekendAverage <- data.frame(aggregate(weekendData$imputed_steps, by=list(interval = weekendData$interval), FUN = mean), type="Weekend");
  
  weekdayData <- calendarizedAdjustedActivity[calendarizedAdjustedActivity$type == "Weekday", c("interval", "imputed_steps")]
  weekdayAverage <- data.frame(aggregate(weekdayData$imputed_steps, by=list(interval = weekdayData$interval), FUN = mean), type="Weekday");

  combined <- rbind(weekendAverage, weekdayAverage);
  names(combined) <- c("interval", "avg_steps", "type");
  plot(weekendData, type="l")
  plot(weekdayData, type="l")
  
  # Make a time series plot (i.e. typed = "l") of the 5-minute interval (x-axis) and the 
  # average number of steps taken, average across all days (y-axis)
  p <- ggplot(calendarizedAdjustedActivity) + geom_line(aes(date_time, imputed_steps)) + facet_grid(type ~ .)
  print(p)  
}

zeroPadInterval <- function(intervalStr) {
  zeros <- rep(0, 4-nchar(as.character(intervalStr)));
  result <- paste0(paste0(zeros, collapse=""), as.character(intervalStr), collapse="")
  return (result);
}

# takes a date string and returns a string (level) indicating whether its a weekday or a weekend
dayTypeFunc <- function(date) {
  d <- strptime(date, format = "%F");
  day <- weekdays(d);
  if (day == "Saturday" | day == "Sunday") {
    return ("Weekend")
  } else return ("Weekday");
}

chooseMeanOrValueOnNA <- function(steps, mean) {
  if(is.na(steps)) {
    return (mean);
  } else {
    return (steps);
  }
}

addAverageForRespectiveDay <- function(date, averages) {
  avg <- averages[(averages$Date == date), "MeanSteps"]
  if (is.na(avg)) return (0);
  return (avg);
}