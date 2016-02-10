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
  dailyMedian <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = median);
  names(dailyMedian) <- c("Date", "MedianSteps")
  dailyMean <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = mean);
  names(dailyMean) <- c("Date", "MeanSteps");
  
  dailySumHist <- ggplot(dailySum) + geom_histogram(aes(TotalSteps), binwidth=500)
  print(dailySumHist);

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
  adjustedDailyTotal <- aggregate(adjustedActivity$steps, by=list(date = adjustedActivity$date), FUN = sum)
  names(adjustedDailyTotal) <- c("Date", "Total");
  adjustedDailyTotalHist <- ggplot(adjustedDailyTotal) + geom_histogram(aes(Total), binwidth=500);
  print(adjustedDailyTotalHist);
  
  adjustedDailyMedian <- aggregate(adjustedActivity$steps, by=list(date = adjustedActivity$date), FUN = median);
  names(adjustedDailyMedian) <- c("Date", "Median")
  adjustedDailyMean <- aggregate(adjustedActivity$steps, by=list(date = adjustedActivity$date), FUN = mean);
  names(adjustedDailyMean) <- c("Date", "Mean");
  
  # Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
  # indicating whether a given date is a weekday or weekend day.
  dayType <- factor(c("Weekend", "Weekday"));
  calendarizedAdjustedActivity <- data.frame(adjustedActivity, type = sapply(adjustedActivity$date, dayTypeFunc));
  
  padded <- data.frame(calendarizedAdjustedActivity, padded_interval = sapply(calendarizedAdjustedActivity$interval, zeroPadInterval));
  justDateTime <- data.frame(date = padded$date, time = padded$padded_interval);
  dateStrings <- paste(justDateTime$date, justDateTime$time);
  parsed_date <- strptime(dateStrings, "%F %H%M");
  
  timeSeriesActivity <- data.frame(calendarizedAdjustedActivity, actual_date = parsed_date);
  
  # Make a time series plot (i.e. typed = "l") of the 5-minute interval (x-axis) and the 
  # average number of steps taken, average across all days (y-axis)
  p <- ggplot(timeSeriesActivity) + geom_line(aes(actual_date, steps)) + facet_grid(type ~ .)
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