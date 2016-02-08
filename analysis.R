library(ggplot2)

ensureActivityData <- function(srcFileName = "activity.csv", archiveName = "activity.zip", baseDir = ".") {
  activitySrcPath <- file.path(baseDir, srcFileName);
  if (!file.exists(activitySrcPath)) {
    unzip(zipfile = file.path(baseDir, archiveName), overwrite = TRUE, exdir = baseDir);
  }
  
  return (read.csv(activitySrcPath, stringsAsFactors = FALSE))
}

main <- function() {
  df <- ensureActivityData();
  
  cleanDf <- df[(which(!is.na(df$steps))), ]
  nonZeroClean <- df[(cleanDf$steps != 0), ]
  # histogram of daily steps taken
  
  dailySum <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = sum);
  names(dailySum) <- c("Date", "TotalSteps");
  dailyMedian <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = median);
  names(dailyMedian) <- c("Date", "MedianSteps")
  dailyMean <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = mean);
  names(dailyMean) <- c("Date", "MeanSteps");
  
  # TODO Format this!
  dailySumHist <- ggplot(dailySum) + geom_histogram(aes(TotalSteps), binwidth=500)
  print(dailySumHist)
  
  
  # At the interval level, we'll need to inject some data:
  
  # Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
  totalMissing = length(which(is.na(df$steps)));
  
  # Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
  # For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
  
  # all NA's get the daily average
  adjustedActivity <- data.frame(df, sapply(df$date, addAverageForRespectiveDay, averages=dailyMean));
  names(adjustedActivity) <- c("steps_unadjusted", "date", "interval", "steps");

  # Make a time series plot (i.e. typed = "l") of the 5-minute interval (x-axis) and the 
  # average number of steps taken, average across all days (y-axis)
  # TODO
  
  
}

addAverageForRespectiveDay <- function(date, averages) {
  avg <- averages[(averages$Date == date), "MeanSteps"]
  if (is.na(avg)) return (0);
  return (avg);
}