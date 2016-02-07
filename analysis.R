
ensureActivityData <- function(srcFileName = "activity.csv", archiveName = "activity.zip", baseDir = ".") {
  activitySrcPath <- file.path(baseDir, srcFileName);
  if (!file.exists(activitySrcPath)) {
    unzip(zipfile = file.path(baseDir, archiveName), overwrite = TRUE, exdir = baseDir);
  }
  
  return (read.csv(activitySrcPath, stringsAsFactors = TRUE))
}

main <- function() {
  df <- ensureActivityData();
  
  cleanDf <- df[(which(!is.na(df$steps))), ]
  nonZeroClean <- df[(cleanDf$steps != 0), ]
  # histogram of daily steps taken
  
  dailySum <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = sum);
  dailyMedian <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = median);
  dailyMean <- aggregate(nonZeroClean$steps, by=list(date = nonZeroClean$date), FUN = mean);
  
  # TODO Format this!
  dailySumHist <- ggplot(dailySum) + geom_histogram(aes(x))
  print(dailySumHist)
}