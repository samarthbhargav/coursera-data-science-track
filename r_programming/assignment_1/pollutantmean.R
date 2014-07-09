pollutantmean <- function(directory, pollutant, id = 1:332) {
  #temp = list.files(pattern="*.csv")
  #myfiles = lapply(temp, read.delim)
  means <- vector()
  for(i in id) {
    ptrn <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
    data <- read.csv(ptrn)
    cleaned <- (data[!is.na(data[[pollutant]]), ])
    means <- c(means, cleaned[[pollutant]])
  }
  return(mean(means))
}