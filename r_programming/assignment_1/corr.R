
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  id <- 1:332
  compl = complete(directory, id)
  compl <- compl[compl$nobs > threshold,]
  cr = vector()
  for(i in compl$id) {
    ptrn <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
    data <- read.csv(ptrn)
    cleaned <- data[!is.na(data$sulfate) & !is.na(data$nitrate), ]
    correlation <- cor(cleaned$sulfate, cleaned$nitrate)
    cr <- c(cr, correlation)
  }
  return(cr)
}