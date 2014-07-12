complete <- function(directory, id = 1:332) {
  ids <- vector()
  nobs <- vector()
  for(i in id) {
    ptrn <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
    data <- read.csv(ptrn)
    count <- nrow(data[!is.na(data$sulfate) & !is.na(data$nitrate), ])
    ids <- c(ids, i)
    nobs <- c(nobs, count)
  }
  to_return <- data.frame(ids,nobs)
  return(to_return)
}