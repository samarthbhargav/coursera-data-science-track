# outcome-of-care-measures.csv

best <- function(state, oc) {
  
  getIndex <- function(outcome_name) {
    outcome_indices[match(outcome_name, outcomes)]
  }
  # Valid States and their indices
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  outcome_indices <- c(11L,17L,23L)
  
  ## Check that outcome is valid
  if(! oc %in% outcomes) {
    stop('invalid outcome')
  }
  ## Read outcome data
  outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  
  ## Check that state is valid
  if( !state %in% outcome$State) {
    stop('invalid state')
  }
  
  outcome <- outcome[outcome$State == state,]
  
  for(ind in outcome_indices) {
    outcome[, ind] <- as.numeric(outcome[, ind])
  }
  
  col_num <- getIndex(oc)
  outcome_arr <- outcome[, col_num]
  min <- min(outcome_arr, na.rm=T)
  min_index <- which(outcome_arr == min)
  sort(as.character(outcome[min_index, 2]))
}