rankhospital <- function(state, oc, num = "best") {
    
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
    #outcome_arr <- outcome[, col_num]
    sorted <- outcome[ order(outcome[,col_num], outcome[,2]), ]
    notisna <- !is.na(sorted[,col_num])
    if(num == "best") {
        sorted[1,2]
    } else if (num == "worst") {
        sorted[which.max(sorted[,col_num]),2]
    } else {
        sorted[num,2]
    }
}