rankall <- function(oc, num = "best") {
    
    outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    outcome_indices <- c(11L,17L,23L)
    
    getIndex <- function(outcome_name) {
        outcome_indices[match(outcome_name, outcomes)]
    }
    
    ## Check that outcome is valid
    if(! oc %in% outcomes) {
        stop('invalid outcome')
    }
    
    # convert to numeric
    for(ind in outcome_indices) {
        outcome[, ind] <- as.numeric(outcome[, ind])
    }
    
    outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    
    
    getNForState <- function(state, oc, num = "best") {        
        
        stateOutcome <- outcome[outcome$State == state,]
        
        col_num <- getIndex(oc)
        #outcome_arr <- outcome[, col_num]
        sorted <- stateOutcome[ order(stateOutcome[,col_num], stateOutcome[,2]), ]
        notisna <- !is.na(sorted[,col_num])
        if(num == "best") {
            sorted[1,2]
        } else if (num == "worst") {
            sorted[which.max(sorted[,col_num]),2]
        } else {
            sorted[num,2]
        }    
    }
    result <- c()
    allStates <- outcome$State
    allStates <- sort(unique(allStates))
    
    for(state in allStates) {
        hospital  <- getNForState(state, oc, num)
        result <- rbind(result, list(state=state, hospital=hospital))
    }
    result <- data.frame(result)    
    result
}
