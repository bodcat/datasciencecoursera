rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data from file
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
    ## To check for invalid state, look to see if state exists in data set
  state_data <- outcome_data[outcome_data$State %in% c(state), ]
  if (nrow(state_data) == 0) {
    stop("invalid state")
  }
  
  
  ## Sort the state data by mortality rate and name (alphabetical)
  if (outcome == 'heart attack') {
    ## Ensure Mortality Rate is numeric
    state_data[,11] <- as.numeric(state_data[,11]) ## Heart Attack Rate
    
    ## Filter out NAs
    valid_state_data <- state_data[!is.na(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]

    ## Sort data by Mortality Rate and Name
    sorted_state_data <- valid_state_data[order(valid_state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, valid_state_data$Hospital.Name),]  

  } else if (outcome == 'heart failure') {
    ## Ensure Mortality Rate is numeric
    state_data[,17] <- as.numeric(state_data[,17]) ## Heart Failure Rate
    
    ## Filter out NAs
    valid_state_data <- state_data[!is.na(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]

    ## Sort data by Mortality Rate and Name
    sorted_state_data <- valid_state_data[order(valid_state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, valid_state_data$Hospital.Name),]      
  } else if (outcome == 'pneumonia') {
    ## Ensure Mortality Rate is numeric
    state_data[,23] <- as.numeric(state_data[,23]) ## Pneumonia Rate
    
    ## Filter out NAs
    valid_state_data <- state_data[!is.na(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    
    ## Sort data by Mortality Rate and Name
    sorted_state_data <- valid_state_data[order(valid_state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, valid_state_data$Hospital.Name),]      
  } else {
    stop("invalid outcome")
  }
  
  ## Validate and interpret num parameter
  if (num == "best") {
    rank <- c(1);
  } else if (num == "worst") {
    rank <- nrow(sorted_state_data)
  } else if (is.numeric(num)) {
    rank <- num
  } else {
    stop("invalid rank")
  }
  
  
  
  ## Identify and return the top of the list
  if (rank > nrow(sorted_state_data)) {
    NA  
  } else {
    sorted_state_data$Hospital.Name[rank]  
  }
  
}