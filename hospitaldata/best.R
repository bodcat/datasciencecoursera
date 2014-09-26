best <- function(state, outcome) {

  ## Read outcome data from file
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## To check for invalid state, look to see if state exists in data set
  state_data <- outcome_data[outcome_data$State %in% c(state), ]
  if (nrow(state_data) == 0) {
    stop("invalid state")
  }
  
  ## Ensure Mortality Rates are numeric
  state_data[,11] <- as.numeric(state_data[,11]) ## Heart Attack Rate
  state_data[,17] <- as.numeric(state_data[,17]) ## Heart Failure Rate
  state_data[,23] <- as.numeric(state_data[,23]) ## Pneumonia Rate
  
  ## Sort the state data by mortality rate and name (alphabetical)
  if (outcome == 'heart attack') {
    sorted_state_data <- state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, state_data$Hospital.Name),]  
  } else if (outcome == 'heart failure') {
    sorted_state_data <- state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, state_data$Hospital.Name),]      
  } else if (outcome == 'pneumonia') {
    sorted_state_data <- state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, state_data$Hospital.Name),]      
  } else {
    stop("invalid outcome")
  }
    
  
  
  ## Identify and return the top of the list
  sorted_state_data$Hospital.Name[1]
}