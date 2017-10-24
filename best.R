best <- function(state, outcome) {
  illness <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% illness) {
    stop("invalid outcome")
  }
  
  if(!state %in% data[, "State"]) {
    stop("invalid state")
  } 
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_index <- which(data[, "State"] == state)
  by_state <- data[state_index, ]
  
  if(outcome == "heart attack") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } 

find_best <- function(column) { 
      by_state[, column] <- as.numeric(by_state[, column])
      lowest <- min(by_state[, column], na.rm = TRUE)
      winner <- subset(by_state, lowest == by_state[, column])
      return(sort(winner[1 , "Hospital.Name"]))
    }
  find_best(column)
}
  
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")