  rankhospital <- function(state, outcome, num = "best") {
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

get_rank <- function(column) {
    by_state[, column] <- as.numeric(by_state[, column])
    state_ordered <- order(by_state[, column] , by_state[, "Hospital.Name"], na.last = NA)
        if(num == "best") {
          return(by_state[, "Hospital.Name"][state_ordered[1]])
        } else if(num == "worst") {
          return(by_state[, "Hospital.Name"][state_ordered[length(state_ordered)]])
        } else if(is.numeric(num)) {
          return(by_state[, "Hospital.Name"][state_ordered[num]])
        } else {
          return(NA)
          }
        }
     get_rank(column)
  } 
  
  rankhospital("TX", "heart failure", 4)
  rankhospital("MD", "heart attack", "worst")
  rankhospital("MN", "heart attack", 5000)
  rankhospital("XN", "heart attack", 5000)
  rankhospital("MN", "heart atack", 5000)
  rankhospital("NY", "heart failure", 1)
  rankhospital("NY", "heart attack", 1)
  rankhospital("NY", "pneumonia", 1)
  
  
