rankall <- function(outcome, num = "best") {
  illness <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% illness) {
    stop("invalid outcome")
  }
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

   if(outcome == "heart attack") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
    
get_hospitals <- function(column) {
        data[, column] <- as.numeric(data[, column])
        by_state <- split(data, data[, "State"])
        Hospital <- sapply(by_state, function (x, num) { 
          x <- x[order(x[, column], x[,"Hospital.Name"], na.last = NA), ]
        if(num == "best") {
          return(x[, "Hospital.Name"][1])
        } else if(num == "worst") {
          return(x[, "Hospital.Name"][length(x)])
        } else if(is.numeric(num)) {
          return(x[, "Hospital.Name"][num])
        } else {
          return(NA)
        } 
      }, num)
        return(data.frame(Hospital))
  }
  get_hospitals(column)
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)