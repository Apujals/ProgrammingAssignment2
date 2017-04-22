rankhospital <- function(state, outcome, num = "best" ) {
  ## Read .csv file with data setted as character class
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Validation of "state" argument
  states <- unique(data$State)
  if (any(state == states)) {} else {stop("invalid state")}
  
  ## Validation of "outcome" argument
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (any(outcome == outcomes)) {} else {stop("invalid outcome")}
  
  ## Defining which outcome will be used
  if(outcome == "heart attack") c <- 11
  else if(outcome == "heart failure") c <- 17
  else if(outcome == "pneumonia") c <- 23
  
  
  ## Subsetting data defined by "state" and "outcome" arguments
  ## this will shorten the data frame, optimizating the speed of the function
  data2 <- subset(data, data$State == state, select = c(2, c))
  
  ## The current data frame has only 2 collumns
  ##    First <- Hospital.Name
  ##    Second <- 30 days mortality outcome
  
  ## Removing NA
  bad <- is.na(as.numeric(data2[, 2]))
  good_data <- data2[!bad, ]
  
  ## Validation of "num" argument
  ## If "num" argument is larger than the sum of hospitals in the
  ## given state, then return NA
  if (is.numeric(num == TRUE)) {
    if (num > sum(data$State == state)) {
      return(NA)
    }
  }
  
  ## Assigning a numeric value to "best" and "worst"
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num <- 1
    }
    else if (num == "worst") {
      num <- nrow(good_data)
    }
  }
  
  ## Sorting by the rate of mortality in increasing order
  ## and by the names of hospitals alphabeticaly
  col_rate <- as.numeric(good_data[, 2])
  col_name <- good_data[, 1]
  ordered_data <- good_data[with(good_data, order(col_rate, col_name)), ]

  ## Returnig the result (in the new data frame hopital's names are in the firt column)
  return(ordered_data[num, 1])
}
