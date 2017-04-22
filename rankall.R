rankall <- function(outcome, num = "best") {
  ## Read .csv file with data setted as character class
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[, c(2, 7, 11, 17, 23)]
  
  ## Validation of the arguments
  ## Validating "outcome" argument
  if (outcome == "heart attack") data <- data[, c(1,2, 3)]
  else if (outcome == "heart failure") data <- data[, c(1,2, 4)]
  else if (outcome == "pneumonia") data <- data[, c(1,2, 5)]
  else stop("invalid outcome")

  ## Naming collumns
  names(data) <- c("Hospital", "State", "Deaths")
  
  ## Removing NA values
  data[, 3] = suppressWarnings(as.numeric(data[, 3]))
  bad <- is.na(data$Deaths)
  data <- data[!bad, ]
  
  ## Splitting data in different data frames by the State
  splited <- split(data, data$State)
  
  loop <- lapply(splited, function(x, num) {
    # Order by Deaths and then HospitalName
    x = x[order(x$Deaths, x$Hospital),]
    
    # Return
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital[1])
      }
      else if(num == "worst") {
        return (x$Hospital[nrow(x)])
      }
    }
    else {
      return (x$Hospital[num])
    }
  }, num)
  
  return (data.frame(hospital=unlist(loop), state=names(loop)))
}
