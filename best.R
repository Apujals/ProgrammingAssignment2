best <- function(state, outcome) {
  ## First we load the data from the .csv file
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Setting the vectors will be used to validate the arguments of "best" function
  st_levels <- unique(data$State)
  out_v <- c("heart attack", "heart failure", "pneumonia")
  
  ## Validating arguments of "best" function
  if(any(st_levels == state)) {
    if(any(out_v == outcome)) {
    } else {
      stop("invalit outcome")
    }
  }else {
    stop("invalid state")
  }
  
  ## Subsetting data of the state given as argument
  st_data <- subset(data, data$State == state)
  
  ## Defining which outcome will be used
  if(outcome == "heart attack") c <- 11
  else if(outcome == "heart failure") c <- 17
  else if(outcome == "pneumonia") c <- 23

  ## NA removing
  ## BE CAREFULL!! the .csv files has been called with colClases = character 
  ## this way "Not Available" data will be coerced as "NA"
  bad <- is.na(as.numeric(st_data[, c]))
  good_data <- st_data[!bad, ]
  
  ## Evaluating which rows have the minimun rate of mortality
  ## the same... first set the data as numeric
  num_data <- as.numeric(good_data[, c])
  min_out <- which(num_data == min(num_data))
  
  ## Getting all hospitals with the minimum rate of mortality
  hosps <- good_data[min_out, 2]
  
  ## Chosing the first hospital in the alphabetical order
  hosp <- sort(hosps)
  hosp[1] 
}
best("AL", "heart attack")
