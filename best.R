best <- function(state, outcome){
  ## Validating the outcome string
  ## Creating a vector the diseases whose outcome we may want
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% outcomes == FALSE) stop("invalid outcome")
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Filter and simplify the column names
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  ## Validating the state string
  states <- data[, 2]
  states <- unique(states)
  if(state %in% states == FALSE) stop("invalid state")
  
  ## Take only those rows with have the required state value	
  data <- data[data$state==state & data[outcome] != 'Not Available', ]
  vals <- data[, outcome]
  rowNum <- which.min(vals)
  ## Return hospital name in that state with lowest 30-day death rate
  data[rowNum, ]$name
}