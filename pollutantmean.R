##calculates the mean of a pollutant (sulfate or nitrate) 
##across a specified list of monitors. The function 'pollutantmean' 
##takes three arguments: 'directory', 'pollutant', and 'id'. 

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## Make a list of filenames
  filenames <- list.files(path=directory, pattern="*.csv")
  
  ## Initialize a vector to hold the values
  values <- vector()
  
  ## Loop over the ID passed in the fuction
  for(i in id) {
    
    ## Pad the i to create a filename
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    
    ## Load the data
    data <- read.csv(filepath)
    
    ## Select our column
    column <- data[,pollutant]
    
    ## Ignore NAs
    column <- column[!is.na(column)]
    
    ## append to our vector
    values <- c(values, column)
  }
  
  ## Return the value 
  mean(values)
  
}

