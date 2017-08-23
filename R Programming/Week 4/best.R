best <- function(state, outcome) {
  
  # Read the data and set up the error stopping functions.
  data <- read.csv('outcome-of-care-measures.csv', na.strings =
                     'Not Available', stringsAsFactors = FALSE)
  
  # if outcome not in the list of chosen outcomes: stop
  # same for states
  
  if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) {
    stop('invalid outcome')
    }
  if(!(state %in% unique(data$State))) {
    stop('invalid state')
    }
  
  # If no errors go on
  ## Subset out the data we will use
  outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  data <- data[, c(2, 7, outcomes[outcome])]
  ## Choose only data from the relevant state
  data <- data[data[['State']] == state, ]
  data <- na.omit(data)
  data <- data[order(data[[3]], data[[1]]), ]
  # Output the hospital(column 1) with the lovest value in column 3(outcome)
  data[1, 1]
  
}
