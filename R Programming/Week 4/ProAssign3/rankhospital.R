rankhospital <- function(state, outcome, rank) {
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
# Dealing with specific cases of the 'rank' variable.
if (rank == 'best') {
  rank <- 1
}
if (rank == 'worst') {
  rank <- nrow(data)
}
if (rank > nrow(data) || (rank < 1)){
  stop('NA')
}

# If no errors go on
## Subset out the data we will use
outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
data <- data[, c(2, 7, outcomes[outcome])]
names(data) <- c('Hospital.Name', 'State', outcome)
## Choose only data from the relevant state
data <- data[data[['State']] == state,]
data <- na.omit(data)

# Sort the data by the outcome
arrangedData <- data[order(data[[3]], data[[1]]), ]
arrangedData[rank, 1]
}