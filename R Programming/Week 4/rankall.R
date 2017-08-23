rankall <- function(outcome, rank = 1) {
# Read the data  
data <- read.csv('outcome-of-care-measures.csv', na.strings =
                     'Not Available', stringsAsFactors = FALSE)
states <- sort(unique(data$State))
#outcome <- 'pneumonia'
#rank <- 'worst'
# Error stoppings
if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) {
  stop('invalid outcome')
}


# Filter out the columns we'll be working with
outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
data <- data[, c(2, 7, outcomes[outcome])]
names(data) <- c('Hospital', 'State', outcome)
head(data, 10)
# Arrange the data in descending order according to the outcome variable
arrangedData <- data[order(data[[3]], data[[1]]), ]
head(arrangedData, 10)
# Split the data on the State variable
split_data <- split(arrangedData, arrangedData$State)
split_data <- lapply(split_data, function(data) data<-na.omit(data))
# Extract the  hospital name and state from eat state with the chosen rank.
if (rank == 'best') {
  rank <- 1
}
if (rank == 'worst') {
  list_of_dfs <- lapply(split_data, function(data) data[nrow(data), 1:2])
  head(list_of_dfs)
  df <- do.call('rbind', list_of_dfs)
  rownames(df) <- NULL
  df
  df$State <- states
  return(df)
}

if (rank > nrow(data) || (rank < 1)){
  stop('NA')
}

list_of_dfs <- lapply(split_data, function(data) data[rank, 1:2])
head(list_of_dfs)
# Combine the list of dataframes into a single datafarme
df <- do.call('rbind', list_of_dfs)
# Remove the rownames(the name of each state)
rownames(df) <- NULL
# Return the dataframe
df$State <- states
df
}