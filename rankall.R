rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomes <- readOutcomes("outcome-of-care-measures_test.csv")
  ## Check that state and outcome are valid
  outcome.index <- grep(tolower(gsub(" ", ".", outcome)), tolower(colnames(outcomes)))[1]
  if (is.na(outcome.index)) {
    stop("invalid outcome", call. = TRUE)    
  }
  df <- outcomes[, c(2:10, outcome.index)]
  bad <- is.na(df[,10])
  df <- df[!bad,]
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df <- df[order(df[, 10], df$State, df[, 1]), ]
 # print(df[1:10, ])
  states <- levels(as.factor(df$State))
  return.df <- data.frame(hospital = NA, state = states)
  for(state in states) {
    state.set <- subset(df, df$State == state)
    if (num == "best") {
      row.idx <- which.min(state.set[,10])
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      return.df[return.df$state == state,]$hospital <- (state.set[row.idx, 1])
    }
    if (num == "worst") {
      row.idx <- which.max(state.set[,10])
      ## Return hospital name in that state with highest 30-day death
      ## rate
      return.df[return.df$state == state,]$hospital <- (state.set[row.idx, 1])    
    }
    if (is.numeric(num)) {
      if (!num > length(state.set[, 1])) {
        return.df[return.df$state == state,]$hospital <- (state.set[num, 1])
      } else {      
        return.df[return.df$state == state,]$hospital <- (NA)
      }
    }
  }
  return(return.df)
}

rankall("pneumonia")
