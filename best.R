best <- function(state, outcome) {
  ## Read the outcome
  outcomes <- readOutcomes("outcome-of-care-measures_test.csv")
  ## Check that state and outcome are valid
  if(!state %in% outcomes$State) {
    stop("invalid state", call. = TRUE)
  }
  outcome.index <- grep(tolower(gsub(" ", ".", outcome)), tolower(colnames(outcomes)))[1]
  if (is.na(outcome.index)) {
    stop("invalid outcome", call. = TRUE)    
  }
  df <- outcomes[outcomes$State == state, c(2:10, outcome.index)]
  df <- df[order(df[, 1]), ]
#  bad <- is.na(df[,10])
#  df <- df[!bad,]
  row.idx <- which.min(df[,10])
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return(df[row.idx, 1])
  
}



