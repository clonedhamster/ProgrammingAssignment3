
readOutcomes <- function(filename) {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  footnotes <- setdiff(seq(11, ncol(outcome)), grep("Footnote", colnames(outcome)))
  numeric.data <- setdiff(footnotes, grep("Comparison", colnames(outcome)))
  for(i in numeric.data) {
    outcome[, i] <- as.numeric(outcome[, i])
  }
  return(outcome)
}