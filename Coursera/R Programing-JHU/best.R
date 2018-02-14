# Test part
state <- "TX"
outcome <- "heart attack"

best <- function(state, outcome){
  # Keep current working directory in wd.bak and set directory to data
  wd.bak <- getwd()
  setwd("C:/Users/Danny/Documents/R Learning/Cousera/Hospital Quality")
  
    # Read data to variable data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check if state valid, if not stop 
  ui.stat <- unique(data$State)
  if(is.na(match(state, ui.stat, nomatch = NA))) stop("invalid state")
  
  # Check if outcome valid, if not stop
  if(is.na(match(outcome, c("heart attack", "heart failure", "pneumonia"), nomatch = NA))) stop("invalid outcome")

  # Map Column index with outcome
 
   if (outcome == "heart attack") {col.ind <- 11} else
    if (outcome == "heart failure") {col.ind <- 17} else
      if (outcome == "pneumonia") {col.ind <- 23}

  # Subset only State, Hospital.Name, Mortality.Rate
  data.out <- data[data$State == state, c(7, 2, col.ind)]
  
  # Reassign column for better reference
  colnames(data.out) <- c("State", "Hospital.Name", "Mortality.Rates")
  
  # Cast Mortality.Rate from factor to numberic
  # data.out$Mortality.Rates <- as.numeric(levels(data.out$Mortality.Rates))[data.out$Mortality.Rates]

  # Cast Mortality.Rate from character to numberic and skip NA value
  data.out$Mortality.Rates <- as.numeric(data.out$Mortality.Rates)

  # Remove NA caused by as.numeric
  data.out <- na.omit(data.out)
  
  # Order by Mortality.Rate then Hospital.Name
  data.out <- data.out[order(data.out$Mortality.Rates, data.out$Hospital.Name),]
  
  # Revert working directory to the backup one
  setwd(wd.bak)
  
  #Return top Hospital.Name 
  return(data.out$Hospital.Name[1])
  }