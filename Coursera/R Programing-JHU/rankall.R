# Coursera week 3 assignment
# Test data

#state <- "MN"
outcome <- "heart attack"
num <- 20

rankall <- function(outcome, num = "best") {
  
  # Keep current working directory in wd.bak and set directory to data
  wd.bak <- getwd()
  setwd("C:/Users/Danny/Documents/R Learning/Cousera/Hospital Quality")
  
  # Read data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check if outcome valid, if not stop
  if(is.na(match(outcome, c("heart attack", "heart failure", "pneumonia"), nomatch = NA))) stop("invalid outcome")
  
  # Set variable col.inc = absolute position of Mortality.Rate from outcome
  if (outcome == "heart attack") {col.ind <- 11} else
    if (outcome == "heart failure") {col.ind <- 17} else
      if (outcome == "pneumonia") {col.ind <- 23}
  
  # Subset State, Hospital.Name, Mortality.Rate
  data.subset <- data[,c(7, 2, col.ind)]
  
  # Rename column subsetdata
  colnames(data.subset) <- c("state", "hospital", "Mortality.Rates")
  
  # Cast Mortality.Rate from charactor to numberic
  data.subset$Mortality.Rates <- as.numeric(data.subset$Mortality.Rates)
  
  # Create all state list and sort
  state.list <- sort(as.character(unique(data$State)))
  
  # Remove NA caused by as.numeric
  data.subset <- na.omit(data.subset)
  
  # Define out for output dataframe 
  out <- data.frame(hospital = character(), state = character())
  
  # Order by data.subet by Mortality.Rate & Hospital.Name 
  # then fetch ranking needed to variable num
  
  # Loop through all states list
  for(i in state.list){
    
    # Subset by each state
    t <- data.subset[data.subset$state == i,]
    
    # Order each State by Mortality.Rates then hospital
    t <- t[order(t$Mortality.Rates, t$hospital),]
    
    # Convet best or worst to be absoute index of rank
    if(num == "best") rank <- 1 else 
    if(num == "worst") rank <- nrow(t) else
    rank <- num
    
    # Check if rank needed > avaliable rank, throw out NA - State
    if(rank > nrow(t)) {
      out<-rbind(out, data.frame(hospital = NA, state = i))
    } else {
      out<-rbind(out, data.frame(hospital = t$hospital[rank], state = i))
      }
  } # loop to next state
  
  # Revert working directory to the backup one
  setwd(wd.bak)
  
  # return data 
  return(out)
}