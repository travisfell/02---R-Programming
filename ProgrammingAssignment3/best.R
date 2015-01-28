## This script contains the best function that ranks hospitals by
# outcomes for the given state.
# This script also breaks ties by hospital name


best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses ="character")
  
  ## Check that state and outcome are valid
  # compare entered state to state column
  uniquestate <- unique(data[,7])
  if (state %in% uniquestate == FALSE) {
    stop("invalid state") 
  }
  
  
  # compare entered outcome to outcome column heads
  validoutcome <- c("Heart Attack", "Heart Failure", "Pneumonia")
  if (outcome %in% validoutcome == FALSE) {
    stop("invalid outcome")
  }
  else if (outcome == "Heart Attack") {
    hospitalandscore <- subset(data, State == state, select = c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
  }
  else if (outcome == "Heart Failure") {
    hospitalandscore <- subset(data, State == state, select = c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
  }
  else if (outcome == "Pneumonia"){
    hospitalandscore <- subset(data, State == state, select = c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
  }
    
    
  ## Return hospital name in the state with lowest 30-day death rate
  # exclude Not Available" data 
  completescores <- subset(hospitalandscore, hospitalandscore[,2] != "Not Available")  
  
  # rank hospitals:
  # find min in hospitalandscore and save to variable
  lowvalue <- completescores[which.min(completescores[,2]),2]
 
  # subset hospitalandscore by minimum score
  lowhospital <- subset(completescores, completescores[,2] == lowvalue, select = Hospital.Name)
  
  # break ties alphabetically by hospital name: 
  # order subset in ascending order and take the first record
  firstlowhospital <- head(lowhospital[order(lowhospital[,1]),])
  
  firstlowhospital
  
}
