best <- function(state, outcome){
  ##Read outcome data
  outcomeData<-read.csv("rprog_data_ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses = "character");
  ##Check that state and outcome are valid
  vState <- outcomeData$State;
  length <-length(vState);
  validState <- FALSE;
   for(i in 1:length){
    if(vState[i] == state){
      validState <- TRUE;
      break;
    }
  }
  if(!validState){
    stop("invalid state");
   }
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia"){
    stop("invalid outcome");
  }
  
    ##Return hospital name in that state with lowest 30-day death rate
  ##change to be number
  nRate <- 0; 
  if(outcome == "heart attack")
    nRate <- 11
  else if(outcome == "heart failure")
    nRate <- 17
  else if(outcome == "pneumonia")
    nRate <- 23
  outcomeData[,nRate] <- as.numeric(outcomeData[,nRate]);
  outcomeData <- subset(outcomeData,State==state & !is.na(outcomeData[,nRate]),select=c(2,nRate));
  
  newdata <- outcomeData[order(outcomeData$Hospital.Name),];
  minIndex <- which.min(newdata[,2]);
  newdata[minIndex,1];
}