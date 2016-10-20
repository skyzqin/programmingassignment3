rankhospital <- function(state, outcome, num = "best"){
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
  ##Return hospital name in that state with the given rank 30-day death rate
  nRate <- 0; 
  if(outcome == "heart attack")
    nRate <- 11
  else if(outcome == "heart failure")
    nRate <- 17
  else if(outcome == "pneumonia")
    nRate <- 23
  outcomeData[,nRate] <- as.numeric(outcomeData[,nRate]);
  outcomeData <- subset(outcomeData,State==state & !is.na(outcomeData[,nRate]),select=c(2,nRate));
  
  newdata <- outcomeData[order(outcomeData[,2], outcomeData[,1]),];
  nRank <- 0;
  nLength <- nrow(newdata);
  if(num == "best")
    nRank <- 1
  else if (num == "worst")
    nRank <- nLength
  else if(num < nLength)
    nRank¡¡<- num
  else
    {
      return (NA);
    }
   newdata[nRank,1];
}