getRank <- function(data,num){
    newdata <- data[order(data[,3], data[,1]),];
    nRank <- 0;
    nLength <- nrow(newdata);
    valid = TRUE;
    if(num == "best")
      nRank <- 1
    else if (num == "worst")
      nRank <- nLength
    else if(num < nLength)
      nRank¡¡<- num
    else
      valid = FALSE;
    hospitalname <- NULL;
    if(valid)
      hospitalname <- newdata[nRank,1]
    else
      hospitalname <- "<NA>";
    stateName = newdata[1,2];
      
    data.frame(hospital = hospitalname, state = stateName, row.names = stateName);
}


rankall <- function(outcome, num = "best"){
  ##Read outcome data
  outcomeData<-read.csv("rprog_data_ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses = "character");

  ##Check that outcome are valid
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia")
    stop("invalid outcome");
  
  ##Return hospital name in that state with the given rank 30-day death rate
  nRate <- 0; 
  if(outcome == "heart attack")
    nRate <- 11
  else if(outcome == "heart failure")
    nRate <- 17
  else if(outcome == "pneumonia")
    nRate <- 23
  outcomeData[,nRate] <- as.numeric(outcomeData[,nRate]);
  outcomeData <- subset(outcomeData, !is.na(outcomeData[,nRate]),select=c(2,7,nRate));
  splitdata <- split(outcomeData,outcomeData$State);
  nState <- length(splitdata);

  rankAllData <- NULL;
  for(i in 1:nState){
    rankData <- getRank(splitdata[[i]],num);
    if(!is.na(rankData))
      rankAllData <- rbind(rankAllData, rankData);
  }
  rankAllData;
  
}