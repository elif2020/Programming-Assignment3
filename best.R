best <- function(state, outcome) {
  ## read outcome data
  alldata<-read.csv("outcome-of-care-measures.csv",
                    na.strings="Not Available",stringsAsFactors=FALSE)
  
  ## check if state is valid
  if(state %in% alldata$State){
    
    #select the rows corresponding to the given state
    id<-which(alldata$State == state)
    odata<-alldata[id,]
    
    #check if outcome is valid
    if(outcome %in% c("heart attack","heart failure","pneumonia")){
      
      #select hospital name and outcome columns
      if(outcome=="heart attack"){
        odata<-odata[,c(2,11)]
      }
      
      if(outcome=="heart failure"){
        odata<-odata[,c(2,17)]
      }
      
      if(outcome=="pneumonia"){
        odata<-odata[,c(2,23)]
      }
    
      odata<-odata[complete.cases(odata),]
      names(odata)<-c("hospital","outcome")
        
      #find min outcome
      mino<-min(odata$outcome)
      hos<-subset(odata$hospital,odata$outcome==mino)
      sorted<-sort(hos)
      
    }
    else{
      stop(print("invalid outcome"))
    }
  }
  else{
    stop(print("invalid state"))
  }
  
  sorted[1]
}