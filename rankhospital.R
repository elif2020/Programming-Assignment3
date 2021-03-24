rankhospital<-function(state,outcome,num="best"){
  
  #read outcome data
  alldata<-read.csv("outcome-of-care-measures.csv",
                    na.strings="Not Available",stringsAsFactors=FALSE)

  if(num=="best"){
    num<-1
  }
  
  ## check if state is valid
  if(state %in% alldata$State){
    
    #select the rows corresponding to the given state
    id<-which(alldata$State == state)
    odata<-alldata[id,]
    
    #check if outcome is valid
    if(outcome %in% c("heart attack","heart failure","pneumonia")){
      
      #select two columns with complete cases
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
        
      #stop if rank is more than the number of hospitals
      
      nhos<-nrow(odata)
      
      if(num=="worst"){
        num<-nhos
      }
      
      if(num>nrow(odata)){
        stop(print(NA))
      }
        
      #order the data
      odata<-odata[with(odata,order(outcome,hospital)),]
      
    }
    
  }
  
  #return hospital name in that state with the given rank
  hos<-odata$hospital
  hos[num]
}