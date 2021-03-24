rankall<-function(outcome,num="best"){


  #read outcome data
  alldata<-read.csv("outcome-of-care-measures.csv",
                    na.strings="Not Available",stringsAsFactors=FALSE)
  
  if(num=="best"){
    num<-1
  }
  
  if(outcome=="heart attack"){
    alldata<-alldata[,c(2,7,11)]
  }
  if(outcome=="heart failure"){
    alldata<-alldata[,c(2,7,17)]
  }
  if(outcome=="pneumonia"){
    alldata<-alldata[,c(2,7,23)]
  }
  
  names(alldata)<-c("hospital","state","outcome")
  alldata<-alldata[with(alldata,order(state,outcome,na.last=NA)),]
  alldata<-alldata[with(alldata,order(state,outcome,hospital)),]

  ralldata<-data.frame()
 
  for(s in unique(alldata$state)){
      nhos<-sum(alldata$state==s)
      if(num=="worst"){
        num<-nhos
      }
      if(num>nhos){
        hos<-NA
      }
      else{
        hosnames<-subset(alldata$hospital,alldata$state==s)
        hos<-hosnames[num]
        
        if(num==nhos){
          num<-"worst"
        }
      }
      newrow<-c(hos,s)
      ralldata<-rbind(ralldata,newrow)
  }
  
  names(ralldata)<-c("Hospital","State")
  ralldata
}