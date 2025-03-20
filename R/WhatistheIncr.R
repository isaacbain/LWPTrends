WhatistheIncr<-function(x,val="TimeIncr"){
  #This is a function to determine what is the time increment type, just to make nice titles in plots
  # NtheTimeIncr<-length(levels(x[,val]))
  ind<-setdiff(1:ncol(x),which(names(x) %in% c("TimeIncr","Season")))
  mycol<-names(x)[ind[sapply(ind,function(y) identical(as.character(x[,val]),as.character(x[,y])))]]

  if(mycol %in% c("Year","CustomYear")){
    myTimeIncr="Annual"
  }else{
    myTimeIncr=SeasonLabs$mySeas[SeasonLabs$SeasName==mycol]
  }
  return(myTimeIncr)
}
