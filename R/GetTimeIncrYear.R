GetTimeIncrYear<-function(x,ValuesToUse = "RawValue",Year="Year"){

  x <- x[order(x$myDate), ]  # make sure the data are in date order!
  if(length(unique(x$TimeIncr))==1){
    x$TimeIncrYear<-x[[Year]]
    x$myFreq<-1
  }else{
    x$TimeIncrYear <- paste(x[["TimeIncr"]], x[[Year]], sep = "-")
    x$myFreq<-length(unique(x$TimeIncr))
  }
  take <- !is.na(x[, ValuesToUse]) & !is.na(x$Censored)  # remove values and censored values that are NA.
  x <- x[take, ] # drop the NA values
  return(x)
}
