ValueForTimeIncr <-function(x, ValuesToUse = "RawValue",Year="Year",UseMidObs=TRUE,TimeIncrMed=TRUE){
  if(is.null(x$Season)){x$Season<-x$TimeIncr} #Just a catch in case season has not been assigned yet
  # this takes the median of observations and DATES in time increment (so a time increment only has one value)
  # if the median of the raw values is less than OR EQUAL to the max censored value, call the value censored, otherwise NOT censored.

  #If the "UseMidObs" variable is set to "TRUE" - the observation that is closest in time to the middle of the time increment is used (rather
  # than takin the median of all observations over time increments).  This variant is appropriate to use when there has a been a systematic change
  # in sampling interval over the trend period
  if(TimeIncrMed){
    if (UseMidObs){
      #Determine mid-dates for time increments.  NOTE, this will only work if seasons are based on Months and all time increments are the same
      #nubmer of months long
      nTimeIncr=length(levels(x$TimeIncr));nTimeIncr=max(nTimeIncr,1)
      midTimeIncr=data.frame(theMidDate=MidTimeIncrList[[nTimeIncr]])
      midTimeIncr$TimeIncr<-levels(x$TimeIncr)

      x<-merge(x,midTimeIncr)

      Data <-  ddply(x, "TimeIncrYear", function(y) {
        if(length(y$TimeIncrYear)>1){
          midTimeIncrdate<-as.Date(paste0(y$Year[1],"-01-01"))+y$theMidDate[1]
          #Find date closest to the middle of the time increment
          theid<- which(abs(y$myDate-midTimeIncrdate)==min(abs(y$myDate-midTimeIncrdate)))[1]
        }else{
          theid<-1
        }

        return(data.frame(V1 = y[theid,ValuesToUse], NewDate = y$myDate[theid], Censored = y$Censored[theid], Year=y[theid,Year],
                          Month=y[theid,"Month"], BiMonth=y[theid,"BiMonth"],Qtr=y[theid,"Qtr"],BiAnn=y[theid,"BiAnn"],
                          TimeIncr=y$TimeIncr[theid],Season=y$Season[theid],CenType=as.character(y$CenType[theid])))
      })

    }else{
      Data <-  ddply(x, "TimeIncrYear", function(y) {
        Median <- median(y[, ValuesToUse]) # get the median of the values
        NewDate <- median(y[, "myDate"]) # get the median of the dates
        if(sum(y$Censored) == 0) {
          Censored = FALSE
        } else {
          MaxCensored <- max(y[, ValuesToUse][y$Censored],na.rm = T)
          Censored <- ifelse(Median <= MaxCensored, TRUE, FALSE)
        }
        return(data.frame(V1 = Median, NewDate = NewDate, Censored = Censored, Year=y[1,Year],
                          Month=y[1,"Month"], BiMonth=y[1,"BiMonth"],Qtr=y[1,"Qtr"],BiAnn=y[1,"BiAnn"],
                          TimeIncr=y$TimeIncr[1],Season=y$Season[1]))
      })
      # this determines censor type for Data  based on CenType that has highest prevalence in the time increment
      Data$CenType <- ddply(x, "TimeIncrYear", function(y) names(which.max(table(y$CenType))))[,2]#
    }

  }else{
    Data=(data.frame(TimeIncrYear=as.character(x[,"TimeIncrYear"]),V1 = x[,ValuesToUse], NewDate = x$myDate,
                     Censored = as.character(x$Censored),
                     Year=x[,Year], TimeIncr=as.character(x$TimeIncr),Season=as.character(x$Season),CenType=as.character(x$CenType)))
  }
  return(Data)
}
