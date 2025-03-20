MannKendall <- function(x, ValuesToUse = "RawValue", Year = "Year",
                        RawValues=TRUE,UseMidObs=TRUE,TimeIncrMed=TRUE,...) { #
  # input data of dates and observations must have RawValue and Censored columns produced by RemoveAlphaDetect
  # input data must have Observation dates in myDate being a date vector of class "Date"
  # input data must have columns specifying the time increment (name =  "TimeIncr") and the year (name = "Year" )

  # CHECK TO SEE IF DATA IS OK TO CONTINUE
  AnalysisNote <- GetAnalysisNote(x,ValuesToUse)

  if(AnalysisNote !="ok") { # if any TRUE dont do the test
    KendalTest <- data.frame(nObs = nrow(x), VarS=NA, S = NA, D = NA, tau = NA, Z=NA, p=NA,C=NA,Cd=NA)
  } else {

    #..............................................................................#
    #Tidy data and add on TimeInceYear column
    x<-GetTimeIncrYear(x,ValuesToUse,Year)
    x$Season<-x$TimeIncr #This is just a catch so the a different season is not sent through the analysis
    x[,ValuesToUse]<-as.numeric(x[,ValuesToUse])

    #Take medians over each time increment if required
    Data<-ValueForTimeIncr(x,ValuesToUse,Year,UseMidObs,TimeIncrMed)

    #..............................................................................#

    # CHECK TO SEE IF DATA OK TO CONTINUE
    AnalysisNote <- GetAnalysisNote(Data,ValuesToUse="V1",SecondTierTest = TRUE)

    if(AnalysisNote != "ok" ) { # if any TRUE dont do the test
      KendalTest <- data.frame(nObs = nrow(x),nTimeIncr = nrow(Data), VarS=NA, S = NA, D = NA,tau = NA, Z=NA, p=NA,C=NA,Cd=NA)
    } else {

      # organise the ordering of data
      Data <- Data[order(Data$NewDate), ] # restores the time ordering

      KendalTest <- GetKendal(x = Data)
      KendalTest$C<-1-KendalTest$p/2
      KendalTest$Cd<-KendalTest$C
      KendalTest$Cd[which(KendalTest$S>0)]<-KendalTest$p[which(KendalTest$S>0)]/2
      names(KendalTest)[names(KendalTest)=="vars"]<-"VarS"
      KendalTest<-cbind(data.frame(nObs=nrow(x)),as.data.frame(KendalTest))
      if(is.na(KendalTest$C)==T) AnalysisNote = "Not Analysed"
    }
  }  # end of first if statement

  KendalTest$AnalysisNote<-AnalysisNote
  KendalTest$prop.censored <- sum(x$Censored)/nrow(x) # proportion of values that were censored
  KendalTest$prop.unique <- length(unique(x[, ValuesToUse]))/nrow(x) # proportion of values that were censored
  KendalTest$no.censorlevels<-length(unique(x[x$Censored=="TRUE",ValuesToUse]))
  KendalTest$TimeIncr<-WhatistheIncr(x,"TimeIncr")
  KendalTest$SeasIncr<-"NonSeasonal"
  # KendalTest$HiCensorNote<-HiCensor

  return(KendalTest)
}
