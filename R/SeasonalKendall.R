SeasonalKendall <- function(x, ValuesToUse = "RawValue", Year = "Year", RawValues=TRUE,
                            UseMidObs=TRUE,TimeIncrMed=TRUE, ...) {
  # Calculates the seasonal Kendall test
  # input data of dates and observations must have RawValue and Censored columns produced by RemoveAlphaDetect
  # input data must have observation dates in myDate being a date vector of class "Date"
  # input data must have columns specifying the season (name =  "Season") and the year (name = "Year" )

  AnalysisNote <- GetAnalysisNote(x,ValuesToUse)   # CHECK TO SEE IF DATA OK TO CONTINUE

  if(AnalysisNote != "ok") { # if any TRUE dont do the test and return NA values
    VarS<-NA; S<-NA; D<-NA; tau<-NA;  Z<-NA; p<-NA; n<-length(nrow(x)) ;C=NA; Cd=NA;SeasIncr=NA
  } else {

    #Tidy data and add on tine increment-year column
    x<-GetTimeIncrYear(x,ValuesToUse,Year)

    myTimeIncr<-WhatistheIncr(x,val="TimeIncr")
    mySeas<-WhatistheIncr(x,val="Season")
    x[,ValuesToUse]<-as.numeric(x[,ValuesToUse])

    #Take medians over each time increment
    Data<-ValueForTimeIncr(x,ValuesToUse,Year,UseMidObs,TimeIncrMed)
    # CHECK TO SEE IF DATA OK TO CONTINUE
    AnalysisNote <- GetAnalysisNote(Data,ValuesToUse = "V1",IsSeasonal = TRUE,SecondTierTest = TRUE)

    if(AnalysisNote != "ok") { # dont do the test if not sufficient variation, non censored values or long runs
      VarS<-NA; S<-NA;  D<-NA;  tau<-NA;  Z<-NA;  p<-NA;  n<-nrow(Data);C=NA;Cd=NA;
    } else {


      Data <- Data[order(Data$NewDate), ] # restores the time ordering

      # take each season, and compute kendall statistics
      thisSeason <- by(Data, Data$Season, function(y) {  # for each season y = Data[Data$Season == "Jan",]
        GetKendal(x = y)
      })

      # sum kendall statistics over seasons
      S <- sum(sapply(thisSeason, function(r) return(r[["S"]])), na.rm=T) # nb put na remove here - some seasons do not have enough replicates to estimate S and return NaN from GetKendall
      VarS <- sum(sapply(thisSeason, function(r) return(r[["vars"]])))
      D <- sum(sapply(thisSeason, function(r) return(r[["D"]])))
      tau <- S/D
      n <- nrow(Data) # total observations

      if (n >= 10 & !is.na(VarS)&VarS>0) {
        SigmaS <- VarS^0.5
        if(S > 0)  Z <- (S-1)/SigmaS
        if(S == 0) Z <- 0
        if(S < 0)  Z <- (S+1)/SigmaS
        if(Z > 0)  p <- pnorm(Z, lower.tail = F)*2
        if(Z == 0) p <- 1
        if(Z < 0)  p <- pnorm(Z, lower.tail = T)*2

        C<-1-p/2
        Cd<-C
        Cd[which(S>0)]<-p[which(S>0)]/2
      } else {
        AnalysisNote="Insufficent data to complete Seasonal Mann Kendall"
        Z <- NA
        p <- NA
        C<-NA
        Cd<-NA
      }
    } # end of second if-else
  } # end of first if-else
  prop.censored <- sum(x$Censored)/nrow(x) # proportion of values that were censored
  prop.unique <- length(unique(x[, ValuesToUse]))/nrow(x) # proportion of values that were censored
  no.censorlevels<-length(unique(x[x$Censored=="TRUE",ValuesToUse]))
  return(data.frame(nObs=nrow(x),nTimeIncr=n, S=S, VarS=VarS, D=D,tau=tau, Z=Z, p=p,C=C, Cd=Cd,
                    AnalysisNote=AnalysisNote,prop.censored=prop.censored,prop.unique=prop.unique,no.censorlevels=no.censorlevels,
                    TimeIncr=myTimeIncr,SeasIncr=mySeas))
}
