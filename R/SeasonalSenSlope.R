SeasonalSenSlope <- function(x, ValuesToUse = "RawValue",ValuesToUseforMedian=NA, Year = "Year",
                             do.plot=FALSE, mymain=NULL,RawValues=TRUE,UseMidObs=TRUE,
                             TimeIncrMed=TRUE,...) {
  # Calculates the seasonal sen slope and its 90% CIs - for 95% confidence in trend direction.
  # input data of dates and observations must have RawValue and Censored columns produced by RemoveAlphaDetect
  # input data must have observation dates in myDate being a date vector of class "Date"
  # input data must have columns specifying the season (name "Season") and the year  (name "Year")

  AnalysisNote <- GetAnalysisNote(x, ValuesToUse = ValuesToUse,...)   # CHECK TO SEE IF DATA OK TO CONTINUE
  if(AnalysisNote != "ok") { # if not "ok" dont do the test
    Median<-NA; VarS<-NA;   AnnualSenSlope<-NA;  Intercept<-NA;  Lci<-NA;  Uci<-NA;
    Sen_Probability<-NA;  Sen_Probabilitymax <- NA;   Sen_Probabilitymin <- NA;  Percent.annual.change<-NA
    myplot<-NA;
  } else {

    #Tidy data and add on timeincrement - Year column
    x<-GetTimeIncrYear(x,ValuesToUse,Year)
    myFreq<-x$myFreq[1]; x$myFreq<-NULL
    x1<-x ; x1$V1<-x1[,ValuesToUse]#Saved here to use for plotting later

    # For calculating the sen slope, replace "lt" censored values with half the censored values
    if(RawValues){  #We only do this if the data has not been flow adjusted (it gets done prior to FA in the "AdjustValues" function)
      x[x$CenType=="lt",ValuesToUse]<-x[x$CenType=="lt",ValuesToUse]*0.5
      x[x$CenType=="gt",ValuesToUse]<-x[x$CenType=="gt",ValuesToUse]*1.1
    }

    #Take medians or mid-values over each time increment
    Data<-ValueForTimeIncr(x,ValuesToUse,Year,UseMidObs,TimeIncrMed)

    if(is.na(ValuesToUseforMedian)){
      Median <- median(Data$V1, na.rm=T) #The median of the data after summarising to time increments
    }else{
      Median <- median(x[,  ValuesToUseforMedian], na.rm=T) #A catch to get the median of raw data when doing covariate adjustment
    }


    Data$Season<-factor(as.character(Data[, "Season"]),levels=unique(Data[,"Season"])) #Adjust the factors for the seasons for the analysis to allow completely missing seasons

    Data <- Data[order(Data$NewDate), ] # restores the time ordering
    Data$myDate<-Data$NewDate

    TheSlopes <- ddply(Data, c("Season"), function(y) GetInterObservationSlopes(y))
    AnnualSenSlope <- median(TheSlopes$Slopes, na.rm=T) # the slopes are between individual observations that are incremenets X frequency apart in time.
    indexforMedian<-which(abs(TheSlopes$Slopes-AnnualSenSlope)==min(abs(TheSlopes$Slopes-AnnualSenSlope)))
    MySenCen<-as.character(unique(TheSlopes$CensorLabel[indexforMedian]))

    #Provide some warnings about Censored values used in the derivation of the Sen Slope
    if(!all(MySenCen == "not not")){
      if(all(MySenCen  %in% c("lt lt","gt gt","gt lt","lt gt"))){
        AnalysisNote<-"WARNING: Sen slope based on two censored values"
      }else{
        AnalysisNote<-"WARNING: Sen slope influenced by censored values"}
    }else{
      if (AnnualSenSlope==0) AnalysisNote<- "WARNING: Sen slope based on tied non-censored values"
    }

    Data$CensoredOrig<-Data$Censored
    Data$Censored <- FALSE # Note the calculation of VarS and Senslope does not include censored values, but these fields are required by GetKendall
    Data$CenType <- "not"

    # estimate confidence intervals for 1-2*Alpha (where alpha = 0.05)
    VarS <- SeasonalKendall(x=Data, ValuesToUse="V1")$VarS # get the variance using the Seasonal Kendall test function
    Z <- 1-(0.05) # NB, (2*0.05/2) 2 X alpha but alpha/2 as per http://vsp.pnnl.gov/help/Vsample/Nonparametric_Estimate_of_Trend.htm

    ########
    nC2 <-  length(TheSlopes$Slopes)
    RL <- (nC2 - qnorm(Z)*sqrt(VarS))/2       # Rank of lower  confidence limit
    RU <- (nC2 + qnorm(Z)*sqrt(VarS))/2      # Rank of upper confidence limit

    RankOfSlopes <- 1:nC2

    ConfInts <- approx(x=RankOfSlopes, y=sort(TheSlopes$Slopes), xout = c(RL, RU))$y
    Lci <- ifelse(is.na(ConfInts[1]),min(TheSlopes$Slopes),ConfInts[1])
    Uci <- ifelse(is.na(ConfInts[2]),max(TheSlopes$Slopes),ConfInts[2])

    # calculate the probability that the slope was truly below zero
    # rank of slope zero by interpolation
    R0 <- approx(y=RankOfSlopes, x=sort(TheSlopes$Slopes), xout = 0,ties=median)$y
    Z1minusAlpha <- (2*R0 - nC2)/sqrt(VarS)
    #BUT if all slopes are either negative or positive, this fails, so put in a catch
    if (sum(TheSlopes$Slopes<0)==length(TheSlopes$Slopes)){R0<-max(RankOfSlopes)}
    if (sum(TheSlopes$Slopes>0)==length(TheSlopes$Slopes)){R0<-min(RankOfSlopes)}

    R0max <- approx(y=RankOfSlopes, x=sort(TheSlopes$Slopes), xout = 0,ties=max)$y
    Z1minusAlphamax <- (2*R0max - nC2)/sqrt(VarS)
    R0min <- approx(y=RankOfSlopes, x=sort(TheSlopes$Slopes), xout = 0,ties=min)$y
    Z1minusAlphamin <- (2*R0min - nC2)/sqrt(VarS)

    # The probability of slope being less than zero is
    Sen_Probability <- pnorm(Z1minusAlpha)
    Sen_Probabilitymax <- pnorm(Z1minusAlphamax)
    Sen_Probabilitymin <- pnorm(Z1minusAlphamin)

    # get intercept.
    Time<-(1:length(Data$V1))-1 # need all dates despite some being censored.
    Ymed <-  median(Data$V1, na.rm=T) # the median of the measurements that are used to compute slope.
    Tmed <- as.numeric(max(Data$NewDate)-min(Data$NewDate))/365.25/2  # the median of the time
    Intercept <- Ymed - (AnnualSenSlope*Tmed)

    Percent.annual.change = AnnualSenSlope/abs(Median)*100 #using abs of median as sometimes median is -ve after flow adjustment


    if(do.plot==TRUE) {
      myTimeIncr<-WhatistheIncr(x,"TimeIncr")
      mySeason<-ifelse(myTimeIncr=="Annual",NA,WhatistheIncr(x,"Season"))
      myplot<-GGPlotTrend(Data,x1,Intercept=Intercept,AnnualSenSlope=AnnualSenSlope,Lci=Lci,Uci=Uci,mymain=mymain,ValuesToUse = "V1",
                          IsSeasonal=TRUE,myTimeIncr=myTimeIncr,mySeason=mySeason,Ymed=Ymed,Tmed=Tmed,Percent.annual.change=Percent.annual.change,UseMidObs=UseMidObs,...)

    }
    # } # end of second if-else statement
  }  # end of first if-else statement

  output<-data.frame(Median=Median, Sen_VarS=VarS, AnnualSenSlope=AnnualSenSlope, Intercept=Intercept,
                     Sen_Lci=Lci, Sen_Uci=Uci, AnalysisNote=AnalysisNote,
                     Sen_Probability=Sen_Probability, Sen_Probabilitymax = Sen_Probabilitymax,  Sen_Probabilitymin = Sen_Probabilitymin,
                     Percent.annual.change=Percent.annual.change)
  if(do.plot==TRUE&!is.na(output$AnnualSenSlope)){output<-list(output,myplot)}
  return(output)
}
