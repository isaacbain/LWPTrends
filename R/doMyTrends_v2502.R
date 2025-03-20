doMyTrends_v2502<-function(x,TimeIncrOpts=NA,do.plot=F,
                           propIncrTol=0.9,propYearTol=0.9,AnnualOK=c("MCI"),
                           TrendPeriod=NA,EndYear=NA,Year="Year",
                           ...){ # elipsis { ... } passes arguments to the internal functions. (e.g., Year="Year",HiCensor=F,ValuesToUse = "RawValues", do.plot=F)

  #Process data to cut down to the trend period, and add an appropriate time increment
  x<-InspectTrendData(x,Year=Year,TrendPeriod = TrendPeriod,EndYear=EndYear,propIncrTol=propIncrTol,
                      propYearTol=propYearTol,AnnualOK = AnnualOK,TimeIncrOpts=TimeIncrOpts)

  #Does the site meet minimum data requirements?
  if(is.null(dim(x))){ #Then there are no observations in the trend period
    return(NULL)
  }else if(x$TimeIncr[1]=="none"){ #then there are not enough observations in the trend period
    return(NULL)
  }else{

    x<-GetSeason(x,do.plot=FALSE,...)[[1]]
    # print(MySeasonality)
    if(x$Seasonal[1]==FALSE){ #Then we will perform non-seasonal trend tests
      OUTPUTS<-NonSeasonalTrendAnalysis(x,do.plot=do.plot,...)
    }else{ #Otherwise it's seasonal
      OUTPUTS<-SeasonalTrendAnalysis(x,do.plot=do.plot,,...)
    }
    if(is.data.frame(OUTPUTS)&do.plot){ #This is when you have asked for plots, but the trend assessment is not analysed
      OUTPUTS<-list(OUTPUTS,NA)
    }
    return(OUTPUTS)
  }}
