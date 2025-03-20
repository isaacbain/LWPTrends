#' GetSeason
#'
#' GetSeason evaluates an appropriate season for the analysis by performing a
#' Kruskal Wallis test (non-parametric ANOVA, via the SeasonalityTest()
#' function) on the observations using the possible time increment as the
#' explanatory (categorical) variable. Possible season time increments are (1)
#' the time increment specified by TimeIncr, and (2) any other time increment
#' that is a whole multiple of TimeIncr. This function also returns the
#' SeasonalityTest() outputs for the selected season (i.e., the user does not
#' need to run SeasonalityTest() separately).
#'
#' @param x Input data frame must contain: TimeIcnr, ValuesToUse, Centype, Censored.
#' @param ValuesToUse Column name with data to use in the test. (Default: "RawValue")
#' @param printKW Logical. If TRUE the function prints a table of the Kruskal Wallis test outputs
#' for all tested seasons to the console.
#' @param do.plot Logical. Option to return a ggplot object of the boxplots for the chosen season
#' (Default: TRUE).
#' @param ... Passed to the plot function.
#'
#' @return A list containing (1) A data frame with fields as follows (in addition to those columns in x), (2) the outputs from SeasonalityTest() and optionally (3) a plot object.\tabular{ll}{
#'    \code{Season} \tab Season string (factor) \cr
#'    \tab \cr
#'    }
#'
#' @export
#'
GetSeason<-function(x,ValuesToUse = "RawValue",RawValues=TRUE,printKW=FALSE,do.plot=FALSE,
                    UseMidObs=TRUE,TimeIncrMed=TRUE,...){

  x<-x[!is.na(x[,ValuesToUse]),]

  minSeas<-WhatistheIncr(x,val="TimeIncr") #Just get the name of the minimum time increment

  if(minSeas!="Annual"){
    StartSeas<-which(SeasonLabs$mySeas==minSeas)
    mylist<-list()
    for(i in TimeIncrToSeas[[minSeas]]){
      x$Season<-x[,SeasonLabs$SeasName[SeasonLabs$mySeas==i]]
      mylist[[SeasonLabs$SeasName[SeasonLabs$mySeas==i]]]<-SeasonalityTest(x,do.plot=FALSE,ValuesToUse = ValuesToUse,RawValues = RawValues,
                                                                           UseMidObs=UseMidObs,TimeIncrMed=TimeIncrMed,...)
    }

    MyTab<-ldply(mylist)
    if(printKW){
      print(MyTab)
    }

    #Look for the smallest possible time increment that is seasonal and set this to season.
    # if none are seasonal, set to the TimeIncr
    if(any(MyTab$p<0.05&MyTab$SeasNote=="ok")){
      MyTab<-MyTab[MyTab$SeasNote=="ok"&MyTab$p<0.05,]
      MyTab<-MyTab[order(MyTab$KWstat,decreasing = TRUE),]
      x$Season<-x[,MyTab$.id[1]]

      temp1<-SeasonalityTest(x,do.plot=do.plot,ValuesToUse = ValuesToUse,RawValues = RawValues,
                             UseMidObs=UseMidObs,TimeIncrMed=TimeIncrMed,...)
      x$Seasonal=TRUE
    }else{
      x$Season<-x$TimeIncr
      x$Seasonal=FALSE
      temp1<-SeasonalityTest(x,do.plot=do.plot,ValuesToUse = ValuesToUse,RawValues = RawValues,
                             UseMidObs=UseMidObs,TimeIncrMed=TimeIncrMed,...)
    }

  }else{ #Then we have an annual time step and we can't do seasonality test
    x$Season<-x$TimeIncr
    x$Seasonal<-FALSE
    temp1<-"Annual time increment"
  }
  out<-list()
  out[[1]]<-x
  if(class(temp1)=="list"){
    out[[2]]<-temp1[[1]]
    out[[3]]<-temp1[[2]]
  }else{out[[2]]<-temp1}
  return(out)
}
