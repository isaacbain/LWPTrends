#' SeasonalityTest
#'
#' The function SeasonalityTest() tests if the data are seasonal. The SeasonalityTest() function
#' performs a Kruskal Wallis test (non-parametric ANOVA) on the observations using season as the
#' explanatory (categorical) variable. The function also optionally produces a box plot of the data
#' grouped by season.
#'
#' @param x Input data frame must contain, Season, ValuesToUse,Centype, Censored
#' @param ValuesToUse Select Column with data to use in the test
#' @param HiCensor
#' @param main
#' @param Year
#' @param do.plot Produces a ggplot object in a list if TRUE
#' @param mymain
#' @param RawValues
#' @param UseMidObs
#' @param TimeIncrMed
#' @param ... Passed to plot function
#'
#' @return A data frame and (optionally) a plot. If do.plot set to TRUE then a list is returned, with the first
#' item the data frame and the second is the boxplot. Data frame fields as follows\tabular{ll}{
#'    \code{Observations} \tab Number of observations \cr
#'    \tab \cr
#'    \code{KWStat} \tab the Kruskal-Wallis rank sum statistic \cr
#'    \tab \cr
#'    \code{pvalue} \tab The p-value for the test (null hypothesis is that the data is NOT seasonal) \cr
#'    \tab \cr
#'    \code{TimeIncr} \tab The time increment of the dataset \cr
#'    \tab \cr
#'    \code{Season} \tab The season (time increment) used for the seasonality test \cr
#' }
#'
#' @export
#'
SeasonalityTest <-  function(x, ValuesToUse = "RawValue", HiCensor=FALSE,main=NULL,Year="Year",
                             do.plot=FALSE,mymain=NA,RawValues=TRUE,UseMidObs=TRUE,TimeIncrMed=TRUE,...) { # x = DataForTest


  #Stop if season is not defined
  if(is.null(x$Season[1])){stop("Season must be defined", call. = FALSE)}
  #
  #Stop if season is not a multiple of time increment
  if(!is.null(x$Season[1])){
    check<-ddply(x,.(TimeIncr),function(x) data.frame(myCount=length(unique(x$Season))))
    if(max(check$myCount>1)){stop("Season must be a multiple of the time increment", call. = FALSE)}}

  Observations <- sum(!is.na(x[, ValuesToUse]))
  mySeas<-WhatistheIncr(x,"Season")
  myTimeIncr<-WhatistheIncr(x,"TimeIncr")
  x<-GetTimeIncrYear(x,ValuesToUse,Year)

  x<-ValueForTimeIncr(x,ValuesToUse=ValuesToUse,Year=Year,UseMidObs=UseMidObs,TimeIncrMed = TimeIncrMed)
  # if(HiCensor==TRUE|is.numeric(HiCensor)) x<-ApplyHiCensor(x,"V1",HiCensor,RawValues)
  # CHECK TO SEE IF DATA OK TO CONTINUE
  AnalysisNote <- GetAnalysisNote(x,ValuesToUse="V1",IsSeasonal = TRUE,SecondTierTest = TRUE)

  if(Observations > 5 & length(unique(x[, "V1"]))>1&AnalysisNote=="ok") { # check there is minimum data and it has some variation

    #If required, apply the hi-censor


    if(do.plot==TRUE){
      x$Value<-x[, "V1"]
      thetitle<-paste0(ifelse(is.na(mymain),"Seasonality plot",paste0(mymain,": Seasonality plot")),"\n",
                       myTimeIncr," time increments, ",mySeas," seasons")

      myplot<-ggplot(x,aes(y=Value,x=Season))+geom_boxplot(fill="lightblue")+
        theme_bw()+ggtitle(thetitle)+theme(plot.title = element_text(hjust = 0.5))
    }

    if(length(table(x[, "Season"][!is.na(x[, "V1"])]))>1)
    { # check there is more than one season represented
      #if(!is.factor(x$Season))  x$Season<-factor(as.character(x[, "Season"]),levels=unique(x[,"Season"]))

      KW <- kruskal.test(x[, "V1"] ~ factor((x[, "Season"])))
      OUT <- data.frame(Observations=Observations, KWstat = KW$statistic,
                        pvalue = KW$p.value,SeasNote=AnalysisNote,
                        TimeIncr=myTimeIncr,Season=mySeas)

      if(do.plot==TRUE){
        mybox<-paste("Observations =", Observations,
                     "\nKruskal Wallis statistic = ", round(OUT$KWstat,3),
                     "\nP value =", round(OUT$pvalue,3))
        myplot<-myplot +
          geom_label(label=mybox,x=-Inf, y=Inf,fill="grey95",label.size=0,vjust="inward",
                     hjust=0,label.padding=unit(0.7,"lines"),label.r=unit(0,"lines"))

        OUT<-list(OUT,myplot)
      }
    } else {
      OUT <-data.frame(Observations=Observations, KWstat = NA, pvalue = NA,SeasNote=AnalysisNote,TimeIncr=myTimeIncr,Season=mySeas)
    }

  } else {
    OUT <- data.frame(Observations=Observations, KWstat = NA, pvalue = NA,SeasNote=AnalysisNote,TimeIncr=myTimeIncr,Season=mySeas)
  }
  OUT$TimeIncr<-myTimeIncr
  OUT$Season<-mySeas
  return(OUT)
}
