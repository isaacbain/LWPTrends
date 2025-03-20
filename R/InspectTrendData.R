#' InspectTrendData
#'
#' InspectTrendData prepares the data for subsequent trend analysis. This involves cutting down
#' the dataset to a specified trend period (specified by an end year and a trend period length in
#' years) and adds on the time increment column for the trend analysis, which is selected as the
#' lowest frequency increment that that meets the minimum data requirements specified (see
#' Snelder et al. 2022 for guidance on minimum data requirements). The function can also provide
#' optional summary statistics and graphs.
#'
#' @param x Input data frame must contain: myDate, RawData, Censored
#' @param Year Vector of acceptable (sub-annual) time increments. Default is NA, which uses all coded time increments c("Monthly","Bi-monthly","Quarterly","Bi-annual") – but a subset of these can alternatively be specified.
#' @param TrendPeriod Column name for Year data to be used
#' @param EndYear Sets the end year of the time series. (If NA, will be selected from data)
#' @param propIncrTol The proportion of time increments x years that must have observations
#' @param propYearTol The proportion of year that must have observations
#' @param ReturnALLincr Logical. If true, only returns x with an additional column indicating the time increment.
#' @param AnnualOK A vector of variables which are allowed to have the time increment set as annual.
#' @param TimeIncrOpts Vector of acceptable (sub-annual) time increments. Default is NA, which uses all coded time increments c("Monthly","Bi-monthly","Quarterly","Bi-annual") – but a subset of these can alternatively be specified.
#' @param do.plot Produce a plot if TRUE and ReturnAllIncr is TRUE
#' @param UseMidObs Default is TRUE and uses value closest to the mid-point of the season. Set to FALSE, to use median of time increments.
#' @param FlowtoUse Name of column of flow data (only required if relevant)
#' @param ... Passed to plot function
#'
#' @return A data frame of x with additional columns appended. Optionally a list with an extra dataframe and optionally a plot. Data frame appended fields as follows\tabular{ll}{
#'    \code{Incr} \tab The time increment name (one row of data per time increment) \cr
#'    \tab \cr
#'    \code{TrendPeriodL} \tab Length of trend period \cr
#'    \tab \cr
#'    \code{Nobs} \tab Total number of observations \cr
#'    \tab \cr
#'    \code{nYear} \tab The number of years with data in the sampling period \cr
#'    \tab \cr
#'    \code{propYear} \tab Proportion of trend period years with observations \cr
#'    \tab \cr
#'    \code{nIcnrYear} \tab Number of time increment x year with observations \cr
#'    \tab \cr
#'    \code{propIncrYear} \tab Proportion of total time increment x year for the trend period with observations \cr
#'    \tab \cr
#'    \code{propCen} \tab Proportion of observations that are censored \cr
#'    \tab \cr
#'    \code{nCenLevelsLT} \tab Total number of unique non-detect censor levels (data with <) \cr
#'    \tab \cr
#'    \code{nCenLevelsT} \tab Total number of unique detection limits (data with >) \cr
#'    \tab \cr
#'    \code{nFlow} \tab Proportion of observations with simultaneous flow observations \cr
#' }
#'
#' @export
#'
InspectTrendData <- function(x,
                             Year = "Year",
                             TrendPeriod = NA, EndYear = NA, propIncrTol=0.9,propYearTol=0.9,
                             ReturnALLincr=FALSE,AnnualOK=c("MCI"),TimeIncrOpts=NA,
                             do.plot = FALSE, UseMidObs=TRUE,FlowtoUse=NA,
                             ...) { # ... arguments passed to the plots

  #The purpose of this function is to (1) prepare the data for subsequent trend analysis, but cutting down to the
  # specified trend period, and determining the time increment for analysis
  # And (2) optionally to provide metadata about data availability for ALL possible time increments and
  # (3) optionally to make plots to demonstrate the data availability

  #When ReturnALLincr is FALSE, only the original data set is returned with a new column "TimeIncr" specifying the
  # highest frequency time increment that meets the minimum data requirements given by propIncrtol (the proportion of
  # time increments that must have at least one observation) and propYearTol (the proportion of time "years" in the trend
  # period that must have at least one observation)



  if(!is.na(FlowtoUse)){
    x$Flow<-x[,FlowtoUse]
  }

  #If the start and end year are not provided, automatically extract from dataset
  if (is.na(EndYear)) EndYear<-max(x[,Year])
  if (is.na(TrendPeriod)) TrendPeriod<-EndYear-min(x[,Year])+1
  #Get Date range:
  if(levels(x$Month)[1]=="Jan"){
    EndDate<-as.Date(paste0(EndYear+1,"-01-01"))-1
    StartDate<-as.Date(paste0(EndYear-TrendPeriod+1,"-01-01"))
  }else{

    EndDate<-as.Date(paste0(EndYear,"-",levels(x$Month)[1],"-01"),format="%Y-%b-%d")-1
    StartDate<-as.Date(paste0(EndYear-TrendPeriod,"-",levels(x$Month)[1],"-01"),format="%Y-%b-%d")
  }

  x <- x[x[["myDate"]] >= StartDate & x[["myDate"]] <= EndDate, ] # trims down the data to the year range
  x<-x[!is.na(x$RawValue),] #Get rid of any rows where the observation is NA
  if(nrow(x)>0){

    #Make data availablity summary table
    if(is.character(TimeIncrOpts)){checkSeas<-SeasonLabs$SeasName[SeasonLabs$mySeas %in% TimeIncrOpts]
    }else{
      checkSeas<-SeasonLabs$SeasName}
    if("analyte" %in% colnames(x)){if(x$analyte[1] %in% AnnualOK){ checkSeas<-c(checkSeas,Year)}}


    DataAvailability<-ldply(checkSeas,function(y,Data=x){
      Data$IncYear<-paste(Data[[y]], Data[[Year]], sep = "-")
      out<-data.frame(Incr=y,
                      TrendPeriodL=TrendPeriod,
                      nobs=nrow(Data),
                      nYear=length(unique(Data[[Year]])),
                      propYear=length(unique(Data[[Year]]))/TrendPeriod,
                      nIncrYear=length(unique(Data$IncYear)),
                      propIncrYear=length(unique(Data$IncYear))/(TrendPeriod*length(levels(Data[,y]))))
      if(y==Year){out$propIncrYear=out$propYear}
      return(out)
    })

    DataAvailability$propCen <- sum(x$Censored[!is.na(x$RawValue)])/nrow(x)
    DataAvailability$nCenLevelsLT<-length(unique(x$RawValue[x$CenType=="lt"]))
    DataAvailability$nCenLevelsGT<-length(unique(x$RawValue[x$CenType=="gt"]))
    if(!is.null(x$Flow)) {
      DataAvailability$nFlow <- sum(!is.na(x$Flow[!is.na(x[,"RawValue"])]))
    } else {  DataAvailability$nFlow = 0 }

    DataAvailability$DataOK<-FALSE
    DataAvailability$DataOK[DataAvailability$propYear>=propYearTol&
                              DataAvailability$propIncrYear>=propIncrTol]<-TRUE

    #Select the highest frequency increment that meets the data requirements
    if(any(DataAvailability$DataOK)){
      mytimeincr<-DataAvailability$Incr[which(DataAvailability$DataOK==TRUE)[1]]
      x$TimeIncr<-x[,mytimeincr] #Assign as time increment for the analysis
    }else{ #If none are ok, then we leave TimeIncr as a note ot remove from analysis
      x$TimeIncr<-"none"
    }
    if (ReturnALLincr==TRUE){
      output<-list(x,DataAvailability)

      if(do.plot==TRUE){
        myYears <-  min(x[,"Year"]):max(x[,"Year"])
        #
        YearsInPeriod <- length(unique(x[,Year]))

        myTimeIncrYearFrame <- expand.grid(list(TimeIncr = NumMonthString, Year = myYears))
        myTimeIncrYearFrame$TimeIncrYear <- paste(myTimeIncrYearFrame$TimeIncr, myTimeIncrYearFrame$Year, sep = "-")
        myTimeIncrYearFrame$TimeIncrYear <- factor(myTimeIncrYearFrame$TimeIncrYear, levels = myTimeIncrYearFrame$TimeIncrYear)

        x$TimeIncrYear <- paste(x[["Month"]], x[["Year"]], sep = "-")
        x$TimeIncrYear <- factor(x$TimeIncrYear, levels = myTimeIncrYearFrame$TimeIncrYear)
        x$TimeIncr<-x$Month
        # this takes the median or mid-point of observations in mont (so a month only has one value)
        # if the median of the raw values is less than OR EQUAL to the max censored value, call the value censored, otherwise NOT censored.
        nValperTimeIncr<-ddply(x,.(TimeIncrYear),function(x) data.frame(nVal=nrow(x)))

        Data<-ValueForTimeIncr(x, ValuesToUse = "RawValue",Year="Year",UseMidObs)
        Data<-merge(Data,nValperTimeIncr)

        # this determines censor type for Data  based on CenType that has highest prevalence in the time increment
        Data$CenType <- ddply(x, "TimeIncrYear", function(y) names(which.max(table(y$CenType))))[,2]
        myTimeIncrYearFrame$Row <- 1:nrow(myTimeIncrYearFrame)
        PlotData <- merge(myTimeIncrYearFrame, Data, all=TRUE) # this over-writes to include all years and TimeIncrs specified by StartYear & EndYear
        PlotData<-PlotData[order(PlotData$Row),]

        output[[3]]<-InspectData_GGPlots(PlotData=PlotData,DateRange=c(StartDate,EndDate),Year="Year",...)#mymain=mymain,Year="Year",
      }
    }else{
      output<-x
    }
    return(output)
  }else{return(NULL)}

}
