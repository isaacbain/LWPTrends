#' GetMoreDateInfo
#'
#' Takes dates and produces additional columns summarising, months, bimonths
#' (samples ever 2 months), quarters, biannual (samples twice a year) and years.
#' If specified, the firstMonth can be used to shift the analysis year. This
#' also automatically shifts the factor levels of the months and quarters to
#' start from this month. The function also as an option to search for
#' observations that should be representative of a sample from the following (or
#' previous) month, and assign the additional date information associated with
#' the following (or previous) month, rather than the month of sampling.\cr\cr The
#' outputs from this function can be then used to specify a season for the
#' analysis. season definition would need to be manually shifted to reflect the
#' custom year, by the user.
#'
#' @param Data Dataframe or vector containing myDate
#' @param firstMonth Specify the first month (numeric (1:12)) for the analysis year
#' @param FindDateShifts Logical. Indicates whether to look for observations that should be
#'                       representative of previous or following months.
#'
#' @return A data frame with fields as follows (in addition to those columns in x)\tabular{ll}{
#'    \code{Year} \tab Calendar Year (numeric) \cr
#'    \tab \cr
#'    \code{CustomYear} \tab Custom Year (matches Calendar year of last month of the custom year) – only output if firstMonth!=1. numeric \cr
#'    \tab \cr
#'    \code{Month} \tab Month String (factor) \cr
#'    \tab \cr
#'    \code{BiMonth} \tab Bi-monthly (pairs of months) String (factor) \cr
#'    \tab \cr
#'    \code{Qtr} \tab Quarter string (factor) \cr
#'    \tab \cr
#'    \code{BiAnn} \tab Bi-annual (6-monthly increments) String (factor) \cr
#' }
#'
#' @export
#'
GetMoreDateInfo<-function(Data,firstMonth=1,FindDateShifts=TRUE){
  #First month is the first month of the custom year.  1 is defaul (Jan-Dec year)


  #First, assign a temporary date to observations, if FindDateShifts=TRUE that shifts observations to teh next month in the case tat they are at the end of the month
  # and there was another observation at the start of the month, and there is no observation in the next month.
  #If false, we just use myDate
  ifelse(FindDateShifts,Data<-ShiftTempDate(Data),Data$tempDate<-Data$myDate)

  if(firstMonth>1){
    yearshift<-365.25-as.numeric(strftime(as.Date(paste("2000",firstMonth,"1",sep="-")), format = "%j"))
    MyNumMonthString<-c(NumMonthString[firstMonth:12],NumMonthString[1:firstMonth-1])
  }else{
    MyNumMonthString<-NumMonthString
    yearshift<-0
  }

  Data$Year <- as.numeric(format(Data$tempDate, "%Y"))
  if(firstMonth!=1){# Customised  year
    Data$CustomYear<-Data$Year
    Data$CustomYear[as.numeric(format(Data$tempDate,"%m"))>=firstMonth]<-Data$Year[as.numeric(format(Data$tempDate,"%m"))>=firstMonth]+1
  }

  Data$Month <- format(Data$tempDate, "%b")    # abbreviated  months
  Data$Month <- factor(Data$Month, levels = MyNumMonthString)

  #Make a data frame of all the extra time increment option s
  A<-data.frame(Month=factor(MyNumMonthString,levels=MyNumMonthString))
  A$Num<-1:12

  A$BiMonth<-paste(A$Month[ceiling((A$Num)/2)*2-1],A$Month[ceiling((A$Num)/2)*2],sep=".to.")
  A$Qtr<-paste(A$Month[ceiling((A$Num)/3)*3-2],A$Month[ceiling((A$Num)/3)*3],sep=".to.")
  A$BiAnn<-paste(A$Month[ceiling((A$Num)/6)*6-5],A$Month[ceiling((A$Num)/6)*6],sep=".to.")

  A$BiMonth<-factor(A$BiMonth,levels=unique(A$BiMonth))
  A$Qtr<-factor(A$Qtr,levels=unique(A$Qtr))
  A$BiAnn<-factor(A$BiAnn,levels=unique(A$BiAnn))

  A$Num<-NULL

  #Determine the middle day for each time increment
  MidTimeIncrList<-list()
  for(i in 1:12){MidTimeIncrList[[i]] <- seq(365/i/2,365-365/i/2,by=365/i)-yearshift-1}
  #Commit to the gloabl environment for later use
  assign("MidTimeIncrList",MidTimeIncrList,envir = .GlobalEnv)

  #Glue all time increment options onto original dataframe
  Data<-merge(Data,A)
  Data$tempDate<-NULL

  return(Data)}
