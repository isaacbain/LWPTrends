#' RemoveAlphaDetect
#'
#' Takes a dataframe including a column observed values that includes censored
#' values (specified as the prefix > or <,column is type character) and returns
#' face values and information about the nature of the censoring.
#'
#' @param Data Dataframe containing column ColToUse
#' @param ColToUse Name of column in dataframe with censored observations
#'
#' @return A data frame with fields as follows (in addition to those columns in x)\tabular{ll}{
#'    \code{RawValue} \tab Numeric face value from ColToUse \cr
#'    \tab \cr
#'    \code{Censored} \tab Logical: whether the observation is censored or not \cr
#'    \tab \cr
#'    \code{CenType} \tab Factor indicating the censor type (lt: less than; gt: greater than, not: not censored) \cr
#' }
#'
#' @export
#'
RemoveAlphaDetect <-  function(Data,ColToUse="Value") {   #  removes non detect ("<" and ">") and produces a dataframe with a numeric value and a boolean defining if value is censored.
  x<-Data[,ColToUse]
  if(is.numeric(x)) xNumeric <- x   # check values may be already numeric
  if(is.factor(x)) xNumeric <- unfactor(x) # converts to numeric (but note the values < and > are returned as NA)
  if(is.character(x)) xNumeric <- as.numeric(x) # converts to numeric (but note the values < and > are returned as NA)
  isNonDetLT <- grep("<", x)  # which values are below detection
  isNonDetGT <- grep(">", x)  # which values are above detection
  DetectionLimLT <- as.numeric(sapply(as.character(x[isNonDetLT]), function(x) DougSplit(x, myPart=2, split= "<", fixed = T ))) # returns the numeric values
  DetectionLimGT <- as.numeric(sapply(as.character(x[isNonDetGT]), function(x) DougSplit(x, myPart=2, split= ">", fixed = T ))) # returns the numeric values
  xNumeric[isNonDetLT] <- DetectionLimLT
  xNumeric[isNonDetGT] <- DetectionLimGT
  Censored <- rep(FALSE, times = length(x))
  Censored[isNonDetLT] <- TRUE
  Censored[isNonDetGT] <- TRUE
  CenType <-  rep("not", times = length(x))  # classification of the type of censoring
  CenType[isNonDetLT] <- "lt" # less than censored
  CenType[isNonDetGT] <- "gt" # greater than censored
  CenType <- factor(CenType, levels = c("gt", "lt", "not"))
  # cat("*** It is safe to ignore warning message ***")

  Censored <- ifelse(Censored == "TRUE", T, F) # ensure this a binary
  Data$RawValue<-xNumeric
  Data$Censored<-Censored
  Data$CenType<-CenType
  return(Data) # raw values means NOT Flow-Adjusted and no < or > values
}
