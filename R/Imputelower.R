#' Impute.lower
#'
#' Imputes replacement values for left-censored data using the regression on order statistics (ROS)
#' function from the NADA package.
#'
#' @param x Input data frame must contain: ValuesToUse, myDate, CenType
#' @param ValuesToUse
#' @param forwardT See NADA function ros()
#' @param reverseT See NADA function ros()
#' @param do.plot Plot the ROS model
#' @param minObsforfit
#'
#' @return A data frame with fields as follows (in addition to those columns in x)\tabular{ll}{
#'    \code{i1Values} \tab Replacements for ValuesToUse \cr
#'    \tab \cr
#'    \code{LeftImpute} \tab Description of whether the observations were imputed or not \cr
#' }
#'
#' @export
#'
Impute.lower <- function(x, ValuesToUse = "RawValue", forwardT="log", reverseT="exp", do.plot=F,minObsforfit=10) {
  # this uses ROS to impute values for obs below multiple detection limits. There is no longer any randomisation of the imputed values

  # x is a data-frame containing (at a minimum)
  # x$RawValue : the raw data
  # x$CenType            :a flag indicating whether the raw data are left censored ("lt"), right censored ("gt"),
  #                     or within the lower and upper detection limits ("ok")
  # x$myDate: the date of the samples (to ensure correct ordering)
  # forwardT: forward transformation to be used in the NADA:ros() function
  # reverseT: reverse transformation to be used in the NADA:ros() function

  myData <- x
  myData$CenType <- as.character(myData$CenType)

  if(sum(myData$CenType == "lt") == 0 | sum(myData$CenType =="lt") == nrow(myData)) {       # if no left censored obs, OR all are censored, return the values.

    if(sum("lt"==myData$CenType) == 0 ) { # first deal with no censored obs
      print("no observations are left censored")
      myData$i1Values <- myData[, ValuesToUse]
      myData$LeftImpute <- "No censored - no imputation required"
    }
    if(sum(myData$CenType =="lt") == nrow(myData)) {
      print("all observations are left censored - cannot fit model")
      myData$i1Values <- myData[, ValuesToUse]/2
      myData$LeftImpute <- "All censored - cannot impute"
    }

  } else {

    r.na2 <- is.na(myData[, ValuesToUse])  # Treat NAs as censored values, then sub NA back  at end
    # Since they are marked as censored, they will not influence the ROS regression.
    # They will receive imputed values, but those will later be overwritten with NAs
    CenType.original <- myData$CenType
    myData$CenType[r.na2] <- "lt"
    myData <- myData[order(myData$myDate),]
    rownames(myData) <- 1:nrow(myData) # to keep order of values in time
    print("some below limits")

    ######################
    # catch some bad myData if censored values exceed max of uncensored values
    #cat("Catch_", length(myData[myData$CenType != "lt", "RawValue"]), "\n")
    if(sum(myData$CenType =="lt") != length(myData$CenType)) MaxUncen <- max(myData[myData$CenType != "lt", "RawValue"])


    if( sum(myData[myData$CenType == "lt", ValuesToUse] >=  MaxUncen, na.rm=T)>0 ) { # if any censored values exceed the observed uncensored values
      print("some bad data")
      BadDates <- (myData$CenType == "lt" & myData[, ValuesToUse] >=  MaxUncen)
      myData_RawValue_orig <- myData[, ValuesToUse]
      myData$RawValue[BadDates] <- 0.5 * MaxUncen
      myData$CenType[BadDates] <- "lt" # this is already the case  if condition above is True

      data2 <- myData
      #################

    } else {
      print("no bad dates")
      data2 <- myData
    }
    rownames(data2) <- 1:nrow(data2) # to keep order of values in time
    usedValues <- data2[, ValuesToUse]
    names(usedValues) <- rownames(data2)


    usedValues[usedValues <= 0] <- min(data2[data2$CenType == "lt", ValuesToUse], na.rm=T) # replace any zero values with the minimum DL

    # added a catch as ros can fail occasionally if the data are not conducive to a regression
    myros <- try(ros(obs = usedValues, censored = data2$CenType == "lt",forwardT=forwardT,reverseT=reverseT))

    if (any(class(myros) == c("ros", "lm"))&sum(data2$Censored==FALSE)>minObsforfit) {  # if a model is fitted then contiune

      SortValues <- sort(usedValues, na.last = T)  # sort order of values # na.last to preserve the NA values on sorting

      NewROS <- cbind.data.frame(SortValues,as.data.frame(myros))

      # the plot helps to show why imputed replacements for censored values are often greater than
      # non-censored values.
      if(do.plot)  {
        #graphics.off();x11();
        plot(myros)
        with(NewROS[!NewROS$censored,],  points(x=qnorm(pp), y=obs, pch=16, col="red"))
        with(NewROS[NewROS$censored,],  points(x=qnorm(pp), y=modeled, cex=2, pch=16, col="blue"))
        legend("topleft",  legend=c("Uncensored","Censored"), col = c("red","blue"), text.col = "black",  pch = c(16, 16), bg = 'white', inset = .05)
      }

      NewROS$OrigOrder <- as.numeric(names(SortValues)) # this is the original time order
      SortROS <- NewROS[order(NewROS$OrigOrder), ]  # reorder ROS to original time order

      data2$i1Values[SortROS$OrigOrder] <- SortROS[,"modeled"] # retain the order.
      data2$i1Values[r.na2] <- NA
      data2$CenType[r.na2] <- CenType.original[r.na2]
      data2$LeftImpute <- "Imputed"
      # note that any data where CenType=="lt" but (original) converted_value > MaxUncen now has an imputed value from ROS
    } else {
      data2$i1Values <- myData$RawValue
      data2$i1Values[data2$CenType=="lt"] <- myData$RawValue[data2$CenType=="lt"]/2
      data2$LeftImpute <- "Not Imputed - model fit failed"
      print("Warning: ROS model was not fitted. Imputed values are original values")
    }
    myData <- data2
  }
  return(myData)
}
