Impute.upper <- function(x, ValuesToUse = "i1Values") {
  # this function uses survreg (package::survival) to impute values right censored observations - i.e., ABOVE (multiple) detection limits

  myData <-  x
  myData<-myData[order(myData$myDate),]

  if(sum(myData$CenType == "gt") == 0 | sum(myData$CenType =="gt") == nrow(myData)) { # if no right censored obs, OR all are censored, return the values.

    if(sum("gt"==myData$CenType) == 0 ) { # first deal with no censored obs
      print("no observations are right censored")
      myData$i2Values <- myData[, ValuesToUse]
      myData$RightImpute <- "No censored - no imputation required"
    }

    if(sum(myData$CenType =="gt") == nrow(myData)) {
      print("all observations are right censored - cannot fit model")
      myData$i2Values <- myData[, ValuesToUse]
      myData$RightImpute <- "All censored - cannot impute"
    }

  } else { # otherwise impute


    # a survreg model cannot be fit with n<24; in this case, simply set all right censored to 1.1*RawValue
    if(nrow(myData) < 24 | sum(myData$CenType == "gt")==0 ) {
      myData$i2Values <- myData[, ValuesToUse]
      myData$i2Values[myData$CenType == "gt"] <- 1.1*myData$i1Values[myData$CenType == "gt"] # add 10%
      myData$RightImpute <- "Increased by 10%"
    } else {
      myData$Temp <- myData[, ValuesToUse]
      myData$Temp[myData[, ValuesToUse]==0]<-min(myData[, ValuesToUse][myData[, ValuesToUse]!=0])  #This is  a catch for bad data that makes the weibull model fall over
      # fit distribution
      myData$status <- myData$CenType != "gt"   # note well if myData flag is "gt" (meaning censored) this is equivalent to "not dead" status in a survival model and therefore is
      myMod <- survreg(Surv(Temp, status) ~ 1, data = myData, dist="weibull") # using original observed non-censored
      RScale <- as.numeric(exp(myMod$coefficients)) # this is the rweibull scale (see "survreg" function notes)
      RShape <- 1/myMod$scale   # this is the weibull shape

      SynthDist <- sort(rweibull(10000, shape=RShape, scale=RScale))   # synthetic distribution
      # NB large sample taken to ensure that there are several values in SynthDist > the non detects
      # otherwise the function below falls over.
      # Include a catch here in the cases that the non-detects are much larger than suggested by the distrbution,
      # replace RawValues with 99th percentil of the distribution

      myData$Temp[myData$CenType=="gt" & myData$Temp > quantile(SynthDist,0.995)]<-quantile(SynthDist,0.995)

      myData$i2Values<-myData$Temp
      myData$i2Values[myData$CenType=="gt"]<-sapply(myData$Temp[myData$CenType=="gt"],function(x) resample(SynthDist[SynthDist > x], size=1))
      myData$status=NULL

      myData <- myData[, -which(names(myData) == "Temp")] # drop the Temp column
      myData$RightImpute <- "Imputed"
    }
  }
  return(myData)
}
