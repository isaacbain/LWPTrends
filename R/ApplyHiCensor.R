ApplyHiCensor <-function(x, ValuesToUse = "RawValue",HiCensor=TRUE,RawValues=TRUE){
  # HiCensor can either be logical, or numeric.  When ==TRUE all values below the highest left censored value are set as censored.
  #This code is only called if HiCensor is numeric OR HiCensor
  # when numeric, only values below the value are reassigne as censored if they are not already so identified.

  CenMax <- max(x[, ValuesToUse][x$CenType == "lt"], na.rm=T)

  if (!is.logical(HiCensor)){ # if there is a numeric high censor value, check to see whether the largest censored value reaches this - if not, hi censor level to largest censored value
    CenMax=min(CenMax,HiCensor)}

  x$Censored[x[, ValuesToUse] < CenMax] <- TRUE
  x$CenType[x[, ValuesToUse] < CenMax] <- "lt"  # and treat these as all censored.
  if(RawValues==TRUE){  #If the data are Flow adjusted, we just want to get the new censoring labels and not change the values
    x[, ValuesToUse][x[, ValuesToUse] < CenMax] <- CenMax # set all values below the max censored value, to the max censored
  }
  return(x)
}
