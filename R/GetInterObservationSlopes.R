GetInterObservationSlopes<-function(Data){
  # number of Time Increments between observations, this excludes comparing observations in the same year+tiem increment
  if(is.null(Data$Season[1])){x$Season<-x$TimeIncr} #Catch in case season is not specified

  Data$SeasYr<-apply(Data,1,function(y) paste0(y['Season'],y['Year']))
  AllTimeIncs <- outer(as.numeric(Data$NewDate), as.numeric(Data$NewDate), `-`)/365.25 # the time increment in years
  SameYearSeason <- outer(Data$SeasYr, Data$SeasYr, "==")
  AllTimeIncs[SameYearSeason] <- NA # remove any increments withing the same year + season

  # take each observation and compute differences for all other observations
  AllDifferences <- outer(Data$V1, Data$V1, `-`)
  AllDifferences[SameYearSeason] <- NA # remove any distances withing the same year + season

  CenLab <- outer(Data$CenType, Data$CenType, 'paste')
  Slopes <- AllDifferences/AllTimeIncs

  OUTPUTS <- data.frame(Slopes=as.vector(Slopes [lower.tri(Slopes, diag = FALSE)]),
                        CensorLabel=as.vector(CenLab [lower.tri(CenLab, diag = FALSE)]) )

  #NOW - tidy this up to account for censoring
  #1.  There can be NO slope between any pairs of censored values:
  OUTPUTS$Slopes[OUTPUTS$CensorLabel %in% c("gt gt","lt lt")]<-0
  #2. There cannot be a slope between a non-censored value and a censored higher value (i.e., a positive slope)
  OUTPUTS$Slopes[OUTPUTS$Slopes>0 & OUTPUTS$CensorLabel %in% c("lt not")]<-0
  #3. There cannot be a slope between a censored value and a non-censored lower value (i.e., a negative slope)
  OUTPUTS$Slopes[OUTPUTS$Slopes<0 & OUTPUTS$CensorLabel %in% c("not lt")]<-0
  #4. There cannot be a slope between a right censored value and a non-censored higher value (i.e., a positive slope)
  OUTPUTS$Slopes[OUTPUTS$Slopes>0 & OUTPUTS$CensorLabel %in% c("not gt")]<-0
  #5. There cannot be a slope between a non-censored value and a right censored lower value (i.e., a negative slope)
  OUTPUTS$Slopes[OUTPUTS$Slopes<0 & OUTPUTS$CensorLabel %in% c("gt not")]<-0

  OUTPUTS <- OUTPUTS[!is.na(OUTPUTS$Slopes),]
  return(OUTPUTS)
}
