getTAU <- function(x, obs,SiteNameCol="siteID",VarNameCol="analyte",ValuesToUse = "RawValue", Year = "Year",
                   UseMidObs=T,DirType="Decrease",Reverse=c("CLAR","MCI"),excl=c("pH")) { #
  x$siteID<-x[,SiteNameCol];obs$siteID<-obs[,SiteNameCol]
  x$analyte<-x[,VarNameCol];obs$analyte<-obs[,VarNameCol]

  x <- x[!is.na(x$C), ] # remove NA values from trend evaluations that result from NotAnalysed

  myObs <- obs[obs$analyte == x$analyte[1], ] # observation data pertaining to this variable
  sitesInCommon <- intersect(unique(myObs$siteID), x$siteID) # sites in common to the orginal data and trend evaluations

  x<-x[x$siteID %in% sitesInCommon,]
  myObs <- myObs[myObs$siteID %in% sitesInCommon, ]  # ensure sites are consistent and get observations for the common sites

  M <- nrow(x)                # the total number of sites

  #Determine the smallest frequency used in the trend assessements
  if(all(x$TimeIncr=="Annual")){
    Frequency<-"Annual"
  } else{Frequency <- SeasonLabs$SeasName[min(match(unique(x$TimeIncr),SeasonLabs$mySeas))]    }  #

  sites.all <- x$siteID # all the sites
  names(sites.all) <- sites.all

  ModalDirection <- sign(sum(sign(x$S)) + sum(x$S ==0)/2)
  if(ModalDirection==0){ModalDirection=1}#Just a catch for when it is exaclty equal
  DT <- ifelse(ModalDirection==1, "Increasing", "Decreasing") # aggregate trend direction

  TAU <- (sum(sign(x$S) == ModalDirection) + sum(x$S ==0)/2)/M # aggregate trend strength (the proportion of trends in the modal direction)

  ProbModalDirection <- ddply(x, "siteID", function(y) ifelse(sign(y$S) == ModalDirection, y$C, 1-y$C))
  x$P_Modal  <- ProbModalDirection[match(x$siteID, ProbModalDirection$siteID),2]

  VarTAU <- 1/M^2 * sum(x$P_Modal * (1-x$P_Modal))          # the non-adjusted variance

  SumVarTAU <- sum(x$P_Modal * (1-x$P_Modal))               # this is the first term From Yue and Wang 2002 Equation 17a (sum of individual variances).This is the first term in manuscript Eqn 9.

  # Following From Yue and Wang 2002 Equation 17a. Calculate pairwise variance (note this is square matrix we need only lower triangle). This is manuscript Eqn 10 without the cross correlation term
  Cov <- outer(x$P_Modal*(1 - x$P_Modal), x$P_Modal*(1-x$P_Modal)) # nb. variance per site is Cd(1-Cd)
  dimnames(Cov)  <- list(x$siteID, x$siteID)


  ###########################################################################
  # the adjustment to the variance to account for covariance

  if(Frequency == "Annual") {  # only one season = annual
    cat("Annual variable", as.character(x$analyte[1]), "\n")
    # get correlation of the observations (this is the pck,k+l term in  Yue and Wang 2002 Equation 17a.)

    # Add on time increment and time incr- year for all sites
    myObs$TimeIncr<-myObs[,Year]
    myObs.TimeIncr<-ddply(myObs,.(siteID,analyte),function(d){
      d<-GetTimeIncrYear(x=d, ValuesToUse = ValuesToUse,Year=Year)
      d <- ValueForTimeIncr(x=d, ValuesToUse=ValuesToUse, Year=Year, UseMidObs=UseMidObs)
      return(d)
    })

    AllDates<-myObs.TimeIncr[,c("TimeIncr","TimeIncrYear")]
    AllDates<-AllDates[!duplicated(AllDates),]

    AllDates<-merge(AllDates,myObs.TimeIncr,all=T)
    AA<- as.data.frame(pivot_wider(AllDates[,c("TimeIncrYear","siteID","V1")],names_from="siteID",values_from="V1"))
    rownames(AA)<-AA$TimeIncrYear
    AA$TimeIncrYear<-NULL

    CorMatrix <- cor(AA, use="pairwise.complete.obs")  # get correlation of the observations (this is the pck,k+l term in  Yue and Wang 2002 Equation 17a.). This is the first term in manuscript Eqn 10.
  }
  #######################################################################

  if (Frequency != "Annual") { # calculate the pairwise-site observation correlations for data that has frequency higher than annual
    # assume that the data can have frequency as high as "Frequency" - get a time series at the Frequency time-step with missing months represented by NA values

    cat(SeasonLabs$mySeas[SeasonLabs$SeasName==Frequency]," variable", as.character(x$analyte[1]), "\n")

    # Add on time increment and time incr- year for all sites
    myObs$TimeIncr<-myObs[,Frequency]
    myObs.TimeIncr<-ddply(myObs,.(siteID,analyte),function(d){
      d<-GetTimeIncrYear(x=d, ValuesToUse = ValuesToUse,Year=Year)
      d <- ValueForTimeIncr(x=d, ValuesToUse=ValuesToUse, Year=Year, UseMidObs=UseMidObs)
      return(d)
    })

    AllDates<-myObs.TimeIncr[,c("TimeIncr","TimeIncrYear")]
    AllDates<-AllDates[!duplicated(AllDates),]

    AllDates<-merge(AllDates,myObs.TimeIncr,all=T)
    AA<- as.data.frame(pivot_wider(AllDates[,c("TimeIncrYear","siteID","V1")],names_from="siteID",values_from="V1"))
    rownames(AA)<-AA$TimeIncrYear
    AA$TimeIncrYear<-NULL

    CorMatrix <- cor(AA, use="pairwise.complete.obs")  # get correlation of the observations (this is the pck,k+l term in  Yue and Wang 2002 Equation 17a.). This is the first term in manuscript Eqn 10.

  }
  #####################################################################

  CorMatrix <- CorMatrix[sites.all, sites.all]
  Cov <- Cov[sites.all, sites.all] # this ensure sites align for the multiplication step
  CovTerm <- sqrt(Cov) * CorMatrix
  SumCovTerm <- sum(CovTerm[lower.tri(CovTerm)], na.rm=T) # this is the whole added adjustment term from Yue and Wang 2002 Equation 17a.
  # note need to use na.rm = T because it can occur that a site has zero variance (all observations same value or censored) [a warning is issued in this instance]

  CorrectedVarTAU <- 1/M^2 * (SumVarTAU + (2 * SumCovTerm)) # this is the whole of Yue and Wang 2002 Equation 17a. This is the whole of manuscript equation 9.

  UncorrectedConfidence<-getCT(TAU=TAU,VarTAU = VarTAU)
  Confidence <- getCT(TAU=TAU, VarTAU=CorrectedVarTAU) # get direction confidence based on the adjusted variance

  ConfCat=AssignConfCat(Confidence,CatType = "Direction")
  UncorrectedConfCat=AssignConfCat(UncorrectedConfidence,CatType = "Direction")

  SummaryResults <- data.frame(M=M, TAU=TAU, VarTAU=VarTAU, CorrectedVarTAU=CorrectedVarTAU, DT = DT,
                               CT = Confidence,ConfCat=ConfCat, UncorrectedCT = UncorrectedConfidence,UncorrectedConfCat=UncorrectedConfCat)
  ObservationCorrelations <- CorMatrix[lower.tri(CorMatrix)]

  if(DirType=="Improve"){
    if(x$analyte[1] %in% excl){
      SummaryResults[,]<-NA
    }
    SummaryResults$DT=ifelse(ModalDirection==1, "Degrading", "Improving")
    if(x$analyte[1] %in% Reverse){
      SummaryResults$DT=ifelse(ModalDirection==0, "Degrading", "Improving")
    }

  }

  return(list(SummaryResults=SummaryResults, ObservationCorrelations=ObservationCorrelations ))
}
