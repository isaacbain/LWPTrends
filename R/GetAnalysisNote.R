GetAnalysisNote<-function(Data, ValuesToUse = "RawValue",IsSeasonal=FALSE,SecondTierTest=FALSE,...){
  #This function is used to check that there are sufficient data at several points in the analysis
  #to contintue with the calculations, and if there are not to provide an error message describing the issue
  AnalysisNote="ok"
  #Set generic levels here about numbers of unique values and non-cenosred values required
  #allows user to adjust and apply consistently across all functions
  noUnique<-3
  noNonCen<-5
  noUniqueSeas<-2
  #First round of filtering
  if(all(is.na(Data[, ValuesToUse]))){
    AnalysisNote="Data all NA values"

  }else if(length(unique((Data[, ValuesToUse][!Data$Censored&!is.na(Data[,ValuesToUse])]))) < noUnique){
    AnalysisNote=paste0("< ",noUnique," unique values")

  }else if(length(which(Data[,'Censored']==FALSE & !is.na(Data[,ValuesToUse]))) < noNonCen){
    AnalysisNote=paste0("< ",noNonCen," Non-censored values")
  }

  #Check to see whether we have failed at the first step, AND that we are requested to do next
  #set of tests (these are for data post averagein over seasons.  Continue on to check more details
  if(AnalysisNote=="ok"&SecondTierTest){
    #Different tests whether Seasonal or not
    if(IsSeasonal){ #Seasonal checks
      #Check to see that there are sufficient non-NA noncensored values in each season
      EnoughSeason <- min(table(Data[,"Season"][!is.na(Data[,ValuesToUse])]))< noUnique
      #Check to see that there are sufficient unique values in each season
      EnoughSeason_2 <- min(ddply(Data,c("Season"),function(x)length(unique(x[, ValuesToUse])))[,"V1"]) <  noUniqueSeas #There must be at least 3 unique values per season

      if (EnoughSeason==TRUE){
        AnalysisNote=paste0("< ",noUnique,"non-NA values in Season")
      }else if (EnoughSeason_2==TRUE){
        AnalysisNote=paste0("< ",noUnique," unique values in Season")
      }else{
        #Then check the run length
        # if TRUE data has long sequence of indentical values within one or more season and will crash
        RunLengthSeason <- ddply(Data, "Season", function(y) {  # for each season y = x[x$Season == "Q1",]
          take <- !is.na(y[, ValuesToUse])  & !is.na(y$Censored)
          theValues <- y[take, ValuesToUse]
          if(length(theValues)>1) {
            longRun <- max(unlist(rle(diff(theValues))[1]))/length(theValues) > 0.75  # this catches seasons with one value to avoid warning.
          } else {
            longRun <- FALSE
          }
          return(longRun)
        })
        RunLength <- sum(RunLengthSeason$V1)>0

        if(RunLength==TRUE) {AnalysisNote="Long run of single value in a Season"}
      }
    }else{#Then not seasonal
      # if TRUE data has long sequence of indentical values and will crash
      RunLength <-  max(unlist(rle(diff(Data[, ValuesToUse]))[1]))/length(Data[, ValuesToUse]) > 0.5
      if(RunLength==TRUE) {AnalysisNote="Long run of single value"}
    }
  }

  return(AnalysisNote)
}
