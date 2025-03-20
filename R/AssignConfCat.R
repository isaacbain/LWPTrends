AssignConfCat<-function(x,CatType="Improve",Reverse=c("CLAR","MCI"),NoImproveDir=c("pH"),nCat="Simple"){
  #CatType options are: Direction, Improve or Decrease
  #Reverse is a list of variables for which increasing trends indicate improvement (only relevant when using "Improve")
  #NoImproveDir is a list of variables for which increasing trends are not clearly assocated with improvement or degradation  (only relevant when using "Improve")
  #nCat options are Full (full IPCC) or Simple (simplified categories)

  if(CatType=="Direction"){  #Returns of confidence in trend direction
    ifelse(is.vector(x),P<-x,P<-x$C)

    if(nCat=="Full"){
      mybreaks=c(0.49,0.67, 0.9, 0.95, 0.99, 1.01)
      mycatlabels=c("As likely as not", "Likely", "Very likely", "Highly likely", "Extremely likely", "Virtually certain")
    }else{
      mybreaks=c(0.49,0.67, 0.9, 0.95, 1.01)
      mycatlabels=c("As likely as not","Likely", "Very likely", "Highly likely")
    }

  }else{
    P<-x$Cd
    if(nCat=="Full"){
      mybreaks =c(-0.01, 0.01, 0.05, 0.1, 0.33,0.67, 0.9, 0.95, 0.99, 1.01)
      #These breaks and labels are based on guidance in IPCC report
      mycatlabels = c("Exceptionally unlikely",   "Extremely unlikely","Very unlikely", "Unlikely","As likely as not",
                      "Likely", "Very likely","Extremely likely","Virtually certain")
    }else{
      mybreaks =c(-0.01,  0.1, 0.33,0.67, 0.9, 1.01)
    }
    if(CatType=="Improve"){ #Returns confidence that trend is
      if(nCat=="Simple"){
        mycatlabels=c("Very likely degrading", "Likely degrading","Low confidence",
                      "Likely improving", "Very likely improving")
      }

      if(!is.na(Reverse[1])){P[x$analyte %in% Reverse]<-1-P[x$analyte %in% Reverse]}
      if(!is.na(NoImproveDir[1])){P[x$analyte %in% NoImproveDir]<-NA}



    }else if (CatType=="Decrease"){
      if(nCat=="Simple"){
        mycatlabels=c("Very likely increasing", "Likely increasing","Low confidence",
                      "Likely decreasing", "Very likely decreasing")
      }
    }else{
      #Catch in case a CatType that doesn't exist is specified
      stop("CatType must be specified as Direction, Improve, or Decrease")
    }
  }


  ConfCats <-cut(P, breaks =mybreaks,labels=mycatlabels)
  ConfCats <-as.character(ConfCats )
  ConfCats [is.na(ConfCats )]<-"Not Analysed"
  ConfCats <-factor(ConfCats ,  levels = c(mycatlabels,"Not Analysed"))
  return (ConfCats)
}
