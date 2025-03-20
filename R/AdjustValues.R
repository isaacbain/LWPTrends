AdjustValues <- function(myFrame, method = c("Gam", "LogLog", "LOESS"), ValuesToAdjust = "RawValue",
                         Covariate = "Flow", Span = c(0.5, 0.75),
                         do.plot = F, plotpval=T,plotloglog=F,
                         Covariate_Lab="Flow",mymain=NA,HiCensor=FALSE,...) {  # myFrame <- WQData[WQData$siteID == "AK1" & WQData$analyte == "TN", ]

  # make sure the data are in date order!
  myFrame <- myFrame[order(myFrame$myDate), ]
  row.names(myFrame) <- 1:nrow(myFrame)        # deliberately naming the rows in ascending order so that adjusted values can be matched to the orginal values at the end

  OriginalValues <- myFrame[,ValuesToAdjust]  #

  if (HiCensor!=FALSE){  #This makes a hicenosr adjustment just for the outputs, we still use all data for the fitting
    x<-ApplyHiCensor(myFrame,ValuesToAdjust,HiCensor)
    OriginalValues[x[,'CenType']=="lt"]<-OriginalValues[x[,'CenType']=="lt"]/2
    OriginalValues[x[,'CenType']=="gt"]<-OriginalValues[x[,'CenType']=="gt"]*1.1

  }else{
    OriginalValues[myFrame[,'CenType']=="lt"]<-OriginalValues[myFrame[,'CenType']=="lt"]/2
    OriginalValues[myFrame[,'CenType']=="gt"]<-OriginalValues[myFrame[,'CenType']=="gt"]*1.1
  }

  # number of analyses and different adjustments output

  if (any(method == "LOESS")) {
    NoAdjusts <- (length(method)-1) + length(Span)
    AdjustNames <- c(method[method != "LOESS"], paste0("LOESS", Span))
  } else {
    NoAdjusts <- length(method)
    AdjustNames <-method
  }

  AdjustList <- vector("list", NoAdjusts) # maintain a list of the adjustments made using different models
  names(AdjustList) <- AdjustNames

  # dataframe for output adjusted values
  OUT <- data.frame(matrix(nrow=length(OriginalValues), ncol=NoAdjusts*3))
  names(OUT) <- c(AdjustNames,sapply(AdjustNames,function(x) paste(x,"R",sep="_")),sapply(AdjustNames,function(x) paste(x,"p",sep="_")))
  OUT$myDate <- myFrame$myDate
  OUT$OriginalRawValues <- myFrame[,ValuesToAdjust]


  #For censored values, use half of <DL and 1.1 >AL
  myFrame[myFrame[,'CenType']=="lt",ValuesToAdjust]<-  myFrame[myFrame[,'CenType']=="lt",ValuesToAdjust]/2
  myFrame[myFrame[,'CenType']=="gt",ValuesToAdjust]<-  myFrame[myFrame[,'CenType']=="gt",ValuesToAdjust]*1.1
  myFrame$ValueswithHiCen<-OriginalValues
  #OUT$analyte<-myFrame[,"analyte"]
  #OUT$siteID<-myFrame[,"siteID"]
  myplot<-NA
  # what percentage of data has Covariates?
  myFrame$HasFlowAndData <- !is.na(myFrame[,Covariate]) & !is.na(OriginalValues) # rows with Covariate and values.
  PropWithFlow <- sum(myFrame$HasFlowAndData)/nrow(myFrame)
  #
  #
  if(all(is.na(myFrame[,Covariate])) |  (nrow(myFrame) < 10) | sum(myFrame$HasFlowAndData)<10 |  # no Covariate data, not much data - PropWithFlow < 0.8 |
     length(unique(myFrame[, ValuesToAdjust]))/nrow(myFrame) < 0.05 |   # many values are the same
     max(unlist(rle(as.vector(myFrame[,Covariate]))[1]))/nrow(myFrame) > 0.3) # or long sequence of indentical Covariate
  {     # dont even try Covariate adjustment
  } else {
    myFrame2 <- myFrame[myFrame$HasFlowAndData, ]    # makes a frame with no NA values
    myFrame2$XXValues <- myFrame2[,ValuesToAdjust]
    myFrame2$XXFlow <- myFrame2[,Covariate]


    hasZeroValues <- ifelse(sum(myFrame2$XXValues <= 0) >0 | sum(myFrame2$XXFlow <= 0)>0, T, F)  # are there any zero or negative Covariate or variable values
    ZerosHere <- myFrame2$XXValues == 0 | myFrame2$XXFlow == 0                                                       # keep an index of zero values
    myFrame2$XXValues[myFrame2$XXValues == 0] <-  min(myFrame2$XXValues[myFrame2$XXValues > 0])/2                                         # make zeros equal to small values
    myFrame2$XXFlow[myFrame2$XXFlow <= 0] <- min(myFrame2$XXFlow[myFrame2$XXFlow > 0])/2                                         # make zeros equal to small values
    myx<-seq(from=min(myFrame2$XXFlow),to=max(myFrame2$XXFlow),length.out=100) #Xvalues for plotting

    # fit GAM
    if(any(method == "Gam")) {
      myGam <- gam::gam(XXValues ~ s(XXFlow), data = myFrame2,contrasts = list(a = "contr.sum"))
      concHat <- predict(myGam, newdata = myFrame2)
      Residuals <- myFrame2$XXValues - concHat
      FlowAdjvalues <- median(OriginalValues, na.rm=T) + myFrame2$ValueswithHiCen - concHat
      R2 <- round(100* (1 - (sum(residuals(myGam)^2)/sum((mean((myFrame2$XXValues)) - (myFrame2$XXValues))^2))))
      R<-cor.test(concHat,myFrame2$XXValues,method="pearson")$estimate
      p<-cor.test(concHat,myFrame2$XXValues,method="pearson")$p.value
      myy<-predict(myGam, newdata = data.frame(XXFlow=myx))
      AdjustList[["Gam"]] <- list(FlowAdjvalues = FlowAdjvalues, Residuals = Residuals, concHat = concHat, Fitted = fitted(myGam), R2 = R2,R=R,p=p,method="Gam",myy=myy,myx=myx)
      # print("**** Safe to ignore MODEL.MATRIX warning *****")
      #Apparenlty this is a long term issue with gam, that now throughs a warning
      # in R v 3.6.0+ but does not affect our evaluations https://stackoverflow.com/questions/57664927/warning-in-gam-with-release-of-r-3-6-1
    }

    # fit log-log
    if(any(method == "LogLog")) {
      myLogLog <- lm(log10(XXValues) ~ log10(XXFlow), data = myFrame2)
      concHat <- 10^predict(myLogLog, newdata = myFrame2)
      Residuals <- myFrame2$XXValues - concHat
      FlowAdjvalues <- median(OriginalValues) + myFrame2$ValueswithHiCen - concHat
      R2 <- round(100 * (1 - (sum(residuals(myLogLog)^2)/sum((mean(log10(myFrame2$XXValues)) - log10(myFrame2$XXValues))^2))))  #Note =- this R2 is in the log-log space
      R<-cor.test(concHat,myFrame2$XXValues,method="pearson")$estimate
      p<-cor.test(concHat,myFrame2$XXValues,method="pearson")$p.value
      myy<-10^predict(myLogLog, newdata =  data.frame(XXFlow=myx))
      AdjustList[["LogLog"]] <- list(FlowAdjvalues = FlowAdjvalues, Residuals = Residuals, concHat = concHat, Fitted = 10^fitted(myLogLog), R2 = R2,R=R,p=p,method="LogLog",myy=myy,myx=myx)  # NB back transformation here (should I cotrrect  for bias???)
    }

    #loess
    if(any(method == "LOESS")) {
      LoessMods <- vector("list", length=length(Span))          # list to store each loess model
      for(i in 1:length(Span)) {                             # for each value of Span.
        thisName <- paste0("LOESS", Span[i])
        op <- options(warn=2)
        tt <- try(loess((XXValues)~(XXFlow), span=Span[i], data = myFrame2))

        if(is(tt,"try-error"))  {
          LoessMods[[i]] <- list(FlowAdjvalues = rep(NA, nrow(myFrame2)), Residuals = rep(NA, nrow(myFrame2)), concHat = rep(NA, nrow(myFrame2)), R2 = NA,R=NA,p=NA,method=thisName,myy=NA,myx=myx)
        } else {
          op <- options(warn=0) # set back to default warnings.
          Lowess <- loess((XXValues)~(XXFlow), span=Span[i], data = myFrame2)
          concHat <- predict(Lowess, newdata = myFrame2)  # the estimated concentrations for each days Covariate
          Residuals <- myFrame2$XXValues - concHat
          FlowAdjvalues <- median(OriginalValues) + myFrame2$ValueswithHiCen - concHat
          R2 <- round(100 * (1 - (sum(residuals(Lowess)^2)/sum((mean((myFrame2$XXValues)) - (myFrame2$XXValues))^2))))
          R<-cor.test(concHat,myFrame2$XXValues,method="pearson")$estimate
          p<-cor.test(concHat,myFrame2$XXValues,method="pearson")$p.value
          myy<-predict(Lowess, newdata = data.frame(XXFlow=myx))
          AdjustList[[thisName]] <- list(FlowAdjvalues = FlowAdjvalues, Residuals = Residuals, concHat = concHat, Fitted = fitted(Lowess), R2 = R2,R=R,p=p,method=thisName,myy=myy,myx=myx)
        }
      }
    }


    if(do.plot!=FALSE) {


      myFits<-data.frame(do.call("rbind", lapply(AdjustList, function(df) cbind.data.frame(df$myx,df$myy,df$method))))
      names(myFits)<-c("myx","myy","Method")
      myFits$myx<-as.numeric(as.character(myFits$myx))
      myFits$myy<-as.numeric(as.character(myFits$myy))
      myFits$Method<-factor(myFits$Method,levels=AdjustNames)
      if(plotpval) {
        ModR2_p <- ldply(AdjustList, function(x) {
          PVal <- "NOT SIGNIFICANT p>0.05"
          if(x$p < 0.05) PVal <- "SIGNIFICANT p<0.05"
          return(paste0("R2 =", round((x$R)^2*100), "% ", PVal))  #Note - R2 here is calcualted in teh original unit space
        })

        ModR2_p$lab <- paste(ModR2_p[,1], ModR2_p[,2])}else{ModR2_p=NA}

      if(do.plot==TRUE){
        myplot<-AdjustValues_GGPlots(myFits=myFits,myFrame2=myFrame2,
                                     ZerosHere=ZerosHere,plotpval=plotpval,
                                     ModR2_p=ModR2_p,
                                     Covariate=Covariate_Lab,mymain=mymain,...)
      }}

    for(i in 1:NoAdjusts) OUT[,i][match(names(AdjustList[[i]]$Residuals), row.names(myFrame))] <- AdjustList[[i]]$Residuals#[order(myFrame$myDate)[myFrame$HasFlowAndData]] # put in original date order only for oginal data with value and flow.
    for(i in 1:NoAdjusts) OUT[,NoAdjusts+i][myFrame$HasFlowAndData] <- AdjustList[[i]]$R
    for(i in 1:NoAdjusts) OUT[,NoAdjusts*2+i][myFrame$HasFlowAndData] <- AdjustList[[i]]$p

    op <- options(warn=0) # set back to default warnings.
  }
  if(do.plot==TRUE){OUT<-list(OUT,myplot)}
  #browser()
  return(OUT)  # pass back the frame with all data including the Covariate adjustment
}
