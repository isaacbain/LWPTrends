SeasonalTrendAnalysis<-function(x,do.plot=F,...){
  #Do Seasonal Kendall Test
  A1<-SeasonalKendall(x,...)

  if(A1$AnalysisNote =="ok"){
    #Continue on to do Seasonal Sen Slope if Seasonal Kendall Test was sucessful
    A1$AnalysisNote <- NULL
    A2<-SeasonalSenSlope(x,Cd=A1$Cd,do.plot=do.plot,...)

  }else{
    A2<-data.frame(Median=NA, Sen_VarS=NA, AnnualSenSlope=NA, Intercept=NA,
                   Sen_Lci=NA, Sen_Uci=NA,Sen_Probability=NA, Sen_Probabilitymax=NA, Sen_Probabilitymin=NA,
                   Percent.annual.change=NA)
    # if(do.plot=="doGGPlot"){A2<-list(A2,NA)}
  }

  if(class(A2)=="list"){
    A<-cbind.data.frame(A1,A2[[1]])
  }else{
    A<-cbind.data.frame(A1,A2)}

  if(is.na(A$Cd)){
    A$TrendCategory<-"Not Analysed"
    A$TrendDirection<-"Not Analysed"
  }else{
    A<-cbind.data.frame(A,GetTrendDirectionandClass(A))}

  if(class(A2)=="list"){
    A<-list(A,A2[[2]])
  }

  #Choose to no longer return some of the older outputs.  They are retained in case anyone wants to find them
  #for comparative purposes (back compatability to older versions), but are no longer part of the core outputs
  if(class(A2)=="list"){
    A[[1]][,c("Sen_Probability","Sen_VarS","Intercept","Sen_Probabilitymax","Sen_Probabilitymin","TrendCategory")]<-NULL
  }else{

    A[,c("Sen_Probability","Sen_VarS","Intercept","Sen_Probabilitymax","Sen_Probabilitymin","TrendCategory")]<-NULL}
  return(A)
}
