ShiftTempDate<-function(x){
  mynames<-names(x)
  x<-x[order(x$myDate),]
  x$dT_bwd<-as.numeric(c(0,diff(x$myDate)))
  x$dT_fwd<-as.numeric(c(diff(x$myDate),0))
  x$Month<-as.numeric(format(x$myDate, "%m"))
  x$Year<-as.numeric(format(x$myDate, "%Y"))
  x$Day<-as.numeric(format(x$myDate, "%d"))
  x$dMonB<-c(0,diff(x$Month))
  x$dMonB[x$dMonB<0]<-x$dMonB[x$dMonB<0]+12
  x$dMonF<-c(diff(x$Month),0)
  x$dMonF[x$dMonF<0]<-x$dMonF[x$dMonF<0]+12

  #Index of observations that should be moved forward a month
  indF<-x$dMonB==0&x$dMonF>1&x$dT_bwd>20&x$dT_fwd>20
  #Index of observations that should be moved backward a month
  indB<-x$dMonF==0&x$dMonB>1&x$dT_bwd>20&x$dT_fwd>20

  x$NextMonth<-x$Month+1
  x$NextMonth[x$NextMonth==13]<-1
  x$PrevMonth<-x$Month-1
  x$PrevMonth[x$NextMonth==1]<-12

  x$tempDateF<-make_date(year=x$Year,month=x$NextMonth,day=1)

  x$tempDateB<-make_date(year=x$Year,month=x$PrevMonth,day=1)

  x$tempDate<-x$myDate
  x$tempDate[indF==TRUE]<-x$tempDateF[indF==T]
  x$tempDate[indB==TRUE]<-x$tempDateB[indB==T]

  x<-x[,c(mynames,"tempDate")]
  return(x)
}
