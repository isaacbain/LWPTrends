GGPlotTrend<-function(x,x1,mymain=NULL,Intercept=NA,ValuesToUse = "RawValue",AnnualSenSlope=NA,
                      Lci=NA,Uci=NA,IsSeasonal=FALSE,myTimeIncr=NA,mySeason=NA,Ymed=NA,Tmed=NA,Percent.annual.change=NA,Cd=NA,legend.pos="top",
                      the_ylab="Values",CropYaxis=FALSE,UseMidObs=TRUE){
  if(is.null(x$myDate)) x$myDate<-x$NewDate

  Ymed<-median(x[,ValuesToUse])
  Interceptu <- Ymed - (Uci*Tmed)
  Interceptl <-Ymed - (Lci*Tmed)

  # value at end of time period
  T1 <- Intercept + as.numeric(diff(range(x$myDate)))/365.25 * AnnualSenSlope
  T1l <- Interceptl + as.numeric(diff(range(x$myDate)))/365.25 * Lci
  T1u <- Interceptu + as.numeric(diff(range(x$myDate)))/365.25 * Uci

  Trends<-data.frame(t=as.Date(c(rep.int(as.character(min(x$myDate)),3),rep.int(as.character(max(x$myDate)),3)),origin="1-1-1970"),
                     y=c(Intercept,Interceptl,Interceptu,T1,T1l,T1u),
                     type=c("Trend","90% C.I.","90% C.I.","Trend","90% C.I.","90% C.I."),
                     gp=c("a","b","c","a","b","c"))
  if(UseMidObs==TRUE){
    x$DataType<-"Observations\n(mid of time increment)"}else{
      x$DataType<-"Observations\n(median of time increment)"
    }
  names(x)[names(x)=="NewDate"]<-"myDate"
  x1$DataType<-"Raw Observations"
  x1$CensoredOrig<-x1$Censored

  xall<-rbind(x[,c("myDate",ValuesToUse,"DataType","CensoredOrig")],
              x1[,c("myDate",ValuesToUse,"DataType","CensoredOrig")])
  names(xall)[c(2,4)]<-c("Values","Censored")
  xall$Censoring<-"Non-censored"
  xall$Censoring[xall$Censored=="TRUE"]<-"Censored"
  xall$Censoring<-factor(xall$Censoring,levels=c("Censored","Non-censored"))

  cencol<-c("red","black")
  names(cencol)<-c("Censored","Non-censored")

  myplot<-ggplot()+#xall,aes(x=myDate,y=Values))+
    geom_point(data=xall,aes(x=myDate,y=Values,colour=Censoring,shape=DataType,alpha=DataType),show.legend = T)+
    scale_color_manual(values=cencol, drop = FALSE)+
    scale_shape_manual(values=c(16,21), drop = FALSE)+
    scale_alpha_manual(values=c(1,0.5), drop = FALSE)+
    theme_bw()+ylab(the_ylab)+
    geom_line(data=Trends,aes(x=t,y=y,group=gp,linetype=type),colour="blue",linewidth=.9)+
    scale_linetype_manual(values=c(2,1))+
    labs(linetype="Trends",colour="Censoring",alpha=NULL,shape="Data Type")+
    xlab("Time")+ylab("Values")+guides(alpha="none")+
    theme(legend.position = "bottom",legend.box="horizontal",
          legend.direction = "vertical",
          legend.spacing=unit(0.1,"lines"),
          legend.box.just = "top",
          legend.title=element_text(hjust=0.5))+
    guides(colour = guide_legend(override.aes = list(shape = 15,size=3)),
           linetype = guide_legend(override.aes = list(shape = NA)))

  SenLabel<-ifelse(IsSeasonal,"Annual Seasonal Sen Slope","Annual Sen Slope")
  mysub<-ifelse(IsSeasonal,paste0(myTimeIncr," time increments; ",mySeason," seasons"),paste0(myTimeIncr," time increments; non-seasonal"))
  if(is.null(mymain)==FALSE){mymain<-paste0(mymain,"\n",mysub)}else{
    mymain<-mysub}
  myplot<-myplot+ggtitle(mymain)+theme(plot.title = element_text(hjust = 0.5))


  mylegend=c(paste("% ",SenLabel," = ", round(Percent.annual.change,1),"%\n",
                   SenLabel," = ", signif(AnnualSenSlope,3),
                   "\nConfidence trend is decreasing = ",round(Cd,3)))
  xp=diff(range(xall$myDate))*0.03+min(xall$myDate)

  if(CropYaxis !=FALSE){
    gb=ggplot_build(myplot)
    ymin = gb$layout$panel_params[[1]]$y.range[1]
    ymax1=gb$layout$panel_params[[1]]$y.range[2]
    ymax=max(min(ymax1,CropYaxis*sqrt(var(xall$Values))+median(xall$Values)),max(Trends$y))

    myplot<-myplot+ylim(c(ymin,ymax))
    if(ymax<ymax1){
      mynote=c(paste0("Note: y-axis cropped at ",CropYaxis," s.d.\n",
                      "Max obs. value  = ", signif(max(xall$Values),3)))

      myplot<-myplot +
        geom_label(data=data.frame(label=mynote,x=as.Date(Inf), y=as.Date(Inf)),
                   aes(label=label,x=x, y=y),fill="white",label.size=0,vjust="inward",
                   hjust="inward",label.padding=unit(0.7,"lines"))
    }
  }

  myplot<-myplot +
    geom_label(data=data.frame(x=as.Date(-Inf), y=as.Date(Inf),label=mylegend),aes(
      label=label,x=x, y=y),fill="grey95",label.size=0,vjust="inward",
      hjust=0,label.padding=unit(0.7,"lines"))

  #
  # myplot<-myplot +
  #   geom_label(label=mylegend,x=-Inf, y=Inf,fill="grey95",label.size=0,vjust="inward",
  # hjust=0,label.padding=unit(0.7,"lines"))
  return(myplot)

}
