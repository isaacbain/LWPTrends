InspectData_GGPlots<-function(PlotData=NA,mymain=NA,Year='Year',DateRange=NA){
  #This produves 3 ggplot plots showing (1) a timeseries of data (2) a matri of values
  # and (3) a matrix of censoring
  #PlotData$theYear<-PlotData[,Year]
  #Only make plots if ggplot is loaded
  if("ggplot2" %in% (.packages())){
    PlotData$Censored<-factor(PlotData$Censored,levels=c("FALSE","TRUE"))
    plotlist=list()
    myTimeIncr<-WhatistheIncr(PlotData[!is.na(PlotData$V1),],val="TimeIncr")

    #1. Make plot of timeseries ##########
    mytitle_1<-paste0("Time Series of data")
    if(!is.na(mymain)){mytitle_1<-paste0(mymain,": ",mytitle_1)}

    myplot_1<-ggplot(data=PlotData[!is.na(PlotData$V1),], aes(x=NewDate,y=V1,colour=Censored,shape=Censored))+
      geom_point(show.legend = T)+theme_bw()+ylab('Value')+xlab('Date')+
      scale_colour_manual(values=c("FALSE"="black","TRUE"="red"),
                          labels=c("Observations","Censored"),
                          limits=c("FALSE","TRUE"),na.value="white", drop = FALSE)+
      scale_shape_manual(values=c("FALSE"=4,"TRUE"=16),
                         labels=c("Observations","Censored"),
                         limits=c("FALSE","TRUE"), drop = FALSE)+
      theme(legend.position.inside = c(0.2, 0.8),
            legend.background = element_rect(fill = "white",colour="black"),
            legend.title = element_blank(),
            plot.title = element_text(hjust=0.5))+
      ggtitle(mytitle_1)
    if(!is.na(DateRange[1])){
      myplot_1<-myplot_1+labs(subtitle = paste0(DateRange[1]," to ",DateRange[2]," (",
                                                round(as.numeric(DateRange[2]-DateRange[1])/365,0)," years)"))+
        theme(plot.subtitle = element_text(hjust=0.5))}

    plotlist[[1]]<-myplot_1
    #2. Make plot of matrix of values ##########
    # if(myTimeIncr!="Annual"){ #can't make matrix plots for annual data
    mytitle_2<-paste0("Matrix of values")
    if(!is.na(mymain)){mytitle_2<-paste0(mymain,": ",mytitle_2)}
    PlotData$YearFac<-factor(PlotData$Year,levels=rev(seq(min(PlotData$Year),max(PlotData$Year))))

    myplot_2<-ggplot(data=PlotData, aes(x=TimeIncr,y=YearFac,fill=V1))+
      geom_tile(colour="black",linewidth=0.1,show.legend = T)+theme_bw()+ylab(Year)+xlab("Month")+
      scale_fill_gradient(low="lemonchiffon",high="red2",na.value="white")+
      labs(fill="Value")+scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text(hjust=0.5),
            legend.position="bottom")+
      ggtitle(mytitle_2)

    plotlist[[2]]<-myplot_2


    #3. Make plot of matrix of cesnored values ##########
    mytitle_3<-paste0("Matrix of censoring")

    if(!is.na(mymain)){mytitle_3<-paste0(mymain,": ",mytitle_3)}

    myplot_3<-ggplot(data=PlotData, aes(x=TimeIncr,y=YearFac,fill=Censored))+
      geom_tile(colour="black",linewidth=0.1,show.legend = T)+theme_bw()+ylab(Year)+xlab("Month")+
      scale_fill_manual(values=c("FALSE"="skyblue2","TRUE"="red"),
                        labels=c(" NOT Censored"," Censored"),
                        limits=c("FALSE","TRUE"),na.value = "white", drop = FALSE)+
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      theme(axis.ticks = element_blank(), legend.title = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 0),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust=0.5),legend.position="bottom")+
      ggtitle(mytitle_3)
    plotlist[[3]]<-myplot_3

    #4. Make matrix of number of samples per month

    mytitle_4<-paste0("Matrix of obs. per month")
    if(!is.na(mymain)){mytitle_4<-paste0(mymain,": ",mytitle_4)}

    myplot_4<-ggplot(data=PlotData, aes(x=TimeIncr,y=YearFac,fill=nVal))+
      geom_tile(colour="black",linewidth=0.1,show.legend = T)+theme_bw()+ylab(Year)+xlab("Month")+
      scale_fill_gradient(low="pink",high="maroon4",na.value="white")+
      labs(fill="Count")+scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 0),
            plot.title = element_text(hjust=0.5),legend.position = "bottom")+
      ggtitle(mytitle_4)

    plotlist[[4]]<-myplot_4
  }else{
    print("*** WARNING: ggplot list not produced as ggplot2 not loaded ***")
  }
  return(plotlist)
}
