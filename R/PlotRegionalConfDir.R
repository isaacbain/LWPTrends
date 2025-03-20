PlotRegionalConfDir<-function(x,mymain=NA,DirType="Decrease"){
  x<-x[!is.na(x$TAU),]
  TauPlot <- x[, c("analyte", "TAU","M", "DT", "CT", "ConfCat")]
  TauPlot$Status <- "Corrected"
  TauPlot2 <- x[, c("analyte", "TAU", "M", "DT", "UncorrectedCT", "UncorrectedConfCat")]
  TauPlot2$Status <- "Uncorrected"
  names(TauPlot2) <- names(TauPlot)

  TauPlot <- rbind.data.frame(TauPlot, TauPlot2)
  shape_leg<-c(24,25)
  if(DirType=="Decrease"){
    TauPlot$Direction <- factor(TauPlot$DT, levels = c("Increasing", "Decreasing"))
    names(shape_leg)<-c("Increasing", "Decreasing")
  }else{
    TauPlot$Direction <- factor(TauPlot$DT, levels = c("Degrading", "Improving"))
    names(shape_leg)<-c("Degrading", "Improving")
  }
  TauPlot$Status <- factor(TauPlot$Status, levels = c("Corrected", "Uncorrected"))
  TauPlot$X_lab <- paste0(TauPlot$analyte, " (", TauPlot$M, ")")
  # TauPlot$X_lab <- factor(TauPlot$X_lab, levels = levels(CatConfSitesStak$Strip)) # get order correct

  TauPlot$ConfCat <-factor(TauPlot$ConfCat ,  levels = c("As likely as not",
                                                         "Likely",
                                                         "Very likely",
                                                         "Highly likely"))


  p1 <- ggplot(data=TauPlot[TauPlot$Status == "Corrected", ],
               aes(y=TAU,x=X_lab,shape=Direction,fill=ConfCat))+
    geom_point(size=7,show.legend = T)+scale_shape_manual(values=shape_leg)+
    scale_fill_viridis(discrete = T, direction = 1, drop=F) +
    theme(panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA,colour="grey70",linewidth=1.2), legend.position="bottom",
          legend.key= element_rect(fill="white"),axis.text.x=element_text(angle=45,vjust=0.5),
          plot.title=element_text(size=13,hjust=0.5), panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5,linetype = 2),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5,linetype = 2),legend.box="vertical") +
    ylab(expression(Aggregate~trend~strength~"("~hat(Tau)~")")) + xlab("Variable")+
    guides(fill = guide_legend(title.position = "top", override.aes=list(shape = 21),order=0),shape=guide_legend(order=1))+ylim(c(0.5,1))+
    labs(fill=expression(Confidence~aggregate~trend~at~regional~level~"(C"^Tau~")"), shape=expression(Direction~"(D"^Tau~")"))
  if(!is.na(mymain)){p1<-p1+ggtitle(mymain)}

  return(p1)
}
