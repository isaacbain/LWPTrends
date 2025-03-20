AdjustValues_GGPlots=function(myFits=NA,myFrame2=NA,ZerosHere=NA,
                              plotpval=FALSE,ModR2_p=NA,
                              Covariate='Flow',mymain=NA){

  myplot<-ggplot(myFits,aes(x=myx,y=myy,colour=Method))+geom_line(linewidth=1.3)+
    theme_bw()+geom_point(data=myFrame2,aes(y=XXValues,x=XXFlow),colour="black")+
    theme(legend.position = "inside",
          legend.position.inside = c(0.05,0.95),
          legend.background = element_rect(colour="black",fill="white"),
          legend.justification = c(0, 1))+
    xlab(Covariate)+ylab("Value")

  if(is.na(mymain)){myplot<-myplot+ggtitle("Covariation plot") }else{
    myplot<-myplot+ggtitle(paste0(mymain),"\n Covariation plot")}

  if(plotpval==TRUE){
    myplot<-myplot+scale_color_discrete(labels=ModR2_p$lab)
  }
  return(myplot)
}
