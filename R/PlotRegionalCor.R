PlotRegionalCor<-function(x){
  ########################
  # cross correlations
  ########################
  ObservationCorrelations <- ldply(names(x), function(y) { # x=names(AdjustedPdValues1)[1]
    theData <- x[[y]]$ObservationCorrelations
    theData <- data.frame(analyte = y, Correlations = theData)
    return(theData)
  })

  ObservationCorrelations$analyte <- factor(ObservationCorrelations$analyte)


  p <- ggplot(ObservationCorrelations, aes(x=Correlations)) +
    facet_wrap( ~ analyte,drop=T) +
    geom_histogram(aes(y=100*after_stat(count)/tapply(after_stat(count),after_stat(PANEL),sum)[after_stat(PANEL)]))+geom_vline(xintercept = 0, col="red", lty=2)+
    xlab("Pearson correlation coefficient") + ylab("Relative frequency (%)") + theme_bw(base_size = 12); p
  return(p)
}
