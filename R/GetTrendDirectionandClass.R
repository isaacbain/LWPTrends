GetTrendDirectionandClass<-function(A3){
  TrendClass <- "Insufficient Data" # default assumption is  that there is insufficient data to define trend direction
  if(A3$Cd >= 0.95)  TrendClass <- "Decreasing"
  if(A3$Cd <= 0.05)  TrendClass <- "Increasing"
  if(is.na(A3$C))  TrendClass <- "Not Analysed"

  TrendDirection<- "Indeterminate" # rare - but dooes occur!
  if(A3$Cd > 0.5 )  TrendDirection <- "Decreasing"
  if(A3$Cd < 0.5)  TrendDirection <- "Increasing"
  if(is.na(A3$S))  TrendDirection <- "Not Analysed"
  return(data.frame(TrendDirection=TrendDirection,TrendCategory=TrendClass))
}
