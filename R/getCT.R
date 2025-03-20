getCT <- function(TAU, VarTAU) {
  Z_05 <- (TAU-0.5)/sqrt(VarTAU)
  CT<-pnorm(Z_05)
  return(CT)
}
