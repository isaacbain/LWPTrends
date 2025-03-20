GetKendal <- function (x) { # x=Data

  # make some adjustments to data to get Helsel to agree (more closely) with TimeTrends.
  # TT sets all greater thans to a value slightly higher than their face value (and makes them all the same),
  # then switches off all censored values (all=0). This treats all greater thans as ties.
  if(sum(x$CenType == "gt") > 0) {
    MaxGTvalue <- max(x[x$CenType == "gt", "V1"])
    MaxGTvalue <- MaxGTvalue + 0.1
    x[x$CenType == "gt", "V1"] <- MaxGTvalue
    x[x$CenType == "gt", "Censored"] <- FALSE
  }


  # For less thans, TimeTrends checks to see if there are multiple less thans with measured values
  # between them. If so TimeTrends uses just uses Helsel code. If there are no real values between
  # them TT sets them all equal and slightly less than their face value (i.e., a < 0.005s the same value as <0.001s)
  # so that they form 1 group of ties. Helsel would treat them as two groups of ties.
  # if(sum(x$CenType == "lt") > 0) {
  #  TabLT <- as.data.frame.matrix(table(x$CenType, x$V1))
  #  ind <- TabLT["not", ] <= TabLT["lt", ] # what censored value has real values that are less?
  #  ind <- as.vector(ind)
  #  NoRealLess <- rle(ind)$lengths[1] # run length of index for which there are no real values less than;
  #  if(NoRealLess > 1) { # if first run>1, then tied less thans if true then set equal
  #    MaxCenVal <- as.numeric(names(TabLT)[NoRealLess])
  #    x$V1[x$CenType == "lt" & x$V1 <= MaxCenVal] <- MaxCenVal - 0.1*MaxCenVal
  #  }}

  x$SeasYr<-apply(x,1,function(y) paste0(y['Season'],y['Year']))
  SeasYrRank<-data.frame(SeasYr=unique(x$SeasYr))
  SeasYrRank$RankTime<-1:nrow(SeasYrRank)
  x$TimeRank<-SeasYrRank$RankTime[match(x$SeasYr,SeasYrRank$SeasYr)]

  xx <- x$V1
  cx <- x$Censored
  yy <- x$TimeRank # y is time and only needs to be  a sequentially increasing value
  cy <- rep(F, length.out = length(xx)) # no censored values for time
  n  <- length(xx)

  delx <- min(diff(sort(unique(xx))))/1000
  dely <- min(diff(sort(unique(yy))))/1000
  dupx <- xx - delx * cx
  diffx <- outer(dupx, dupx, "-")
  diffcx <- outer(cx, cx, "-")
  xplus <- outer(cx, -cx, "-")
  dupy <- yy - dely * cy
  diffy <- outer(dupy, dupy, "-")
  diffcy <- outer(cy, cy, "-")
  yplus <- outer(cy, -cy, "-")
  signyx <- sign(diffy * diffx)
  tt <- (sum(1 - abs(sign(diffx))) - n)/2
  uu <- (sum(1 - abs(sign(diffy))) - n)/2
  cix <- sign(diffcx) * sign(diffx)
  cix <- ifelse(cix <= 0, 0, 1)
  tt <- tt + sum(cix)/2
  signyx <- signyx * (1 - cix)
  ciy <- sign(diffcy) * sign(diffy)
  ciy <- ifelse(ciy <= 0, 0, 1)
  uu <- uu + sum(ciy)/2
  signyx <- signyx * (1 - ciy)
  xplus <- ifelse(xplus <= 1, 0, 1)
  yplus <- ifelse(yplus <= 1, 0, 1)
  diffx <- abs(sign(diffx))
  diffy <- abs(sign(diffy))
  tplus <- xplus * diffx
  uplus <- yplus * diffy
  tt <- tt + sum(tplus)/2
  uu <- uu + sum(uplus)/2


  test<-signyx * (1 - xplus) * (1 - yplus)

  itot <- sum(signyx * (1 - xplus) * (1 - yplus))
  kenS <- itot/2
  tau <- (itot)/(n * (n - 1))

  J <- n * (n - 1)/2
  taub <- kenS/(sqrt(J - tt) * sqrt(J - uu))

  varS <- n * (n - 1) * (2 * n + 5)/18

  intg <- 1:n
  dupx <- xx - delx * cx
  dupy <- yy - dely * cy
  dorder <- order(dupx)
  dxx <- dupx[dorder]
  dcx <- cx[dorder]
  dorder <- order(dupy)
  dyy <- dupy[dorder]
  dcy <- cy[dorder]
  tmpx <- dxx - intg * (1 - dcx) * delx
  tmpy <- dyy - intg * (1 - dcy) * dely
  rxlng <- rle(rank(tmpx))$lengths
  nrxlng <- table(rxlng)
  rxlng <- as.integer(names(nrxlng))
  x1 <- nrxlng * rxlng * (rxlng - 1) * (2 * rxlng + 5)
  x2 <- nrxlng * rxlng * (rxlng - 1) * (rxlng - 2)
  x3 <- nrxlng * rxlng * (rxlng - 1)
  rylng <- rle(rank(tmpy))$lengths
  nrylng <- table(rylng)
  rylng <- as.integer(names(nrylng))
  y1 <- nrylng * rylng * (rylng - 1) * (2 * rylng + 5)
  y2 <- nrylng * rylng * (rylng - 1) * (rylng - 2)
  y3 <- nrylng * rylng * (rylng - 1)
  delc <- (sum(x1) + sum(y1))/18 - sum(x2) * sum(y2)/(9 * n *
                                                        (n - 1) * (n - 2)) - sum(x3) * sum(y3)/(2 * n * (n-1))

  x4 <- nrxlng * (rxlng - 1)
  y4 <- nrylng * (rylng - 1)
  tmpx <- intg * dcx - 1
  tmpx <- ifelse(tmpx < 0, 0, tmpx)
  nrxlng <- sum(tmpx)
  rxlng <- 2
  x1 <- nrxlng * rxlng * (rxlng - 1) * (2 * rxlng + 5)
  x2 <- nrxlng * rxlng * (rxlng - 1) * (rxlng - 2)
  x3 <- nrxlng * rxlng * (rxlng - 1)
  tmpy <- intg * dcy - 1
  tmpy <- ifelse(tmpy < 0, 0, tmpy)
  nrylng <- sum(tmpy)
  rylng <- 2
  y1 <- nrylng * rylng * (rylng - 1) * (2 * rylng + 5)
  y2 <- nrylng * rylng * (rylng - 1) * (rylng - 2)
  y3 <- nrylng * rylng * (rylng - 1)
  deluc <- (sum(x1) + sum(y1))/18 - sum(x2) * sum(y2)/(9 *
                                                         n * (n - 1) * (n - 2)) - sum(x3) * sum(y3)/(2 * n * (n - 1)) - (sum(x4) + sum(y4))

  dxx <- dxx - intg * dcx * delx
  dyy <- dyy - intg * dcy * dely
  rxlng <- rle(rank(dxx))$lengths
  nrxlng <- table(rxlng)
  rxlng <- as.integer(names(nrxlng))
  x1 <- nrxlng * rxlng * (rxlng - 1) * (2 * rxlng + 5)
  x2 <- nrxlng * rxlng * (rxlng - 1) * (rxlng - 2)
  x3 <- nrxlng * rxlng * (rxlng - 1)
  rylng <- rle(rank(dyy))$lengths
  nrylng <- table(rylng)
  rylng <- as.integer(names(nrylng))
  y1 <- nrylng * rylng * (rylng - 1) * (2 * rylng + 5)
  y2 <- nrylng * rylng * (rylng - 1) * (rylng - 2)
  y3 <- nrylng * rylng * (rylng - 1)
  delu <- (sum(x1) + sum(y1))/18 - sum(x2) * sum(y2)/(9 * n *
                                                        (n - 1) * (n - 2)) - sum(x3) * sum(y3)/(2 * n * (n -  1))

  varS <- varS - delc - deluc - delu

  if (n >= 3 & !is.na(varS)&varS>0) {
    SigmaS <- varS^0.5
    if(kenS > 0)  Z <- (kenS-1)/SigmaS
    if(kenS == 0) Z <- 0
    if(kenS < 0)  Z <- (kenS+1)/SigmaS

    if(Z > 0)  p <- pnorm(Z, lower.tail = F)*2
    if(Z == 0) p <- 1
    if(Z < 0)  p <- pnorm(Z, lower.tail = T)*2
  } else {
    Z <- NA
    p <- NA
  }

  return(list(nTimeIncr = n, S = kenS, vars=varS,  D = J, tau = kenS/J, Z=Z, p=p))
}
