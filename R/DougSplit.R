DougSplit  <- function(mytext, myPart = 1, ...) {  # use: sapply(MyNames, DougSplit, split = "\\.", myPart = 2)
  myOut <- strsplit(mytext, ...)[[1]][myPart]      # note space is DougSplit("1996-02-07 00:00:00", split = "\\s")
  return(myOut)
}
