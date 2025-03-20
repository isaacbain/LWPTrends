# Make a list of the seasons that can be used for each of the time increments
TimeIncrToSeas <- list()
TimeIncrToSeas[["Monthly"]]    <- c("Monthly", "Bi-monthly", "Quarterly", "Bi-annual")
TimeIncrToSeas[["Bi-monthly"]] <- c("Bi-monthly", "Bi-annual")
TimeIncrToSeas[["Quarterly"]]  <- c("Quarterly", "Bi-annual")
TimeIncrToSeas[["Bi-annual"]]  <- c("Bi-annual")

# DefaultSeason labels
SeasonLabs <- data.frame(
  mySeas   = c("Monthly", "Bi-monthly", "Quarterly", "Bi-annual"),
  SeasName = c("Month", "BiMonth", "Qtr", "BiAnn"),
  stringsAsFactors = FALSE
)
