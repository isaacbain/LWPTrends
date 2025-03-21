% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetMoreDateInfo.R
\name{GetMoreDateInfo}
\alias{GetMoreDateInfo}
\title{GetMoreDateInfo}
\usage{
GetMoreDateInfo(Data, firstMonth = 1, FindDateShifts = TRUE)
}
\arguments{
\item{Data}{Dataframe or vector containing myDate}

\item{firstMonth}{Specify the first month (numeric (1:12)) for the analysis year}

\item{FindDateShifts}{Logical. Indicates whether to look for observations that should be
representative of previous or following months.}
}
\value{
A data frame with fields as follows (in addition to those columns in x)\tabular{ll}{
\code{Year} \tab Calendar Year (numeric) \cr
\tab \cr
\code{CustomYear} \tab Custom Year (matches Calendar year of last month of the custom year) – only output if firstMonth!=1. numeric \cr
\tab \cr
\code{Month} \tab Month String (factor) \cr
\tab \cr
\code{BiMonth} \tab Bi-monthly (pairs of months) String (factor) \cr
\tab \cr
\code{Qtr} \tab Quarter string (factor) \cr
\tab \cr
\code{BiAnn} \tab Bi-annual (6-monthly increments) String (factor) \cr
}
}
\description{
Takes dates and produces additional columns summarising, months, bimonths
(samples ever 2 months), quarters, biannual (samples twice a year) and years.
If specified, the firstMonth can be used to shift the analysis year. This
also automatically shifts the factor levels of the months and quarters to
start from this month. The function also as an option to search for
observations that should be representative of a sample from the following (or
previous) month, and assign the additional date information associated with
the following (or previous) month, rather than the month of sampling.\cr\cr The
outputs from this function can be then used to specify a season for the
analysis. season definition would need to be manually shifted to reflect the
custom year, by the user.
}
