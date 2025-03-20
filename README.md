
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LWPTrends

<!-- badges: start -->
<!-- badges: end -->

The goal of LWPTrends is to provide functions to undertake water quality
trend analysis in the R statistical computing environment.

## Installation

You can install the development version of LWPTrends from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("isaacbain/LWPTrends")
```

## Introduction

This document describes how to use the functions in the LWP-Trends
library to undertake water quality trend analysis in the R statistical
computing environment. The functions were developed from scratch to
provide up to date methods to evaluate trends using non-parametric
methods based on the Mann-Kendall correlation coefficient and the
Theil–Sen estimator. An update to the methods description and output was
made in April 2021 in order to align the methodology description and
terminology to recent New Zealand guidelines for trend assessment
(Snelder et al. 2021). We recommend the reader refers to these
guidelines for a more detailed description of the methods; only a
summarised description is provided within this help document. Updates in
this release are to provide some corrections, incorporate some
additional utilities, as well as to:

1.  Provide new functions to evaluate aggregate trend assessments,
    following Snelder et al. (2022). The method assesses the modal
    direction of individual trends at multiple sites (i.e., the
    aggregate trend direction) and assesses the confidence associated
    with this determination while taking into account the spatial
    autocorrelation between sites. Previous functions that evaluated
    aggregate trends are superseded and removed from the library.
2.  Allow for separate specification of the time increment of analysis
    (TimeIncr) and the season (Season) of the analysis. Previous
    versions required the time increment and season to be the same,
    which was specified in the data by a variable called Season. This
    change was for two reasons. First, the terminology was confusing
    when dealing with non- seasonal trend assessments. Second, the
    flexibility to assign TimeIncr and Season separately should
    generally increase the statistical power of the trend assessments by
    simultaneously reducing variability associated with seasonality but
    retaining as many observations as possible.
3.  Removal of the high censor filter. The inclusion of the high censor
    filter had been a pragmatic choice following recommendations of
    Helsel et al. (2020. p. 357) to account for changes in censor level.
    However, it is unnecessary to apply this filter to the evaluation of
    Kendall’s S as implemented in LWPTrends, as the formulation already
    robustly accounts for varying censor levels (following cenken() from
    the NADA package and implemented in LWPTrends by the GetKendal()
    function).
4.  The calculation of the Sen Slope now accounts for varying censor
    levels in the same manner as the GetKendal(), ensuring that slopes
    are treated as ties between higher left censored data and lower
    non-censored data (or conversely lower right censored data and
    higher non-censored data). This removes the need to have a high
    censor filter. More details are provided in section 4.2.
5.  Make adjustments to date metadata for sampling dates that should be
    associated with a previous or following month. This occurs by
    default in the function GetMoreDateInfo(), but can be turned off by
    setting the argument FindDateShifts=FALSE. Sampling might, for
    example, be consistently carried out towards the end of the month.
    However, on occasion, a sample occasion associated with a monthly
    monitoring programme for a specific month might occur on the first
    day of the following month, leading to two samples in the following
    month, and none in the previous month. If this happens frequently,
    the site may not comply with filtering rules (i.e., minimum data
    requirements for a trend assessment to be judged robust). However,
    by reassigning observations at the end of the month to the following
    month there would be sufficient approximately monthly observations.
    We have added an option to make adjustments to the date metadata in
    these situations; the method is described in section 3.2.

The changes listed above have relatively minor implications for the
evaluated trend confidence and trend rates. The more significant effect
is that fewer analyses will have insufficient data and therefore fewer
analyses will be categorised as “not analysed”.

## Trend Analysis Method

The purpose of trend assessment is to evaluate the direction (i.e.,
increasing or decreasing) and rate of the change in the central tendency
of the observed water quality values over the period of analysis (i.e.,
the trend). Because the observations represent samples of the water
quality over the period of analysis, there is uncertainty about the
conclusions drawn from their analysis. Therefore, statistical models are
used to determine the direction and rate of the trend and to evaluate
the uncertainty of these determinations.

### Trend direction assessment

The trend direction and the confidence in the trend direction were
evaluated using either the Mann Kendall assessment or the Seasonal
Kendall assessment. Although the non-parametric Sen slope regression
also provides information about trend direction and its confidence, the
Mann Kendall assessment is recommended, rather than Sen slope
regression, because the former more robustly handles censored values.
However, Sen slope regression is the recommended method for assessing
the trend rate (see Section 2.2). The Mann Kendall assessment requires
no a priori assumptions about the distribution of the data but does
require that the observations are randomly sampled and independent (no
serial correlation) and that there is a sample size of ≥ 8. Both the
Mann Kendall and Seasonal Kendall assessments are based on calculating
the Kendall S statistic, which is explained diagrammatically in Figure
4.

The Kendall S statistic is calculated by first evaluating the
differences between all pairs of water quality observations (Figure 1, A
and B). Positive differences are termed ‘concordant’ (i.e., the
observations increase with increasing time) and negative differences are
termed ‘discordant’ (i.e., the observations decrease with increasing
time). Pairs of observations that are tied in value or tied in time[^1]
(i.e., have the same Season-Year) are assigned differences of zero
(Gilbert, 1987, p. 218). The Kendall S statistic is the number of
concordant pairs minus the number of discordant pairs (Figure 1, C1).
The water quality trend direction is indicated by the sign of S with a
positive or negative sign indicating an increasing or decreasing trend,
respectively (Figure 1, C2). The seasonal version of the Kendall S
statistic S is calculated in two steps. First, for each season, the S
statistic is calculated in the same manner as shown in Figure 1, but for
data pertaining to observations in each individual season. Second, S is
the sum of values over all seasons (𝑆 = 𝑛 ∑ 𝑆𝑖 1 ), where Si is the
number of concordant pairs minus the number of discordant pairs in the
ith season and n is the number of seasons. The variance of S is
calculated for each season and then summed over all seasons. The sign
(i.e., + or -) of the S statistic calculated from the sample represents
the best estimate of the population trend direction but is uncertain
(i.e., the direction of the population trend cannot be known with
certainty). A continuous measure of confidence in the assessed trend
direction can be determined based on the posterior probability
distribution of S, the true (i.e., population) difference in concordant
and discordant pairs (Snelder et al., 2022). The posterior probability
distribution of S is given by a normal distribution with mean of S and
variance of var(S). The confidence in assessed trend direction can be
evaluated as the proportion of the posterior probability distribution
that has the same sign as S. In practice. confidence can be calculated
by first transforming the value of 𝑆 = 0 on the posterior probability
distribution into a standard normal deviate, Z (panel C2). C is then
calculated as area under the standard normal distribution to the left
(Z\>0) or right (Z\<0) of the value of Z, using the quantile function
for the normal distribution. The value 𝐶 can be interpreted as the
probability that the sign of the calculated value of S indicates the
direction of the population trend (i.e., that the calculated trend
direction is correct). The value 𝐶 ranges between 0.5, indicating the
sign of S is equally likely to be in the opposite direction to that
indicated by the true trend, to 1, indicating complete confidence that
the sign of S is the same as the true trend. As the size of the sample
(i.e., the number of observations) increases, confidence in the trend
direction increases. When the sample size is very large, 𝐶 can be high,
even if the trend rate is very low. It is important therefore that 𝐶 is
interpreted correctly as the confidence in direction and not as the
importance of the trend. As stated at the beginning of this section;
both trend direction and the trend rate are relevant and important
aspects of a trend assessment.

### Assessment of trend rate

The method used to assess trend rate is based on non-parametric Sen
slope regressions of water quality observations against time. The Sen
slope estimator (SSE; Hirsch et al., 1982) is the slope parameter of a
non-parametric regression. SSE is calculated as the median of all
possible inter- observation slopes (i.e., the difference in the measured
observations divided by the time between sample dates; Figure 2).

The seasonal version of the SSE is used in situations where there are
significant (e.g., p ≤ 0.05, as evaluated using a Kruskall Wallis test)
differences in water quality measurements between ‘seasons’. Seasons are
defined primarily by sampling intervals, which are commonly monthly or
quarterly for water quality monitoring but can also be defined (as of
release version 2502) as whole multiples of the sampling interval (i.e.,
monthly sampling interval and quarterly season). The seasonal Sen slope
estimator (SSSE) is calculated in two steps. First, for each season, the
median of all possible inter-observation slopes is calculated in same
manner as shown in Figure 2 but for data pertaining to observations in
each individual season. When the time increment is smaller than the
Season time increment (i.e. there is generally more than one observation
per Season by Year) the inter-observation slopes are excluded for pairs
of observations that are in the same season-year, but all slopes in the
season of one year are compared with all slopes in the season of the
other years (Gilbert, 1987, p. 218). Second, SSSE is the median of the
seasonal values. Uncertainty in the assessed trend rate is evaluated
following a methodology outlined in Helsel and Hirsch (2002). To
calculate the 100(1-α)% two-sided symmetrical confidence interval about
the fitted slope parameter, the ranks of the upper and lower confidence
limits are determined, and the slopes associated with these observations
are applied as the confidence intervals.

### Handling Censored Values

Censored values are those above or below a detection limit (e.g., \>2.5
or \<0.001). Values above the detection limit are described as right
censored and values below the detection level are described as left
censored. Trends are most robust when there are few censored values in
the time-period of analysis.

When calculating point statistics such as means, standard deviations or
quantiles, it is appropriate to replace censored values with imputed
values. The LWP-Trends library included the functions Impute.lower() and
Impute.upper(), which impute replacement values for lower (left)
censored data and upper (right) censored data respectively. The
Impute.lower() function is based on regression on order statistics (ROS)
function from the NADA package. The Impute.upper() function is based on
the survreg() function which is also from the NADA package. Both methods
are based on fitting a distribution to the non-censored values and using
that model to impute replacement values for the censored values and are
described in detail in Nondetects and Data Analysis for Environmental
Data (Helsel, 2005) and Statistics for censored environmental data using
MINITAB and R (Helsel, 2012). Censored values in the data used to
calculate Kendall’s S and its p-value are robustly handled in the manner
recommended by Helsel (2005, 2012). Briefly, for left-censored data,
increases and decreases in a water quality variable are identified
whenever possible. Thus, a change from a censored data entry of \<1 to a
measured value of 10 is considered an increase. A change from a censored
data entry of \<1 to a measured value 0.5 was considered a tie, as is a
change from \<1 to a \<5, because neither can definitively be called an
increase or decrease. Similar logic applied to right censored values.
The information about ties is used in the calculation of the Kendall S
statistic and its variance following Helsel (2012) and this provides for
robust calculation of the p- value associated with the Kendall test.
This approach is implemented in LWPTrends using source code from the
NADA package. The method is robust to changes in detection limit over
time. Note that as the proportion of censored values increases, the
proportion of ties increases and the confidence in the trend direction
decreases. Therefore, confidence in direction tends to be low when
trends are calculated from data with high proportions of censored
observations. When calculating Sen slopes, the inter-observation slope
cannot be definitively calculated between any combination of
observations in which either one or both are censored. Therefore, when
SSE and SSSE (i.e., Sen slopes) are calculated by the LWPTrends library,
the censored data entries are substituted with their corresponding raw
values (i.e., the numeric component of a censored data entry) multiplied
by a factor (0.5 for left-censored and 1.1 for right-censored values).
This ensures that any measured value that is equal to a raw value is
treated as being larger than the censored value if it is left-censored
value and smaller than the censored value if it is right-censored. The
inter-observation slopes associated with the censored values are
therefore imprecise (because they are calculated from the substituted
values). However, because the Sen slope is the median of all the
inter-observation slopes, the Sen slope is unaffected by censoring when
a small proportion of observations are censored. As the proportion of
censored values increase, the probability that the Sen slope is affected
by censoring increases. In previous versions of LWPTrends, the influence
of changes in detection limits over time on the Sen slope were handled
through a high censor filter. Briefly, this filter replaced all
observations below the highest detection limit (or some user specified
value) to the highest detection limit and marked these observations as
censored. We now implement a different approach that follows the logic
used in the calculation of Kendall’s S. Inter-observation slopes are
considered to be ties and set to zero, regardless of their values, when:
(1) both observations were either left or right censored, (2) when one
observation is left censored and larger than the other non-censored
observation; (3) when one observation is right censored and smaller than
the other non-censored observation. This approach has the advantage that
the user decisions around if and how to implement a high censor filter
are removed.

Helsel (1990) estimated that the impact of censored values on the Sen
slope is negligible when fewer than 15% of the values are censored.
However, this is a rule of thumb and is not always true. Depending on
the arrangement of the data, a small proportion of censored values
(e.g., 15% or less) could affect the computation of a Sen slope (Helsel,
2012). To provide information about the robustness of the SSE and SSSE
values, the output from LWPTrends includes the proportion of
observations that were censored and whether the Sen slope (i.e., the
median of all inter- observation slopes) was calculated from
observations that were censored. The estimate of the magnitudes (i.e.,
the SSE and SSSE values) and confidence intervals of individual site
trends decreases in reliability as the proportion of censored values
increases. In addition, when there are censored values, greater
confidence should be placed in the statistics returned by the Kendall
tests (including the trend direction and the probability the trend was
decreasing).

## Data and preliminary set up

These functions are designed to undertake trend analyses on data
pertaining to a single site + variable. The functions can be used to
analyse data that pertains to many sites + variable combinations by
applying the functions to appropriately sub-setted data using (for
example the ddply function from the plyr package). It is expected that
the data is in an R data frame format such that each water quality
observation is a row. Each row must have columns that define the value,
the date and the value of any covariate (e.g., flow). If the data
pertains to many sites + variable combinations, a column must specify
the variable name. An example of this type of data is shown in Figure 3.

### Date Format

The first step is to add a column called myDate that represents the date
of each observation as a vector of class “Date”. This is achieved for
the above data (which are a data frame called WQData) with following
command:

`WQData_Ex1a$myDate <- as.Date(as.character(WQData_Ex1a$sdate),"%Y-%m-%d")`

*Note, the format “%Y-%m-%d”will need to be adjusted to match the format
of the user input data.*

### Adding additional date labels

Additional date information used by the subsequent trend functions is
added to the data with the function `GetMoreDateInfo()`. This function
uses `myDate` and has an optional argument `firstMonth` that can be used
to choose an alternative start month for the analysis (i.e.,
`first month=7` would provide time increments that start in July and
output a “custom year” (or water-year) that starts in July, the default
is that the year starts in January). The function also provides
additional columns (date metadata) describing the months, 2-months
(`BiMonth`), quarters, twice annual (`BiAnn`), and years – these are
required for the analysis, and are common choices for the time increment
or season of the dataset. The additional time increment options are all
started at the start month and are labelled based on the start and end
month of the increment (i.e., quarters might be: Jan.to.Mar, Apr.to.Jun;
Jul.to.Sept; Oct.to.Dec). In the 2502 release, this function now
generates a default estimate of the time increment of the data
(`TimeIncr`), based on a user defined quantile for the spacing between
observations. The 2502 version also includes a function that looks for
months that have two or more observations in combination with a month
before or after that month that is missing an observation. Where the
previous month has a missing observation, the first observation is
assigned metadata associated with the previous month, and where the
following month has a missing observation, the last observation is
assigned metadata associated with the following month. The observations
retain the original date for use in the SenSlope evaluation. The date
metadata columns in the output are factors (e.g. BiMonth, Qtr, BiAnn).
If the year is shifted, the factor levels for these additional columns
are also shifted (this is useful for plotting purposes later).

`WQData_Ex1b<-GetMoreDateInfo(WQData_Ex1b,firstMonth = 7)`

### Time increments

The 2502 release has separated time increments from seasons. Now,
seasons are only used as the factor to evaluate seasonality and to
perform a seasonal trend assessment. Seasons can now be coarser (but a
multiple of) the time increment. See 4.1.2 for details about selecting
an appropriate Season. The default time increment is defined by the
GetMoreDateInfo function, but the user can override this, e.g.,:

`WQData_Ex1b$TimeIncr<-WQData_Ex1b$Month`

*Note, the time-increment must be a factor. Any user-defined
time-increment can be analysed.*

### Processing Censored Data

The next step assumes that the water quality measures contain less than
and greater than signs that signify the data are censored (below
detection limit is “\<” and above the “reporting limit” “\>”). These
must be converted to their face values + information concerning
censoring. This is achieved with the function `RemoveAlphaDetect()` as
follows:

`WQData_Ex1b <- RemoveAlphaDetect(WQData_Ex1b,ColToUse="Value")`

This output is a data frame with the original dataset plus three
additional columns. The columns additional columns represent the face
value of the water quality measures (named RawValue), a logical
indicating if the observation was censored (named Censored) and the type
of censoring (less than (lt), greater than (gt) or not censored (not).

## Performing a trend analysis

The following sections present the main functions of the LWPTrends
library, providing examples from the accompanying demonstration dataset.

### Inspecting the data

#### Inspect trend data

The function InspectTrendData() assists with inspection of the data and
the selection of an appropriate time increment, given some minimum data
requirements. The function also evaluates whether the data are . It is
assumed that the trend analysis is for a specific time-period that is
defined by the arguments Year (specifies whether to use a calendar or
custom year), TrendPeriod (number of years) and EndYear. The function
outputs a dataframe that cuts the input data down to the specified time
period and adds on a column TimeIncr, which is the highest observation
frequency that meets the minimum data requirements (which are specified
by propIncrTol and propYearTol, the proportion of time increment x years
and proportion of years which must have observations).

The function also optionally outputs a data frame (when
ReturnALLIncr=TRUE) that identifies the trend period length
(TrendPeriodL), total number of observations (nobs), number of years
with observations (nYear), proportion of years with observations
(propYear), the proportion of observations that are censored (propCen),
the number of unique left censored values (nCenLevelsLT), the number of
unique right censored values (nCenLevelsGT) and the number of
observations with flow data (nFlow). For each possible time increment
(monthly, bimonthly, quarterly, biannual and annual) the total number of
time increment x years with observations (nIncrYear) is provided, as
well as the proportion of all possible time increment x years for the
trend period with observations (propIncrYear). Based on the user defined
minimum data requirements, each possible time increment is assigned a
logical value to DataOK, to specify whether the time increment meets the
minimum data requirements. The time increment used in the first
dataframe output is designated as the highest frequency time increment
where DataOK is TRUE.

The function also optionally outputs four graphs: a time series plot; a
matrix plot of the data; a matrix plot of the censoring; and a matrix
plot of the number of observations per month. Matrix plots are always
shown with months on the x-axis and calendar years of the y-axis. This
can assist with visualising the sampling frequency and any changes in
frequency over time. All four plots can be shown together with the
following command:
ggarrange(Out1a\[\[3\]\]\[\[1\]\],ggarrange(Out1a\[\[3\]\]\[\[2\]\],Out1a\[\[3\]\]\[\[3\]\],Out1a\[\[3\]\]\[\[4\]\],nr
ow=1,align=“h”),nrow=2)

#### Season and Seasonality Test

The function SeasonalityTest() tests if the data are seasonal. If the
data are seasonal (p- Value\<0.05), the trend analysis should be
seasonal (i.e., the Kendall test should be the seasonal Kendall test and
the Sen slope should be a seasonal Sen slope). The SeasonalityTest()
function performs a Kruskal Wallis test (non-parametric ANOVA) on the
observations using season as the explanatory (categorical) variable. A
test is also performed to check whether the season would meet the
minimum data requirements for the trend assessment (i.e., minimum number
of samples and unique values per season). If these requirements are not
met, the data is assessed as non- seasonal, as a trend assessment with
this season would return a “Not Analysed” result. The consequence of
analysing as non-seasonal will generally be to have lower confidence in
direction (and large Sen slope confidence intervals) than had the
analysis been seasonal. The SeasonalityTest() function has an option to
return a plot summary of the seasonality test, in which case it outputs
a list of: 1. A summary of the KW test outputs for the selected season
2. A box plot of the data grouped by season (e.g., Figure 6 and Figure
7). If do.plot is FALSE, SeasonalityTest() only returns a dataframe of
the KW test outputs.

As of version 2502, Season may be defined separately from the time
increment (TimeIncr) for the analysis. A new function GetSeason() has
been added (as of v2502) that selects an appropriate season by
evaluating the Kruskall Wallis test to determine which potential season
explains the most variability in the data. When performing this
selection, the possible options for Season are the selected time
increment, and any other increments (i.e., month, BiMonth, Qtr,BiAnn)
that are whole multiples of the time increment (these are defined
internally). The selected season is the time increment that has a p
value \<0.05 or, in the case that more than season increment meets this
threshold, has the largest KW statistic. The function is a wrapper for
SeasonalityTest(), and can return a dataframe of the data with a
“Season” column added, as well as the outputs available from
SeasonalityTest(). When data are not seasonal, “Season” is set to the
time increment. While Season is not required for subsequent non-seasonal
analysis, it ensures the same data structure across sites x variables.
The function also has an option (ReturnALLincr=TRUE) to print out the KW
test results for all Seasons tested:

### Non-seasonal trend analysis

The function NonSeasonalTrendAnalysis () performs a Mann Kendall test of
correlation between the observations and time, followed by a
non-parametric regression (Sen’s slope estimator, also known as the
Theil–Sen estimator) to the observations versus time. The function
handles the censored values as described in Handling Censored Values
section. The output is a data frame of the test results. P is the
two-tail p-value from the Mann Kendall test, C is the confidence in the
trend direction, and Cd is the confidence that the trend is decreasing.

Trend1b\<-NonSeasonalTrendAnalysis(WQData_Ex1b,mymain=“Ex 1b Raw Trend”,
Year=“CustomYear”,do.plot=T);Trend1b\[\[1\]\]

The NonSeasonalTrendAnalysis () function is a master function that calls
both the MannKendall() and SenSlope() functions. Each of these functions
can be standalone, but note that (1) if the outputs from the
MannKendall() function suggest that there are insufficient data, it is
not necessary to go on to run the SenSlope() function and (2) the
SenSlope() function requires the probability output from the
MannKendall() function as an input, for plotting.

The percentage annual change in trend slope is calculated as the Sen
slope divided by the median, multiplied by 100. The default is to
calculate the median using the face values of the RAW data (including
censored data). When there are insufficient data, the models will return
NULL outputs – labelled “Not analysed”. Trends were classified as “not
analysed” for two reasons: 1) When a large proportion of the values were
censored (data has \<5 non-censored values and/or \<3 unique
non-censored values). This arises because trend analysis is based on
examining differences in the value of the variable under consideration
between all pairs of sample occasions. When a value is censored, it
cannot be compared with any other value and the comparison is treated as
a “tie” (i.e., there is no change in the variable between the two sample
occasions). When there are many ties there is little information content
in the data and a meaningful statistic cannot be calculated. 2) When
there is no, or very little variation in the data (\<3 unique
non-censored values, or a long run of identical values), because this
also results in ties. This can occur because laboratory analysis of some
variables has low precision (i.e., values have few or no significant
figures). In this case, many samples have the same value resulting in
ties.

#### Analysis Notes: warnings description

Analysis notes return some warnings when Sen slopes are evaluated based
on tied values and/or censored values. When values are tied, the
estimated Sen slope will be zero. These warnings generally arise for
site/variable combinations with large proportions of censored values.
They indicate that the ‘true’ Sen slope cannot be evaluated due to a
lack of sufficient resolution of the measurement method (i.ee, the
smallest detectable change that the testing method can accurately
identify). Assuming the measurement resolution is fit for purpose, we
generally would recommend that the results with any of these warnings
are still used. “WARNING: Sen slope based on two censored values” This
message occurs when the pair of observations that produce the median
inter-observation slope are both censored. This can be \> or \<
censoring, although generally, we find that this is a pair of \<
censored values. This is common at sites with high levels of censored
data. The returned Sen slope will be zero, and this warning recognises
that the ‘true’ Sen slope (i.e., if the water quality measurements were
analysed and reported with higher resolution) would not be zero. In
other words, the ‘true’ Sen slope would be a small value that cannot be
evaluated due to the lack of resolution of the measurement method. Cases
where the Sen slope is reported as zero and UCI and LCI are also zero,
indicate that most observations are censored. In these cases, the ‘true’
Sen slope, UCI and LCI would be non-zero values if the resolution of the
measurement method were higher. Generally, in these cases the direction
of trend is also uncertain. “WARNING: Sen slope influenced by censored
values” This message occurs when the one of the two observations that
produces the median inter- observation slope is censored. This can be \>
or \< censoring, although generally, we find that this is associated
with a \< censored value. This warning generally occurs when there are
high levels of censoring. This warning recognises that the estimate of
the Sen slope uncertainty does not account for the imprecision
associated with calculating Sen slopes using values that are below the
detection limit. Theoretically, this imprecision adds to the uncertainty
of the Sen slope estimate, but this is not accounted for in the
calculations. If the detection limit is small relative to the range of
observation values, the additional uncertainty will be small. “WARNING:
Sen slope based on tied non-censored values” This message occurs when
the pair of observations that produce the median inter-observation slope
are the same but are not censored. In this case, the Sen Slope is
reported as zero. This is common at sites where the observations
comprise many repeated values, (and this is generally due to low
resolution of the measurement method). The returned Sen slope is not the
‘true’ Sen slope due to insufficient measurement resolution. The ‘true’
Sen slope would be a small value that cannot be evaluated due to the
lack of sufficient resolution of the measurement method.

## Seasonal Trend Analysis

The function SeasonalTrendAnalysis() performs a seasonal Kendall test of
correlation between the observations and time and a seasonal version of
the non-parametric regression (Sen’s slope estimator) of the
observations versus time. The seasonal trend analysis has been updated
to accommodate the possibility of differences in the assignment of the
time increment and the season. When the Season increment is larger than
the specified time increment, all observations within the same Season
and Year are treated as ties in time when calculating S and the variance
of the S statistic for the Mann Kendall test (following methods
described in Gilbert, 1987, p. 243).

Further, when evaluating the seasonal Sen slope, slopes are not included
between observations with the same Season and Year, but all slopes in
the season of one year are compared with all slopes in the season of the
other years (Gilbert, 1987, p. 218). This approach should increase the
statistical power of the trend assessments by simultaneously reducing
variability associated with seasonality, but retaining as many
observations as possible. The function handles the censored values as
described in Handling Censored Values section. The output is a data
frame of the test results.
Trend_ex2\<-SeasonalTrendAnalysis(WQData_Ex2,mymain=“Ex 1a Raw
Trend”,do.plot=T)

The same additional outputs as described for the Sen slope are also
provided for the seasonal Sen slope. Note, there are generally more not
analysed sites for the seasonal Sen slope tests, as there must be
sufficient data for each season. Note: in this example the dataset
includes two observations in March 2003 (10th and 31st), and none in
April 2003. The new date shift fix assigns the 31st of March observation
to April, as such, there is one more “observation” in the trend
assessment, compared to as analysed in versions prior to 2502.

## Batch processing trend assessments

For many common applications of water quality trend assessments, there
is a need to process multiple site x variable combinations at the same
time. In this update we have provided a wrapper function that can be
used in conjunction with ddply or dplyr to assist with this task:
doMyTrends_v2502. It can also be used with a single site x variable
timeseries. Before using this function, it is still necessary to clean
data appropriately, and to run the functions GetMoreDateInfo and
RemoveAlphaDetect. The function then executes the following steps: 1.
InspectTrendData a. Cuts dataset down to specified trend period b. Adds
on a column TimeIncr to describe the highest frequency time increment
that meets the minimum data requirements 2. GetSeason a. Evaluates
whether data are seasonal and adds the column SeasIncr to the data 3.
Run either NonSeasonlTrendAnalysis or SeasonalTrendAnalysis My10yTrends
\<- ddply(RegionExData, c(“siteID”, “analyte”), function(x) {
doMyTrends_v2502(x, Year = “CustomYear”, propIncrTol=0.9,
propYearTol=0.9,TrendPeriod=10, EndYear=2020, ValuesToUse=“RawValue”,
UseMidObs=TRUE, do.plot=FALSE)}, .progress = “win”)

## Covariate adjustment

While the functionality to undertake covariate adjustment is retained in
LWPTrends, we do not recommend using flow adjusted trends for water
quality data for what Snelder et al. (2021) refer to as regional
applications (i.e., assessing and reporting trends across many sites and
variables in the context of regional state of environment reporting).
The purpose of flow-adjustment is to remove the confounding effect of
flow so that the pattern of interest (the relationship between the
observed water quality observations and time i.e., the trend) can be
more confidently inferred. However, the definition of models describing
observations - instantaneous flow is subjective and therefore there are
unquantified uncertainties that arise due to procedural choices around
flow adjustment that are likely to be made by individual analysts.
Furthermore, there is evidence that trends are often associated with
changes in the relationship between concentration and flow through the
trend’s time period (Snelder and Kerr, 2022). However, flow adjustment
(based on defining a relationship between observations and instantaneous
flow) assumes that the concentration - flow relationship is constant
through time. Violations of this assumption will affect the robustness
of flow adjustment. The function AdjustValues() performs an adjustment
of the data based on a covariate (for example flow if the data
represents river concentrations). The function fits a variety of models
to the observation versus covariate relationship. The user needs to
consider which of these models is the most appropriate basis for
adjustment. Censored values are used in fitting the observation versus
covariate relationship after values below detection limits are set at
0.5*face value and values above reporting limits are set to 1.1*face
value. This follows the same conventions used in the Sen Slope
assessment. In order that these adjustments are not repeated in the Sen
Slope assessment, an argument is used to identify that the data are not
based on raw values. The function returns a data frame with the adjusted
values (regression residuals) for each of the models. The column that
represents the chosen model in this data frame is then used in any of
the functions above. The adjustment is achieved with following command:
FlowAdjusted\<-AdjustValues(WQData_Ex1a, method = c(“Gam”, “LogLog”,
“LOESS”), ValuesToAdjust = “RawValue”, Covariate = “finalQ”, Span =
c(0.7), do.plot =T, plotpval=T, mymain=“Example 1a”) The adjusted output
is shown on Figure 12 and the plots are shown in Figure 13.
head(FlowAdjusted)

In the above example the LOESS model might be judged to be the most
appropriate, although no model is particularly robust in this case. The
adjusted values that are output by the GetAdjustment function are used
in any of the above four trend analysis functions with following
commands as follows:
WQData_Ex1a\<-merge(WQData_Ex1a,FlowAdjusted\[,c(myDate,LOESS0.7)\])
Trend_ex1aFA\<-SeasonalTrendAnalysis(WQData_Ex1a,
ValuesToUse=“LOESS0.7”, RawValues=FALSE,
ValuesToUseforMedian=“RawValue”, mymain=“Example 1a: Flow Adjusted
Trend”, do.plot=T)

## Trend Aggregation

The aggregated results of analysis of water-quality trends are intended
to provide an overview of recent water quality changes over a spatial
domain of interest (e.g., environmental classes, regions, or the entire
country). The LWP Trends functions provide two approaches for
aggregating trend directions. Inputs to these functions are a dataframe
comprising the outputs of the previously described trend analysis
functions with many rows, each being the outputs for a given site and
variable combination. The first type of trend aggregation is a plot of
aggregated confidence that the trend direction is decreasing. For each
site and variable, the confidence that a trend is decreasing (or its
complement; increasing) is expressed as a confidence category. The
confidence categories (Table 1) can be assigned using the function
AssignConfCat(): AllTrends10FA\$DirectionConf \<-
AssignConfCat(x=AllTrends10FA,CatType=”Decrease”)

Functionality also exists to use to categorise confidence that trend
direction is improving. Each site trend is assigned to a category by
firstly, converting Cd into a confidence that a trend was improving
(Ci). Improvement is indicated by decreasing trends for most water
quality variables (Ci = Cd), but for some variables, increasing trends
indicated improvement (e.g., macroinvertebrate metrics, visual clarity,
Secchi depth, dissolved oxygen etc). For these variables, Ci is the
complement of Cd (i.e., Ci=1-Cd). For some variables, (e.g., pH,
temperature etc), improvement is not clearly related to increasing or
decreasing trends, and for these variables this approach should not be
used. Each site/variable combination can then be assigned to a
confidence in trend direction category according to its evaluated
confidence of improvement. An option also exists to provide a simpler
categorisation, following the categories shown in Table 1.
AllTrends10FA\$ImproveConf \<-
AssignConfCat(x=AllTrends10FA,CatType=”Improve”,Reverse=”MCI”,excl=”pH”,nCat=”Sim
ple”)

The categorical levels of confidence for all sites and variables are
then summarised using a colour coded bar chart (Figure 15). See the
demonstration in the example file: RunLWPTrendsExample_v2502.R.

The second approach uses a procedure for assessing aggregate trends
developed by Snelder et al. (2022). The method uses the directions of
the individual site trends derived using the Mann Kendall trend
assessment method to calculate the aggregate trend direction (𝐷𝑇) to be
the modal (i.e., most frequently occurring) direction of the individual
site trends and the aggregate trend strength (Τ ̂)[^2] as the proportion
of site trends that are in the modal direction. The confidence in the
assessed aggregate trend direction (𝐶Τ) is calculated from the
confidence in the direction of the individual site trends and adjusted
for spatial autocorrelation between sites using the method described by
Snelder et al. (2022). Briefly, the adjustment is necessary if there is
spatial correlation among sites within a monitoring network. Spatial
correlation means that the information representing the sites is not
independent and this results in under-estimation of the variance and
over-estimation of confidence in the assessed aggregate trend direction.
The method of Snelder et al. (2022) uses the sample cross-correlation
coefficient, which is computed from the observation time series for all
sites, to “correct” the variance to account for the spatial correlation.
𝐶Τ can be expressed as categorical levels of confidence in assessed
trend direction by four categories shown in Table 3. The function also
allows user to select whether the trend direction is expressed purely as
direction (i.e., increasing/decreasing) or as improvement (i.e.,
improving/degrading).

TauRegional \<- dlply(My10yTrends,.(analyte), function(y) getTAU(x=y,
obs=RegionExData\[RegionExData$analyte==y$analyte\[1\],\],
SiteNameCol=“siteID”, VarNameCol=“analyte”, ValuesToUse = “RawValue”,
Year = “CustomYear”, UseMidObs=T)) Extract summary table:
TauRegionalSummary \<- ldply(TauRegional, function(x)
return(x\$SummaryResults))

Histograms of the Pearson correlation coefficients between sites, by
variable, can be obtained from PlotRegionalCor() (Figure 16) and
aggregate trend direction, trend strength and confidence can be
summarised graphically by PlotRegionalConfDir() (Figure 17)

[^1]: Accounting for ties in time is an addition to the code since
    version 2502. This addition is to accommodate seasonal analyses
    where the Season time increment is larger than the data TimeIncr
    (time increment).

[^2]: Note that the Greek letter tau is used for trends corresponding to
    both individual sites and aggregated sites. When an individual site
    trend is being referred to the lower-case tau (𝜏) is used. When an
    aggregate trend is being referred to, the upper-case tau is used
    (Τ).
