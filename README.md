
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
