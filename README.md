
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OpenLong

<!-- badges: start -->
<!-- badges: end -->

OpenLong harmonizes commonly used longitudinal data sets on aging. Its
purpose is to facilitate machine learning benchmark studies, prediction
modeling, and meta-analyses, enabling researchers to perform more
efficient and accurate analyses.

The cohorts currently available for harmonization are:

1.  **Health ABC**: The Health, Aging and Body Composition Study ABC

2.  **CHS**: The Cardiovascular Health Study

3.  **MESA**: The Multi-Ethnic Study of Atheroscelerosis

<!-- 4. **WHI-CTOS**: The Women's Health Initiative: Clinical Trial and Observational Study -->

4.  **ARIC**: Atherosclerosis Risk in Communities Study

Any combination of these data sets can be harmonized and output in a
standardized format which consists of two data sets:

1.  **baseline.csv**: A cross-sectional data set with information on
    each patient at the baseline of the study.

2.  **long.csv**: A longitudinal data set with information on each
    patient collected as a sequence of time points.

## Installation

You can install the development version of OpenLong like so:

``` r
remotes::install_github("briannathanwhite/OpenLong")
```

## Example

TBA

``` r
library(OpenLong)
## basic example code
```
