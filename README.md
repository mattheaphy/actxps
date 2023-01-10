
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actxps

<!-- badges: start -->

[![R-CMD-check](https://github.com/mattheaphy/actxps/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mattheaphy/actxps/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The actxps package provides a set of tools to assist with the creation
of actuarial experience studies. Experience studies are used by
actuaries to explore historical experience across blocks of business and
to inform assumption setting for projection models.

- The `expose()` family of functions convert census-level records into
  policy or calendar year exposure records.
- The `exp_stats()` function creates experience summary data frames
  containing observed termination claims and rates. Optionally, expected
  termination rates, actual-to-expected ratios, and limited fluctuation
  credibility estimates can also be returned.
- The `autoplot()` and `autotable()` functions creates plots and tables
  for reporting.
- The `exp_shiny()` app launches a Shiny app that allows for interactive
  exploration of experience drivers.

## Installation

The actxps package can be installed from CRAN with:

``` r
install.packages("actxps")
```

To install the development version from [GitHub](https://github.com/)
use:

``` r
devtools::install_github("mattheaphy/actxps")
```

## Basic usage

An expanded version of this demo is available in the package’s [Get
started](https://mattheaphy.github.io/actxps/articles/actxps.html)
vignette.

The actxps package includes sample simulated census data for a
theoretical deferred annuity product with an optional guaranteed income
rider. The grain of this data is one row *per policy*.

``` r
library(actxps)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

census_dat
#> # A tibble: 20,000 × 10
#>    pol_num status    issue_date inc_guar qual    age product gender wd_age
#>      <int> <fct>     <date>     <lgl>    <lgl> <int> <fct>   <fct>   <int>
#>  1       1 Active    2014-12-17 TRUE     FALSE    56 b       F          77
#>  2       2 Surrender 2007-09-24 FALSE    FALSE    71 a       F          71
#>  3       3 Active    2012-10-06 FALSE    TRUE     62 b       F          63
#>  4       4 Surrender 2005-06-27 TRUE     TRUE     62 c       M          62
#>  5       5 Active    2019-11-22 FALSE    FALSE    62 c       F          67
#>  6       6 Active    2018-09-01 FALSE    TRUE     77 a       F          77
#>  7       7 Active    2011-07-23 TRUE     TRUE     63 a       M          65
#>  8       8 Surrender 2005-11-08 TRUE     TRUE     58 a       M          58
#>  9       9 Active    2010-09-19 FALSE    FALSE    53 c       M          64
#> 10      10 Active    2012-05-25 TRUE     FALSE    61 b       M          73
#> # … with 19,990 more rows, and 1 more variable: term_date <date>
```

Convert census records to exposure records with one row *per policy per
year*.

``` r
exposed_data <- expose(census_dat, end_date = "2019-12-31", 
                        target_status = "Surrender")

exposed_data
#> Exposure data
#> 
#>  Exposure type: policy_year 
#>  Target status: Surrender 
#>  Study range: 1900-01-01 to 2019-12-31 
#> 
#> # A tibble: 141,297 × 13
#>    pol_num status issue_date inc_guar qual    age product gender wd_age
#>  *   <int> <fct>  <date>     <lgl>    <lgl> <int> <fct>   <fct>   <int>
#>  1       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77
#>  2       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77
#>  3       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77
#>  4       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77
#>  5       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77
#>  6       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77
#>  7       2 Active 2007-09-24 FALSE    FALSE    71 a       F          71
#>  8       2 Active 2007-09-24 FALSE    FALSE    71 a       F          71
#>  9       2 Active 2007-09-24 FALSE    FALSE    71 a       F          71
#> 10       2 Active 2007-09-24 FALSE    FALSE    71 a       F          71
#> # … with 141,287 more rows, and 4 more variables: term_date <date>,
#> #   pol_yr <int>, pol_date_yr <date>, exposure <dbl>
```

Create a summary grouped by policy year and the presence of a guaranteed
income rider.

``` r

exp_res <- exposed_data |> 
  group_by(pol_yr, inc_guar) |> 
  exp_stats()

exp_res
#> Experience study results
#> 
#>  Groups: pol_yr, inc_guar 
#>  Target status: Surrender 
#>  Study range: 1900-01-01 to 2019-12-31 
#> 
#> # A tibble: 30 × 6
#>    pol_yr inc_guar n_claims claims exposure   q_obs
#>  *  <int> <lgl>       <int>  <int>    <dbl>   <dbl>
#>  1      1 FALSE          42     42    7719. 0.00544
#>  2      1 TRUE           38     38   11526. 0.00330
#>  3      2 FALSE          65     65    7117. 0.00913
#>  4      2 TRUE           61     61   10605. 0.00575
#>  5      3 FALSE          67     67    6476. 0.0103 
#>  6      3 TRUE           60     60    9626. 0.00623
#>  7      4 FALSE         103    103    5823. 0.0177 
#>  8      4 TRUE           58     58    8697. 0.00667
#>  9      5 FALSE          94     94    5147. 0.0183 
#> 10      5 TRUE           72     72    7779. 0.00926
#> # … with 20 more rows
```

Calculate actual-to-expected ratios.

First, attach one or more columns of expected termination rates to the
exposure data. Then, pass these column names to the `expected` argument
of `exp_stats`.

``` r

expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))

# using 2 different expected termination rates
exposed_data <- exposed_data |> 
  mutate(expected_1 = expected_table[pol_yr],
         expected_2 = ifelse(exposed_data$inc_guar, 0.015, 0.03))

exp_res <- exposed_data |> 
  group_by(pol_yr, inc_guar) |> 
  exp_stats(expected = c("expected_1", "expected_2"))

exp_res
#> Experience study results
#> 
#>  Groups: pol_yr, inc_guar 
#>  Target status: Surrender 
#>  Study range: 1900-01-01 to 2019-12-31 
#>  Expected values: expected_1, expected_2 
#> 
#> # A tibble: 30 × 10
#>    pol_yr inc_g…¹ n_cla…² claims expos…³   q_obs expec…⁴ expec…⁵ ae_ex…⁶ ae_ex…⁷
#>  *  <int> <lgl>     <int>  <int>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1      1 FALSE        42     42   7719. 0.00544 0.005     0.03    1.09    0.181
#>  2      1 TRUE         38     38  11526. 0.00330 0.005     0.015   0.659   0.220
#>  3      2 FALSE        65     65   7117. 0.00913 0.00778   0.03    1.17    0.304
#>  4      2 TRUE         61     61  10605. 0.00575 0.00778   0.015   0.740   0.383
#>  5      3 FALSE        67     67   6476. 0.0103  0.0106    0.03    0.980   0.345
#>  6      3 TRUE         60     60   9626. 0.00623 0.0106    0.015   0.591   0.416
#>  7      4 FALSE       103    103   5823. 0.0177  0.0133    0.03    1.33    0.590
#>  8      4 TRUE         58     58   8697. 0.00667 0.0133    0.015   0.500   0.445
#>  9      5 FALSE        94     94   5147. 0.0183  0.0161    0.03    1.13    0.609
#> 10      5 TRUE         72     72   7779. 0.00926 0.0161    0.015   0.574   0.617
#> # … with 20 more rows, and abbreviated variable names ¹​inc_guar, ²​n_claims,
#> #   ³​exposure, ⁴​expected_1, ⁵​expected_2, ⁶​ae_expected_1, ⁷​ae_expected_2
```

Create visualizations using the `autoplot()` and `autotable()`
functions.

``` r

library(ggplot2)

.colors <- c("#eb15e4", "#7515eb")
theme_set(theme_light())

exp_res |> 
  autoplot() + 
  scale_color_manual(values = .colors) + 
  labs(title = "Observed Surrender Rates by Policy Year and Income Guarantee Presence")
```

<img src="man/figures/README-plots-1.png" width="100%" />

``` r
autotable(exp_res)
```

<center>
<img src="man/figures/exp_gt.png" width="55%" height="55%" />
</center>

Launch a shiny app to interactively explore experience data.

``` r
exp_shiny(exposed_data)
```

<img src="man/figures/exp_shiny.png" width="100%" />
