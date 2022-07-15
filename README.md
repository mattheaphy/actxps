
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xps

<!-- badges: start -->
<!-- badges: end -->

Experience studies are used by actuaries to explore historical
experience across blocks of business and to inform assumption setting
activities. This package provides tidyverse-insprired functions for
preparing data, creating studies, and beginning assumption development.

## Installation

You can install the development version of xps from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mattheaphy/xps")
```

## Simulated data set

The `xps` package includes a data frame containing simulated census data
for a theoretical deferred annuity product with an optional guaranteed
income rider. The grain of this data is one row per policy.

``` r
library(xps)
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
#> # A tibble: 20,000 x 10
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
#> # ... with 19,990 more rows, and 1 more variable: term_date <date>
```

The data includes 3 policy statuses: Active, Death, and Surrender.

``` r
(status_counts <- table(census_dat$status))
#> 
#>    Active     Death Surrender 
#>     15195      1860      2945
```

Let’s assume we’re interested in calculating the probability of
surrender over one policy year. We cannot simply calculate the
proportion of policies in a surrendered status as this does not
represent an annualized surrender rate.

``` r
prop.table(status_counts)
#> 
#>    Active     Death Surrender 
#>   0.75975   0.09300   0.14725
```

## Creating exposed data

In order to calculate annual surrender rates, we need to break each
policy into multiple records. There should be one row per policy per
year.

The `expose_` family of functions is used to perform this
transformation.

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
#> # A tibble: 141,297 x 13
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
#> # ... with 141,287 more rows, and 4 more variables: term_date <date>,
#> #   pol_yr <int>, pol_date_yr <date>, exposure <dbl>
```

Now that the data has been “exposed” by policy year, the observed annual
surrender probability can be calculated as:

``` r
sum(exposed_data$status == "Surrender") / sum(exposed_data$exposure)
#> [1] 0.02145196
```

As a default, the `expose` function calculate exposures by policy year.
This can also be accomplished with the function `expose_py`. Other
implementations of `expose` include:

-   `expose_cy` = exposures by calendar year
-   `expose_cq` = exposures by calendar quarter
-   `expose_pm` = exposures by policy month
-   `expose_cm` = exposures by calendar month

## Experience study summary function

The `exp_stats` function creates a summary of observed experience data.
The output of this function is an `exp_df` object.

``` r
exp_stats(exposed_data)
#> Experience study results
#> 
#>  Groups:  
#>  Target status: Surrender 
#>  Study range: 1900-01-01 to 2019-12-31 
#> 
#> # A tibble: 1 x 3
#>   claims exposure  q_obs
#> *  <int>    <dbl>  <dbl>
#> 1   2846  132669. 0.0215
```

### Grouped experience data

If the data frame passed into `exp_stats` is grouped, the resulting
output will contain one record for each unique group.

``` r
library(dplyr)

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
#> # A tibble: 30 x 5
#>    pol_yr inc_guar claims exposure   q_obs
#>  *  <int> <lgl>     <int>    <dbl>   <dbl>
#>  1      1 FALSE        42    7719. 0.00544
#>  2      1 TRUE         38   11526. 0.00330
#>  3      2 FALSE        65    7117. 0.00913
#>  4      2 TRUE         61   10605. 0.00575
#>  5      3 FALSE        67    6476. 0.0103 
#>  6      3 TRUE         60    9626. 0.00623
#>  7      4 FALSE       103    5823. 0.0177 
#>  8      4 TRUE         58    8697. 0.00667
#>  9      5 FALSE        94    5147. 0.0183 
#> 10      5 TRUE         72    7779. 0.00926
#> # ... with 20 more rows
```

### Actual-to-expected rates

To derive actual-to-expected rates, first attach one or more columns of
expected termination rates to the exposure data. Then, pass these column
names to the `expected` argument of `exp_stats`.

``` r
expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))


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
#> # A tibble: 30 x 9
#>    pol_yr inc_guar claims exposure   q_obs expected_1 expected_2 ae_expected_1
#>  *  <int> <lgl>     <int>    <dbl>   <dbl>      <dbl>      <dbl>         <dbl>
#>  1      1 FALSE        42    7719. 0.00544    0.005        0.03          1.09 
#>  2      1 TRUE         38   11526. 0.00330    0.005        0.015         0.659
#>  3      2 FALSE        65    7117. 0.00913    0.00778      0.03          1.17 
#>  4      2 TRUE         61   10605. 0.00575    0.00778      0.015         0.740
#>  5      3 FALSE        67    6476. 0.0103     0.0106       0.03          0.980
#>  6      3 TRUE         60    9626. 0.00623    0.0106       0.015         0.591
#>  7      4 FALSE       103    5823. 0.0177     0.0133       0.03          1.33 
#>  8      4 TRUE         58    8697. 0.00667    0.0133       0.015         0.500
#>  9      5 FALSE        94    5147. 0.0183     0.0161       0.03          1.13 
#> 10      5 TRUE         72    7779. 0.00926    0.0161       0.015         0.574
#> # ... with 20 more rows, and 1 more variable: ae_expected_2 <dbl>
```

### `autoplot()` and `autotable()`

The `autoplot()` and `autotable()` functions can be used to create
pre-built visualizations and summary tables.

``` r
library(ggplot2)

.colors <- c("#eb15e4", "#7515eb")
theme_set(theme_light())

exp_res |> 
  autoplot() + 
  scale_color_manual(values = .colors) + 
  labs(title = "Observed Surrender Rates by Policy Year and Income Guarantee Presence")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
autotable(exp_res, groupname_col = "inc_guar")
```

<div id="udjubknfmn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#udjubknfmn .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 100%;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#udjubknfmn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#udjubknfmn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#udjubknfmn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#udjubknfmn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#udjubknfmn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#udjubknfmn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#udjubknfmn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#udjubknfmn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#udjubknfmn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#udjubknfmn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#udjubknfmn .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#udjubknfmn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#udjubknfmn .gt_from_md > :first-child {
  margin-top: 0;
}

#udjubknfmn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#udjubknfmn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#udjubknfmn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#udjubknfmn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#udjubknfmn .gt_row_group_first td {
  border-top-width: 2px;
}

#udjubknfmn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#udjubknfmn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#udjubknfmn .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#udjubknfmn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#udjubknfmn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#udjubknfmn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#udjubknfmn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#udjubknfmn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#udjubknfmn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#udjubknfmn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#udjubknfmn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#udjubknfmn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#udjubknfmn .gt_left {
  text-align: left;
}

#udjubknfmn .gt_center {
  text-align: center;
}

#udjubknfmn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#udjubknfmn .gt_font_normal {
  font-weight: normal;
}

#udjubknfmn .gt_font_bold {
  font-weight: bold;
}

#udjubknfmn .gt_font_italic {
  font-style: italic;
}

#udjubknfmn .gt_super {
  font-size: 65%;
}

#udjubknfmn .gt_two_val_uncert {
  display: inline-block;
  line-height: 1em;
  text-align: right;
  font-size: 60%;
  vertical-align: -0.25em;
  margin-left: 0.1em;
}

#udjubknfmn .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#udjubknfmn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#udjubknfmn .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#udjubknfmn .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#udjubknfmn .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="8" class="gt_heading gt_title gt_font_normal" style>Experience Study Results</th>
    </tr>
    <tr>
      <th colspan="8" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Target status: Surrender</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">pol_yr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">claims</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">exposure</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">q_obs</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;">
        <span class="gt_column_spanner">expected_1</span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;">
        <span class="gt_column_spanner">expected_2</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">E[<em>X</em>]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">A/E</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">E[<em>X</em>]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">A/E</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="8" class="gt_group_heading">FALSE</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_right">1</td>
<td class="gt_row gt_right">42</td>
<td class="gt_row gt_right">7,719</td>
<td class="gt_row gt_right" style="background-color: #F6FBEF; color: #000000;">0.5%</td>
<td class="gt_row gt_right">0.5%</td>
<td class="gt_row gt_right" style="background-color: #9CCAE1; color: #000000;">108.8%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #053061; color: #FFFFFF;">18.1%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">2</td>
<td class="gt_row gt_right gt_striped">65</td>
<td class="gt_row gt_right gt_striped">7,117</td>
<td class="gt_row gt_right gt_striped" style="background-color: #F3FBED; color: #000000;">0.9%</td>
<td class="gt_row gt_right gt_striped">0.8%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #B3D5E7; color: #000000;">117.4%</td>
<td class="gt_row gt_right gt_striped">3.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #08376A; color: #FFFFFF;">30.4%</td></tr>
    <tr><td class="gt_row gt_right">3</td>
<td class="gt_row gt_right">67</td>
<td class="gt_row gt_right">6,476</td>
<td class="gt_row gt_right" style="background-color: #F3FAEC; color: #000000;">1.0%</td>
<td class="gt_row gt_right">1.1%</td>
<td class="gt_row gt_right" style="background-color: #7EB6D6; color: #000000;">98.0%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #09396D; color: #FFFFFF;">34.5%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">4</td>
<td class="gt_row gt_right gt_striped">103</td>
<td class="gt_row gt_right gt_striped">5,823</td>
<td class="gt_row gt_right gt_striped" style="background-color: #EEF8E8; color: #000000;">1.8%</td>
<td class="gt_row gt_right gt_striped">1.3%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #D7E8F1; color: #000000;">132.7%</td>
<td class="gt_row gt_right gt_striped">3.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #114680; color: #FFFFFF;">59.0%</td></tr>
    <tr><td class="gt_row gt_right">5</td>
<td class="gt_row gt_right">94</td>
<td class="gt_row gt_right">5,147</td>
<td class="gt_row gt_right" style="background-color: #EEF8E7; color: #000000;">1.8%</td>
<td class="gt_row gt_right">1.6%</td>
<td class="gt_row gt_right" style="background-color: #A8D0E4; color: #000000;">113.3%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #114781; color: #FFFFFF;">60.9%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">6</td>
<td class="gt_row gt_right gt_striped">98</td>
<td class="gt_row gt_right gt_striped">4,530</td>
<td class="gt_row gt_right gt_striped" style="background-color: #EBF7E5; color: #000000;">2.2%</td>
<td class="gt_row gt_right gt_striped">1.9%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #ACD2E5; color: #000000;">114.5%</td>
<td class="gt_row gt_right gt_striped">3.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #154D8A; color: #FFFFFF;">72.1%</td></tr>
    <tr><td class="gt_row gt_right">7</td>
<td class="gt_row gt_right">101</td>
<td class="gt_row gt_right">3,930</td>
<td class="gt_row gt_right" style="background-color: #E9F6E3; color: #000000;">2.6%</td>
<td class="gt_row gt_right">2.2%</td>
<td class="gt_row gt_right" style="background-color: #B6D7E8; color: #000000;">118.6%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #195595; color: #FFFFFF;">85.7%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">8</td>
<td class="gt_row gt_right gt_striped">72</td>
<td class="gt_row gt_right gt_striped">3,308</td>
<td class="gt_row gt_right gt_striped" style="background-color: #EBF7E5; color: #000000;">2.2%</td>
<td class="gt_row gt_right gt_striped">2.4%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #61A4CC; color: #000000;">89.0%</td>
<td class="gt_row gt_right gt_striped">3.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #154E8A; color: #FFFFFF;">72.6%</td></tr>
    <tr><td class="gt_row gt_right">9</td>
<td class="gt_row gt_right">88</td>
<td class="gt_row gt_right">2,726</td>
<td class="gt_row gt_right" style="background-color: #E5F5DF; color: #000000;">3.2%</td>
<td class="gt_row gt_right">2.7%</td>
<td class="gt_row gt_right" style="background-color: #B6D7E8; color: #000000;">118.6%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #1F62A7; color: #FFFFFF;">107.6%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">10</td>
<td class="gt_row gt_right gt_striped">84</td>
<td class="gt_row gt_right gt_striped">2,195</td>
<td class="gt_row gt_right gt_striped" style="background-color: #E1F3DC; color: #000000;">3.8%</td>
<td class="gt_row gt_right gt_striped">3.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #CDE3EF; color: #000000;">127.5%</td>
<td class="gt_row gt_right gt_striped">3.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #276CAF; color: #FFFFFF;">127.5%</td></tr>
    <tr><td class="gt_row gt_right">11</td>
<td class="gt_row gt_right">503</td>
<td class="gt_row gt_right">1,711</td>
<td class="gt_row gt_right" style="background-color: #084081; color: #FFFFFF;">29.4%</td>
<td class="gt_row gt_right">20.0%</td>
<td class="gt_row gt_right" style="background-color: #EDF2F5; color: #000000;">147.0%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #67001F; color: #FFFFFF;">980.1%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">12</td>
<td class="gt_row gt_right gt_striped">193</td>
<td class="gt_row gt_right gt_striped">876</td>
<td class="gt_row gt_right gt_striped" style="background-color: #2C8DBF; color: #FFFFFF;">22.0%</td>
<td class="gt_row gt_right gt_striped">15.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #EDF2F5; color: #000000;">146.9%</td>
<td class="gt_row gt_right gt_striped">3.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #E7876A; color: #000000;">734.6%</td></tr>
    <tr><td class="gt_row gt_right">13</td>
<td class="gt_row gt_right">64</td>
<td class="gt_row gt_right">470</td>
<td class="gt_row gt_right" style="background-color: #8CD2BF; color: #000000;">13.6%</td>
<td class="gt_row gt_right">5.0%</td>
<td class="gt_row gt_right" style="background-color: #6C0120; color: #FFFFFF;">272.3%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #E5EFF4; color: #000000;">453.9%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">14</td>
<td class="gt_row gt_right gt_striped">33</td>
<td class="gt_row gt_right gt_striped">241</td>
<td class="gt_row gt_right gt_striped" style="background-color: #8BD1BF; color: #000000;">13.7%</td>
<td class="gt_row gt_right gt_striped">5.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #67001F; color: #FFFFFF;">274.2%</td>
<td class="gt_row gt_right gt_striped">3.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #E7EFF4; color: #000000;">457.0%</td></tr>
    <tr><td class="gt_row gt_right">15</td>
<td class="gt_row gt_right">8</td>
<td class="gt_row gt_right">68</td>
<td class="gt_row gt_right" style="background-color: #A3DBB7; color: #000000;">11.7%</td>
<td class="gt_row gt_right">5.0%</td>
<td class="gt_row gt_right" style="background-color: #CA4B41; color: #FFFFFF;">234.0%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #C9E1EE; color: #000000;">390.0%</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="8" class="gt_group_heading">TRUE</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_right gt_striped">1</td>
<td class="gt_row gt_right gt_striped">38</td>
<td class="gt_row gt_right gt_striped">11,526</td>
<td class="gt_row gt_right gt_striped" style="background-color: #F7FCF0; color: #000000;">0.3%</td>
<td class="gt_row gt_right gt_striped">0.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #3077B5; color: #FFFFFF;">65.9%</td>
<td class="gt_row gt_right gt_striped">1.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #063264; color: #FFFFFF;">22.0%</td></tr>
    <tr><td class="gt_row gt_right">2</td>
<td class="gt_row gt_right">61</td>
<td class="gt_row gt_right">10,605</td>
<td class="gt_row gt_right" style="background-color: #F5FBEF; color: #000000;">0.6%</td>
<td class="gt_row gt_right">0.8%</td>
<td class="gt_row gt_right" style="background-color: #3B86BC; color: #FFFFFF;">74.0%</td>
<td class="gt_row gt_right">1.5%</td>
<td class="gt_row gt_right" style="background-color: #0B3B70; color: #FFFFFF;">38.3%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">3</td>
<td class="gt_row gt_right gt_striped">60</td>
<td class="gt_row gt_right gt_striped">9,626</td>
<td class="gt_row gt_right gt_striped" style="background-color: #F5FBEE; color: #000000;">0.6%</td>
<td class="gt_row gt_right gt_striped">1.1%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #256AAE; color: #FFFFFF;">59.1%</td>
<td class="gt_row gt_right gt_striped">1.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #0C3D73; color: #FFFFFF;">41.6%</td></tr>
    <tr><td class="gt_row gt_right">4</td>
<td class="gt_row gt_right">58</td>
<td class="gt_row gt_right">8,697</td>
<td class="gt_row gt_right" style="background-color: #F5FBEE; color: #000000;">0.7%</td>
<td class="gt_row gt_right">1.3%</td>
<td class="gt_row gt_right" style="background-color: #195797; color: #FFFFFF;">50.0%</td>
<td class="gt_row gt_right">1.5%</td>
<td class="gt_row gt_right" style="background-color: #0D3E75; color: #FFFFFF;">44.5%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">5</td>
<td class="gt_row gt_right gt_striped">72</td>
<td class="gt_row gt_right gt_striped">7,779</td>
<td class="gt_row gt_right gt_striped" style="background-color: #F3FBED; color: #000000;">0.9%</td>
<td class="gt_row gt_right gt_striped">1.6%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #2367AD; color: #FFFFFF;">57.4%</td>
<td class="gt_row gt_right gt_striped">1.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #124882; color: #FFFFFF;">61.7%</td></tr>
    <tr><td class="gt_row gt_right">6</td>
<td class="gt_row gt_right">65</td>
<td class="gt_row gt_right">6,854</td>
<td class="gt_row gt_right" style="background-color: #F3FAEC; color: #000000;">0.9%</td>
<td class="gt_row gt_right">1.9%</td>
<td class="gt_row gt_right" style="background-color: #1A5797; color: #FFFFFF;">50.2%</td>
<td class="gt_row gt_right">1.5%</td>
<td class="gt_row gt_right" style="background-color: #124883; color: #FFFFFF;">63.2%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">7</td>
<td class="gt_row gt_right gt_striped">54</td>
<td class="gt_row gt_right gt_striped">5,982</td>
<td class="gt_row gt_right gt_striped" style="background-color: #F3FBED; color: #000000;">0.9%</td>
<td class="gt_row gt_right gt_striped">2.2%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #10447D; color: #FFFFFF;">41.7%</td>
<td class="gt_row gt_right gt_striped">1.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #114781; color: #FFFFFF;">60.2%</td></tr>
    <tr><td class="gt_row gt_right">8</td>
<td class="gt_row gt_right">75</td>
<td class="gt_row gt_right">5,146</td>
<td class="gt_row gt_right" style="background-color: #F0F9E9; color: #000000;">1.5%</td>
<td class="gt_row gt_right">2.4%</td>
<td class="gt_row gt_right" style="background-color: #266BAF; color: #FFFFFF;">59.6%</td>
<td class="gt_row gt_right">1.5%</td>
<td class="gt_row gt_right" style="background-color: #1C5C9E; color: #FFFFFF;">97.2%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">9</td>
<td class="gt_row gt_right gt_striped">83</td>
<td class="gt_row gt_right gt_striped">4,289</td>
<td class="gt_row gt_right gt_striped" style="background-color: #EDF8E7; color: #000000;">1.9%</td>
<td class="gt_row gt_right gt_striped">2.7%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #3781BA; color: #FFFFFF;">71.1%</td>
<td class="gt_row gt_right gt_striped">1.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #286DB0; color: #FFFFFF;">129.0%</td></tr>
    <tr><td class="gt_row gt_right">10</td>
<td class="gt_row gt_right">88</td>
<td class="gt_row gt_right">3,468</td>
<td class="gt_row gt_right" style="background-color: #E9F7E3; color: #000000;">2.5%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #529BC7; color: #000000;">84.6%</td>
<td class="gt_row gt_right">1.5%</td>
<td class="gt_row gt_right" style="background-color: #367FB9; color: #FFFFFF;">169.1%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">11</td>
<td class="gt_row gt_right gt_striped">350</td>
<td class="gt_row gt_right gt_striped">2,713</td>
<td class="gt_row gt_right gt_striped" style="background-color: #95D5BC; color: #000000;">12.9%</td>
<td class="gt_row gt_right gt_striped">20.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #2E74B3; color: #FFFFFF;">64.5%</td>
<td class="gt_row gt_right gt_striped">1.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #BB2F33; color: #FFFFFF;">860.1%</td></tr>
    <tr><td class="gt_row gt_right">12</td>
<td class="gt_row gt_right">163</td>
<td class="gt_row gt_right">1,757</td>
<td class="gt_row gt_right" style="background-color: #BBE5BE; color: #000000;">9.3%</td>
<td class="gt_row gt_right">15.0%</td>
<td class="gt_row gt_right" style="background-color: #2A6FB1; color: #FFFFFF;">61.9%</td>
<td class="gt_row gt_right">1.5%</td>
<td class="gt_row gt_right" style="background-color: #FCCEB6; color: #000000;">618.6%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">13</td>
<td class="gt_row gt_right gt_striped">38</td>
<td class="gt_row gt_right gt_striped">1,109</td>
<td class="gt_row gt_right gt_striped" style="background-color: #E3F4DE; color: #000000;">3.4%</td>
<td class="gt_row gt_right gt_striped">5.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #347CB7; color: #FFFFFF;">68.5%</td>
<td class="gt_row gt_right gt_striped">1.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #549CC8; color: #000000;">228.4%</td></tr>
    <tr><td class="gt_row gt_right">14</td>
<td class="gt_row gt_right">23</td>
<td class="gt_row gt_right">596</td>
<td class="gt_row gt_right" style="background-color: #E1F3DC; color: #000000;">3.9%</td>
<td class="gt_row gt_right">5.0%</td>
<td class="gt_row gt_right" style="background-color: #3F8CC0; color: #000000;">77.2%</td>
<td class="gt_row gt_right">1.5%</td>
<td class="gt_row gt_right" style="background-color: #6DABD0; color: #000000;">257.4%</td></tr>
    <tr><td class="gt_row gt_right gt_striped">15</td>
<td class="gt_row gt_right gt_striped">3</td>
<td class="gt_row gt_right gt_striped">185</td>
<td class="gt_row gt_right gt_striped" style="background-color: #EFF9E8; color: #000000;">1.6%</td>
<td class="gt_row gt_right gt_striped">5.0%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #053061; color: #FFFFFF;">32.5%</td>
<td class="gt_row gt_right gt_striped">1.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #1F62A7; color: #FFFFFF;">108.2%</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="8">Study range: 1900-01-01 to 2019-12-31</td>
    </tr>
  </tfoot>
  
</table>
</div>

### `summary()`

Calling the `summary` function on an `exp_df` object re-summarizes
experience results. This also produces an `exp_df` object.

``` r
summary(exp_res) |> autotable()
```

<div id="eplxgcioqw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#eplxgcioqw .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 100%;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#eplxgcioqw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eplxgcioqw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#eplxgcioqw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#eplxgcioqw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eplxgcioqw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eplxgcioqw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#eplxgcioqw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#eplxgcioqw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#eplxgcioqw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#eplxgcioqw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#eplxgcioqw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#eplxgcioqw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#eplxgcioqw .gt_from_md > :first-child {
  margin-top: 0;
}

#eplxgcioqw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#eplxgcioqw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#eplxgcioqw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#eplxgcioqw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#eplxgcioqw .gt_row_group_first td {
  border-top-width: 2px;
}

#eplxgcioqw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eplxgcioqw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#eplxgcioqw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#eplxgcioqw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eplxgcioqw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eplxgcioqw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#eplxgcioqw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#eplxgcioqw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eplxgcioqw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eplxgcioqw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eplxgcioqw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eplxgcioqw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eplxgcioqw .gt_left {
  text-align: left;
}

#eplxgcioqw .gt_center {
  text-align: center;
}

#eplxgcioqw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#eplxgcioqw .gt_font_normal {
  font-weight: normal;
}

#eplxgcioqw .gt_font_bold {
  font-weight: bold;
}

#eplxgcioqw .gt_font_italic {
  font-style: italic;
}

#eplxgcioqw .gt_super {
  font-size: 65%;
}

#eplxgcioqw .gt_two_val_uncert {
  display: inline-block;
  line-height: 1em;
  text-align: right;
  font-size: 60%;
  vertical-align: -0.25em;
  margin-left: 0.1em;
}

#eplxgcioqw .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#eplxgcioqw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#eplxgcioqw .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#eplxgcioqw .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#eplxgcioqw .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="7" class="gt_heading gt_title gt_font_normal" style>Experience Study Results</th>
    </tr>
    <tr>
      <th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Target status: Surrender</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">claims</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">exposure</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">q_obs</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;">
        <span class="gt_column_spanner">expected_1</span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;">
        <span class="gt_column_spanner">expected_2</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">E[<em>X</em>]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">A/E</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">E[<em>X</em>]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">A/E</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_right">2,846</td>
<td class="gt_row gt_right">132,669</td>
<td class="gt_row gt_right" style="background-color: #7BCCC4; color: #000000;">2.1%</td>
<td class="gt_row gt_right">2.4%</td>
<td class="gt_row gt_right" style="background-color: #F7F7F7; color: #000000;">88.5%</td>
<td class="gt_row gt_right">2.1%</td>
<td class="gt_row gt_right" style="background-color: #F7F7F7; color: #000000;">102.6%</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="7">Study range: 1900-01-01 to 2019-12-31</td>
    </tr>
  </tfoot>
  
</table>
</div>

If additional variables are passed to `...`, these variables become
groups in the re-summarized `exp_df` object.

``` r
summary(exp_res, inc_guar) |> autotable()
```

<div id="mtuhwwdklb" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#mtuhwwdklb .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 100%;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#mtuhwwdklb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mtuhwwdklb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#mtuhwwdklb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#mtuhwwdklb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtuhwwdklb .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mtuhwwdklb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#mtuhwwdklb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#mtuhwwdklb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mtuhwwdklb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mtuhwwdklb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#mtuhwwdklb .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#mtuhwwdklb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#mtuhwwdklb .gt_from_md > :first-child {
  margin-top: 0;
}

#mtuhwwdklb .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mtuhwwdklb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#mtuhwwdklb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#mtuhwwdklb .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#mtuhwwdklb .gt_row_group_first td {
  border-top-width: 2px;
}

#mtuhwwdklb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtuhwwdklb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mtuhwwdklb .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mtuhwwdklb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtuhwwdklb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtuhwwdklb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mtuhwwdklb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mtuhwwdklb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtuhwwdklb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mtuhwwdklb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtuhwwdklb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mtuhwwdklb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtuhwwdklb .gt_left {
  text-align: left;
}

#mtuhwwdklb .gt_center {
  text-align: center;
}

#mtuhwwdklb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mtuhwwdklb .gt_font_normal {
  font-weight: normal;
}

#mtuhwwdklb .gt_font_bold {
  font-weight: bold;
}

#mtuhwwdklb .gt_font_italic {
  font-style: italic;
}

#mtuhwwdklb .gt_super {
  font-size: 65%;
}

#mtuhwwdklb .gt_two_val_uncert {
  display: inline-block;
  line-height: 1em;
  text-align: right;
  font-size: 60%;
  vertical-align: -0.25em;
  margin-left: 0.1em;
}

#mtuhwwdklb .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#mtuhwwdklb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mtuhwwdklb .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#mtuhwwdklb .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#mtuhwwdklb .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="8" class="gt_heading gt_title gt_font_normal" style>Experience Study Results</th>
    </tr>
    <tr>
      <th colspan="8" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Target status: Surrender</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" style="font-weight: bold;">inc_guar</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">claims</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">exposure</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;">q_obs</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;">
        <span class="gt_column_spanner">expected_1</span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;">
        <span class="gt_column_spanner">expected_2</span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">E[<em>X</em>]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">A/E</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">E[<em>X</em>]</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;">A/E</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_center">FALSE</td>
<td class="gt_row gt_right">1,615</td>
<td class="gt_row gt_right">52,338</td>
<td class="gt_row gt_right" style="background-color: #084081; color: #FFFFFF;">3.1%</td>
<td class="gt_row gt_right">2.3%</td>
<td class="gt_row gt_right" style="background-color: #67001F; color: #FFFFFF;">131.6%</td>
<td class="gt_row gt_right">3.0%</td>
<td class="gt_row gt_right" style="background-color: #67001F; color: #FFFFFF;">102.9%</td></tr>
    <tr><td class="gt_row gt_center gt_striped">TRUE</td>
<td class="gt_row gt_right gt_striped">1,231</td>
<td class="gt_row gt_right gt_striped">80,331</td>
<td class="gt_row gt_right gt_striped" style="background-color: #F7FCF0; color: #000000;">1.5%</td>
<td class="gt_row gt_right gt_striped">2.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #053061; color: #FFFFFF;">61.9%</td>
<td class="gt_row gt_right gt_striped">1.5%</td>
<td class="gt_row gt_right gt_striped" style="background-color: #053061; color: #FFFFFF;">102.2%</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="8">Study range: 1900-01-01 to 2019-12-31</td>
    </tr>
  </tfoot>
  
</table>
</div>
