
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
  containing observed claims, observed claim rates. Optionally, expected
  claim rates, actual-to-expected ratios, and limited fluctuation
  credibility estimates can also be returned.
- The `autoplot()` and `autotable()` creates plots and tables for
  reporting.
- The `exp_shiny()` app launches a Shiny app that allows for interactive
  exploration of experience drivers.

## Installation

The actxps package can be installed from CRAN with:

``` r
install.packages("devtools")
```

To install the development version from [GitHub](https://github.com/)
use:

``` r
devtools::install_github("mattheaphy/actxps")
```

## Basic usage

The `actxps` package includes a data frame containing simulated census
data for a theoretical deferred annuity product with an optional
guaranteed income rider. The grain of this data is one row per policy.

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

Convert census records to exposure records with one row per policy per
year.

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

Creates a summary of observed experience.

``` r
exp_stats(exposed_data)
#> Experience study results
#> 
#>  Groups:  
#>  Target status: Surrender 
#>  Study range: 1900-01-01 to 2019-12-31 
#> 
#> # A tibble: 1 × 4
#>   n_claims claims exposure  q_obs
#> *    <int>  <int>    <dbl>  <dbl>
#> 1     2846   2846  132669. 0.0215
```

Using `dplyr::group_by`, create a summary grouped by policy year and the
presence of a guaranteed income rider.

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

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r
autotable(exp_res)
```

<div id="umzfexkcdt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#umzfexkcdt .gt_table {
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

#umzfexkcdt .gt_heading {
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

#umzfexkcdt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#umzfexkcdt .gt_title {
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

#umzfexkcdt .gt_subtitle {
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

#umzfexkcdt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umzfexkcdt .gt_col_headings {
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

#umzfexkcdt .gt_col_heading {
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

#umzfexkcdt .gt_column_spanner_outer {
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

#umzfexkcdt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#umzfexkcdt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#umzfexkcdt .gt_column_spanner {
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

#umzfexkcdt .gt_group_heading {
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
  text-align: left;
}

#umzfexkcdt .gt_empty_group_heading {
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

#umzfexkcdt .gt_from_md > :first-child {
  margin-top: 0;
}

#umzfexkcdt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#umzfexkcdt .gt_row {
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

#umzfexkcdt .gt_stub {
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

#umzfexkcdt .gt_stub_row_group {
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

#umzfexkcdt .gt_row_group_first td {
  border-top-width: 2px;
}

#umzfexkcdt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umzfexkcdt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#umzfexkcdt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#umzfexkcdt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umzfexkcdt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umzfexkcdt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#umzfexkcdt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#umzfexkcdt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umzfexkcdt .gt_footnotes {
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

#umzfexkcdt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#umzfexkcdt .gt_sourcenotes {
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

#umzfexkcdt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#umzfexkcdt .gt_left {
  text-align: left;
}

#umzfexkcdt .gt_center {
  text-align: center;
}

#umzfexkcdt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#umzfexkcdt .gt_font_normal {
  font-weight: normal;
}

#umzfexkcdt .gt_font_bold {
  font-weight: bold;
}

#umzfexkcdt .gt_font_italic {
  font-style: italic;
}

#umzfexkcdt .gt_super {
  font-size: 65%;
}

#umzfexkcdt .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#umzfexkcdt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#umzfexkcdt .gt_indent_1 {
  text-indent: 5px;
}

#umzfexkcdt .gt_indent_2 {
  text-indent: 10px;
}

#umzfexkcdt .gt_indent_3 {
  text-indent: 15px;
}

#umzfexkcdt .gt_indent_4 {
  text-indent: 20px;
}

#umzfexkcdt .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <td colspan="9" class="gt_heading gt_title gt_font_normal" style>Experience Study Results</td>
    </tr>
    <tr>
      <td colspan="9" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Target status: Surrender</td>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;" scope="col" id="pol_yr">pol_yr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" style="font-weight: bold;" scope="col" id="inc_guar">inc_guar</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;" scope="col" id="Claims">Claims</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;" scope="col" id="Exposures">Exposures</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" style="font-weight: bold;" scope="col" id="&lt;em&gt;q&lt;sup&gt;obs&lt;/sup&gt;&lt;/em&gt;"><em>q<sup>obs</sup></em></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;" scope="colgroup" id="&lt;code&gt;expected_1&lt;/code&gt;">
        <span class="gt_column_spanner"><code>expected_1</code></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" style="font-weight: bold;" scope="colgroup" id="&lt;code&gt;expected_2&lt;/code&gt;">
        <span class="gt_column_spanner"><code>expected_2</code></span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="&lt;em&gt;q&lt;sup&gt;exp&lt;/sup&gt;&lt;/em&gt;"><em>q<sup>exp</sup></em></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="&lt;em&gt;A/E&lt;/em&gt;"><em>A/E</em></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="&lt;em&gt;q&lt;sup&gt;exp&lt;/sup&gt;&lt;/em&gt;"><em>q<sup>exp</sup></em></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="font-weight: bold;" scope="col" id="&lt;em&gt;A/E&lt;/em&gt;"><em>A/E</em></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="pol_yr" class="gt_row gt_right">1</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">42</td>
<td headers="exposure" class="gt_row gt_right">7,719</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #F6FBEF; color: #000000;">0.5%</td>
<td headers="expected_1" class="gt_row gt_right">0.5%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #9CCAE1; color: #000000;">108.8%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #053061; color: #FFFFFF;">18.1%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">1</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">38</td>
<td headers="exposure" class="gt_row gt_right gt_striped">11,526</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #F7FCF0; color: #000000;">0.3%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">0.5%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #3077B5; color: #FFFFFF;">65.9%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #063264; color: #FFFFFF;">22.0%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">2</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">65</td>
<td headers="exposure" class="gt_row gt_right">7,117</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #F3FBED; color: #000000;">0.9%</td>
<td headers="expected_1" class="gt_row gt_right">0.8%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #B3D5E7; color: #000000;">117.4%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #08376A; color: #FFFFFF;">30.4%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">2</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">61</td>
<td headers="exposure" class="gt_row gt_right gt_striped">10,605</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #F5FBEF; color: #000000;">0.6%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">0.8%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #3B86BC; color: #FFFFFF;">74.0%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #0B3B70; color: #FFFFFF;">38.3%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">3</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">67</td>
<td headers="exposure" class="gt_row gt_right">6,476</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #F3FAEC; color: #000000;">1.0%</td>
<td headers="expected_1" class="gt_row gt_right">1.1%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #7EB6D6; color: #000000;">98.0%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #09396D; color: #FFFFFF;">34.5%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">3</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">60</td>
<td headers="exposure" class="gt_row gt_right gt_striped">9,626</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #F5FBEE; color: #000000;">0.6%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">1.1%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #256AAE; color: #FFFFFF;">59.1%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #0C3D73; color: #FFFFFF;">41.6%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">4</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">103</td>
<td headers="exposure" class="gt_row gt_right">5,823</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #EEF8E8; color: #000000;">1.8%</td>
<td headers="expected_1" class="gt_row gt_right">1.3%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #D7E8F1; color: #000000;">132.7%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #114680; color: #FFFFFF;">59.0%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">4</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">58</td>
<td headers="exposure" class="gt_row gt_right gt_striped">8,697</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #F5FBEE; color: #000000;">0.7%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">1.3%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #195797; color: #FFFFFF;">50.0%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #0D3E75; color: #FFFFFF;">44.5%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">5</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">94</td>
<td headers="exposure" class="gt_row gt_right">5,147</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #EEF8E7; color: #000000;">1.8%</td>
<td headers="expected_1" class="gt_row gt_right">1.6%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #A8D0E4; color: #000000;">113.3%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #114781; color: #FFFFFF;">60.9%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">5</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">72</td>
<td headers="exposure" class="gt_row gt_right gt_striped">7,779</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #F3FBED; color: #000000;">0.9%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">1.6%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #2367AD; color: #FFFFFF;">57.4%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #124882; color: #FFFFFF;">61.7%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">6</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">98</td>
<td headers="exposure" class="gt_row gt_right">4,530</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #EBF7E5; color: #000000;">2.2%</td>
<td headers="expected_1" class="gt_row gt_right">1.9%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #ACD2E5; color: #000000;">114.5%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #154D8A; color: #FFFFFF;">72.1%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">6</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">65</td>
<td headers="exposure" class="gt_row gt_right gt_striped">6,854</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #F3FAEC; color: #000000;">0.9%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">1.9%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #1A5797; color: #FFFFFF;">50.2%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #124883; color: #FFFFFF;">63.2%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">7</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">101</td>
<td headers="exposure" class="gt_row gt_right">3,930</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #E9F6E3; color: #000000;">2.6%</td>
<td headers="expected_1" class="gt_row gt_right">2.2%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #B6D7E8; color: #000000;">118.6%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #195595; color: #FFFFFF;">85.7%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">7</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">54</td>
<td headers="exposure" class="gt_row gt_right gt_striped">5,982</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #F3FBED; color: #000000;">0.9%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">2.2%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #10447D; color: #FFFFFF;">41.7%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #114781; color: #FFFFFF;">60.2%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">8</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">72</td>
<td headers="exposure" class="gt_row gt_right">3,308</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #EBF7E5; color: #000000;">2.2%</td>
<td headers="expected_1" class="gt_row gt_right">2.4%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #61A4CC; color: #FFFFFF;">89.0%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #154E8A; color: #FFFFFF;">72.6%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">8</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">75</td>
<td headers="exposure" class="gt_row gt_right gt_striped">5,146</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #F0F9E9; color: #000000;">1.5%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">2.4%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #266BAF; color: #FFFFFF;">59.6%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #1C5C9E; color: #FFFFFF;">97.2%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">9</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">88</td>
<td headers="exposure" class="gt_row gt_right">2,726</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #E5F5DF; color: #000000;">3.2%</td>
<td headers="expected_1" class="gt_row gt_right">2.7%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #B6D7E8; color: #000000;">118.6%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #1F62A7; color: #FFFFFF;">107.6%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">9</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">83</td>
<td headers="exposure" class="gt_row gt_right gt_striped">4,289</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #EDF8E7; color: #000000;">1.9%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">2.7%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #3781BA; color: #FFFFFF;">71.1%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #286DB0; color: #FFFFFF;">129.0%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">10</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">84</td>
<td headers="exposure" class="gt_row gt_right">2,195</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #E1F3DC; color: #000000;">3.8%</td>
<td headers="expected_1" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #CDE3EF; color: #000000;">127.5%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #276CAF; color: #FFFFFF;">127.5%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">10</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">88</td>
<td headers="exposure" class="gt_row gt_right gt_striped">3,468</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #E9F7E3; color: #000000;">2.5%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">3.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #529BC7; color: #FFFFFF;">84.6%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #367FB9; color: #FFFFFF;">169.1%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">11</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">503</td>
<td headers="exposure" class="gt_row gt_right">1,711</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #084081; color: #FFFFFF;">29.4%</td>
<td headers="expected_1" class="gt_row gt_right">20.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #EDF2F5; color: #000000;">147.0%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #67001F; color: #FFFFFF;">980.1%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">11</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">350</td>
<td headers="exposure" class="gt_row gt_right gt_striped">2,713</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #95D5BC; color: #000000;">12.9%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">20.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #2E74B3; color: #FFFFFF;">64.5%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #BB2F33; color: #FFFFFF;">860.1%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">12</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">193</td>
<td headers="exposure" class="gt_row gt_right">876</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #2C8DBF; color: #FFFFFF;">22.0%</td>
<td headers="expected_1" class="gt_row gt_right">15.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #EDF2F5; color: #000000;">146.9%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #E7876A; color: #FFFFFF;">734.6%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">12</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">163</td>
<td headers="exposure" class="gt_row gt_right gt_striped">1,757</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #BBE5BE; color: #000000;">9.3%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">15.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #2A6FB1; color: #FFFFFF;">61.9%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #FCCEB6; color: #000000;">618.6%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">13</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">64</td>
<td headers="exposure" class="gt_row gt_right">470</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #8CD2BF; color: #000000;">13.6%</td>
<td headers="expected_1" class="gt_row gt_right">5.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #6C0120; color: #FFFFFF;">272.3%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #E5EFF4; color: #000000;">453.9%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">13</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">38</td>
<td headers="exposure" class="gt_row gt_right gt_striped">1,109</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #E3F4DE; color: #000000;">3.4%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">5.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #347CB7; color: #FFFFFF;">68.5%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #549CC8; color: #FFFFFF;">228.4%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">14</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">33</td>
<td headers="exposure" class="gt_row gt_right">241</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #8BD1BF; color: #000000;">13.7%</td>
<td headers="expected_1" class="gt_row gt_right">5.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #67001F; color: #FFFFFF;">274.2%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #E7EFF4; color: #000000;">457.0%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">14</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">23</td>
<td headers="exposure" class="gt_row gt_right gt_striped">596</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #E1F3DC; color: #000000;">3.9%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">5.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #3F8CC0; color: #FFFFFF;">77.2%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #6DABD0; color: #000000;">257.4%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right">15</td>
<td headers="inc_guar" class="gt_row gt_center">FALSE</td>
<td headers="claims" class="gt_row gt_right">8</td>
<td headers="exposure" class="gt_row gt_right">68</td>
<td headers="q_obs" class="gt_row gt_right" style="background-color: #A3DBB7; color: #000000;">11.7%</td>
<td headers="expected_1" class="gt_row gt_right">5.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right" style="background-color: #CA4B41; color: #FFFFFF;">234.0%</td>
<td headers="expected_2" class="gt_row gt_right">3.0%</td>
<td headers="ae_expected_2" class="gt_row gt_right" style="background-color: #C9E1EE; color: #000000;">390.0%</td></tr>
    <tr><td headers="pol_yr" class="gt_row gt_right gt_striped">15</td>
<td headers="inc_guar" class="gt_row gt_center gt_striped">TRUE</td>
<td headers="claims" class="gt_row gt_right gt_striped">3</td>
<td headers="exposure" class="gt_row gt_right gt_striped">185</td>
<td headers="q_obs" class="gt_row gt_right gt_striped" style="background-color: #EFF9E8; color: #000000;">1.6%</td>
<td headers="expected_1" class="gt_row gt_right gt_striped">5.0%</td>
<td headers="ae_expected_1" class="gt_row gt_right gt_striped" style="background-color: #053061; color: #FFFFFF;">32.5%</td>
<td headers="expected_2" class="gt_row gt_right gt_striped">1.5%</td>
<td headers="ae_expected_2" class="gt_row gt_right gt_striped" style="background-color: #1F62A7; color: #FFFFFF;">108.2%</td></tr>
  </tbody>
  <tfoot class="gt_sourcenotes">
    <tr>
      <td class="gt_sourcenote" colspan="9">Study range: 1900-01-01 to 2019-12-31</td>
    </tr>
  </tfoot>
  
</table>
</div>

Launches a shiny app to interactively explore experience data.

``` r
exp_shiny(exposed_data)
```

<img src="man/figures/exp_shiny.png" width="100%" />
