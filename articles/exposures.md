# Exposure calculations

**Census-level data** refers to a data set wherein there is one row per
policy. **Exposure-level data** expands census-level data such that
there is one record per policy per observation period. Observation
periods could be any meaningful period of time such as a policy year,
policy month, calendar year, calendar quarter, calendar month, etc.

A common step in experience studies is converting census-level data into
exposure-level data. The
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
family of functions assists with this task. Specifically, the
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
family:

- Expands a census-level data frame to an exposure-level data frame
- Calculates partial exposures for left-censored and right-censored
  records using exact day counts
- For terminated policies, sets the policy status to an active state for
  all observation periods except the last. Similarly, the termination
  date is set to `NA` for all periods except the last.
- Adds identifier columns for observation periods
- Converts the data to the `exposed_df` class, which is a format
  expected by several actxps functions.

If you already have exposure-level data available, the
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
functions are not necessary. However, we recommend converting your data
to the `exposed_df` format using the function
[`as_exposed_df()`](https://mattheaphy.github.io/actxps/reference/is_exposed_df.md).

## Toy census data

To get started, we’re going to use a toy census data frame from the
actxps package that contains 3 policies: one active, one that terminated
due to death, and one that terminated due to surrender.

`toy_census` contains the 4 columns necessary to compute exposures:

- `pol_num`: a unique identifier for individual policies
- `status`: the policy status
- `issue_date`: issue date
- `term_date`: termination date, if any. Otherwise `NA`

``` r

library(actxps)
library(dplyr)

toy_census
#>   pol_num    status issue_date  term_date
#> 1       1    Active 2010-01-01       <NA>
#> 2       2     Death 2011-05-27 2020-09-14
#> 3       3 Surrender 2009-11-10 2022-02-25
```

## Policy year exposures

Let’s assume we’re performing an experience study as of 2022-12-31 and
we’re interested in policy year exposures. Here’s what we should expect
for our 3 policies.

- Policy 1 was issued on January 1, 2010 and has not terminated.
  Therefore we expect 13 exposure years.
- Policy 2 was issued on May 27, 2011 and was terminated in 2020 due to
  death. The death occurred after the 9th policy anniversary, therefore
  we expect 9 fully exposed years and a partial exposure in the 10th
  year.
- Policy 3 was issued on November 10, 2009 and was terminated in 2022
  due to surrender. The surrender occurred after the 12th policy
  anniversary, therefore we expect 12 fully exposed years and a partial
  exposure in the 13th year.

To calculate exposures, we pass our data to the
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
function and we specify a study `end_date`.

``` r

exposed_data <- expose(toy_census, end_date = "2022-12-31")
```

This creates an `exposed_df` object, which is a type of data frame with
some additional attributes related to the experience study.

``` r

is_exposed_df(exposed_data)
#> [1] TRUE
```

Let’s examine what happened to each policy.

**Policy 1**: As expected, there are 13 rows for this policy. New
columns were added for the policy year (`pol_yr`), date ranges
(`pol_date_yr`, `pol_date_yr_end`), and exposure. All exposures are 100%
since this policy was active for all 13 years.

When the data is printed, additional attributes from the `exposed_df`
class are displayed.

``` r

exposed_data |> filter(pol_num == 1)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status:
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 13 × 8
#>    pol_num status issue_date term_date pol_yr pol_date_yr pol_date_yr_end
#>      <int> <fct>  <date>     <date>     <int> <date>      <date>         
#>  1       1 Active 2010-01-01 NA             1 2010-01-01  2010-12-31     
#>  2       1 Active 2010-01-01 NA             2 2011-01-01  2011-12-31     
#>  3       1 Active 2010-01-01 NA             3 2012-01-01  2012-12-31     
#>  4       1 Active 2010-01-01 NA             4 2013-01-01  2013-12-31     
#>  5       1 Active 2010-01-01 NA             5 2014-01-01  2014-12-31     
#>  6       1 Active 2010-01-01 NA             6 2015-01-01  2015-12-31     
#>  7       1 Active 2010-01-01 NA             7 2016-01-01  2016-12-31     
#>  8       1 Active 2010-01-01 NA             8 2017-01-01  2017-12-31     
#>  9       1 Active 2010-01-01 NA             9 2018-01-01  2018-12-31     
#> 10       1 Active 2010-01-01 NA            10 2019-01-01  2019-12-31     
#> 11       1 Active 2010-01-01 NA            11 2020-01-01  2020-12-31     
#> 12       1 Active 2010-01-01 NA            12 2021-01-01  2021-12-31     
#> 13       1 Active 2010-01-01 NA            13 2022-01-01  2022-12-31     
#> # ℹ 1 more variable: exposure <dbl>
```

**Policy 2**: There are 10 rows for this policy. The first 9 periods
show the policy in an active `status` and the termination date
(`term_date`) is set to `NA`. The last period includes the final status
of “Death” and the actual termination date. The last exposure is less
than one because roughly a third of a year elapsed between the last
anniversary date on 2020-05-27 and the termination date on 2020-09-14.

``` r

exposed_data |> filter(pol_num == 2)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status:
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 10 × 8
#>    pol_num status issue_date term_date  pol_yr pol_date_yr pol_date_yr_end
#>      <int> <fct>  <date>     <date>      <int> <date>      <date>         
#>  1       2 Active 2011-05-27 NA              1 2011-05-27  2012-05-26     
#>  2       2 Active 2011-05-27 NA              2 2012-05-27  2013-05-26     
#>  3       2 Active 2011-05-27 NA              3 2013-05-27  2014-05-26     
#>  4       2 Active 2011-05-27 NA              4 2014-05-27  2015-05-26     
#>  5       2 Active 2011-05-27 NA              5 2015-05-27  2016-05-26     
#>  6       2 Active 2011-05-27 NA              6 2016-05-27  2017-05-26     
#>  7       2 Active 2011-05-27 NA              7 2017-05-27  2018-05-26     
#>  8       2 Active 2011-05-27 NA              8 2018-05-27  2019-05-26     
#>  9       2 Active 2011-05-27 NA              9 2019-05-27  2020-05-26     
#> 10       2 Death  2011-05-27 2020-09-14     10 2020-05-27  2021-05-26     
#> # ℹ 1 more variable: exposure <dbl>
```

**Policy 3**: There are 13 rows for this policy. The first 12 periods
show the policy in an active `status` and the termination date
(`term_date`) is set to `NA`. The last period includes the final status
of “Surrender” and the actual termination date. The last exposure is
less than one because the roughly a third of a year elapsed between the
last anniversary date on 2021-11-10 and the termination date on
2022-02-25.

``` r

exposed_data |> filter(pol_num == 3)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status:
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 13 × 8
#>    pol_num status    issue_date term_date  pol_yr pol_date_yr pol_date_yr_end
#>      <int> <fct>     <date>     <date>      <int> <date>      <date>         
#>  1       3 Active    2009-11-10 NA              1 2009-11-10  2010-11-09     
#>  2       3 Active    2009-11-10 NA              2 2010-11-10  2011-11-09     
#>  3       3 Active    2009-11-10 NA              3 2011-11-10  2012-11-09     
#>  4       3 Active    2009-11-10 NA              4 2012-11-10  2013-11-09     
#>  5       3 Active    2009-11-10 NA              5 2013-11-10  2014-11-09     
#>  6       3 Active    2009-11-10 NA              6 2014-11-10  2015-11-09     
#>  7       3 Active    2009-11-10 NA              7 2015-11-10  2016-11-09     
#>  8       3 Active    2009-11-10 NA              8 2016-11-10  2017-11-09     
#>  9       3 Active    2009-11-10 NA              9 2017-11-10  2018-11-09     
#> 10       3 Active    2009-11-10 NA             10 2018-11-10  2019-11-09     
#> 11       3 Active    2009-11-10 NA             11 2019-11-10  2020-11-09     
#> 12       3 Active    2009-11-10 NA             12 2020-11-10  2021-11-09     
#> 13       3 Surrender 2009-11-10 2022-02-25     13 2021-11-10  2022-11-09     
#> # ℹ 1 more variable: exposure <dbl>
```

### Study start date

The previous section only supplied data and a study `end_date` to
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md).
Optionally, a `start_date` can be supplied that will drop exposure
periods that begin before a specified date.

``` r

expose(toy_census, end_date = "2022-12-31", start_date = "2019-12-31")
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status:
#> • Study range: 2019-12-31 to 2022-12-31
#> 
#> # A tibble: 6 × 8
#>   pol_num status    issue_date term_date  pol_yr pol_date_yr pol_date_yr_end
#>     <int> <fct>     <date>     <date>      <int> <date>      <date>         
#> 1       1 Active    2010-01-01 NA             11 2020-01-01  2020-12-31     
#> 2       1 Active    2010-01-01 NA             12 2021-01-01  2021-12-31     
#> 3       1 Active    2010-01-01 NA             13 2022-01-01  2022-12-31     
#> 4       2 Death     2011-05-27 2020-09-14     10 2020-05-27  2021-05-26     
#> 5       3 Active    2009-11-10 NA             12 2020-11-10  2021-11-09     
#> 6       3 Surrender 2009-11-10 2022-02-25     13 2021-11-10  2022-11-09     
#> # ℹ 1 more variable: exposure <dbl>
```

### Target status

Most experience studies use the annual exposure method which allocates a
full period of exposure for the particular termination event of interest
in the scope of the study.

The intuition for this approach is simple: let’s assume we have an
unrealistically small study with a single data point for one policy over
the course of one year. Let’s assume that policy terminated due to
surrender half way through the year.

If we don’t apply the annual exposure method, we would calculate a
termination rate as:

``` math
q^{surr} = \frac{claims}{exposures} = \frac{1}{0.5} = 200\%
```

A termination rate of 200% doesn’t make any sense. Under the annual
exposure method we would see a rate of 100%, which is intuitive.

``` math
q^{surr} = \frac{claims}{exposures} = \frac{1}{1} = 100\%
```

The annual exposure method can be applied by passing a character vector
of target statuses to the
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
function.

Let’s assume we are performing a surrender study.

``` r

exposed_data2 <- expose(
  toy_census,
  end_date = "2022-12-31",
  target_status = "Surrender"
)
```

Now let’s verify that the exposure on the surrendered policy increased
to 100% in the last exposure period.

``` r

exposed_data2 |>
  group_by(pol_num) |>
  slice_max(pol_yr)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 3 × 8
#> # Groups:   pol_num [3]
#>   pol_num status    issue_date term_date  pol_yr pol_date_yr pol_date_yr_end
#>     <int> <fct>     <date>     <date>      <int> <date>      <date>         
#> 1       1 Active    2010-01-01 NA             13 2022-01-01  2022-12-31     
#> 2       2 Death     2011-05-27 2020-09-14     10 2020-05-27  2021-05-26     
#> 3       3 Surrender 2009-11-10 2022-02-25     13 2021-11-10  2022-11-09     
#> # ℹ 1 more variable: exposure <dbl>
```

## Other exposure periods

The default exposure basis used by
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md) is
policy years. Using the arguments `cal_expo` and `expo_length` other
exposure periods can be used.

### Calendar years

If `cal_expo` is set to `TRUE`, calendar year exposures will be
calculated.

Looking at the second policy, we can see that the first year is
left-censored because the policy was issued two-fifths of the way
through the year, and the last period is right-censored because the
policy terminated roughly seven-tenths of the way through the year.

``` r

exposed_cal <- toy_census |>
  expose(end_date = "2022-12-31", cal_expo = TRUE, target_status = "Surrender")

exposed_cal |> filter(pol_num == 2)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: calendar_year
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 10 × 7
#>    pol_num status issue_date term_date  cal_yr     cal_yr_end exposure
#>      <int> <fct>  <date>     <date>     <date>     <date>        <dbl>
#>  1       2 Active 2011-05-27 NA         2011-01-01 2011-12-31    0.6  
#>  2       2 Active 2011-05-27 NA         2012-01-01 2012-12-31    1    
#>  3       2 Active 2011-05-27 NA         2013-01-01 2013-12-31    1    
#>  4       2 Active 2011-05-27 NA         2014-01-01 2014-12-31    1    
#>  5       2 Active 2011-05-27 NA         2015-01-01 2015-12-31    1    
#>  6       2 Active 2011-05-27 NA         2016-01-01 2016-12-31    1    
#>  7       2 Active 2011-05-27 NA         2017-01-01 2017-12-31    1    
#>  8       2 Active 2011-05-27 NA         2018-01-01 2018-12-31    1    
#>  9       2 Active 2011-05-27 NA         2019-01-01 2019-12-31    1    
#> 10       2 Death  2011-05-27 2020-09-14 2020-01-01 2020-12-31    0.705
```

### Quarters, months, and weeks

The length of the exposure period can be decreased by passing
`"quarter"`, `"month"`, or `"week"` to the `expo_length` argument. This
can be used with policy or calendar-based exposures.

``` r

toy_census |>
  expose(
    end_date = "2022-12-31",
    cal_expo = TRUE,
    expo_length = "quarter",
    target_status = "Surrender"
  ) |>
  filter(pol_num == 2)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: calendar_quarter
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 38 × 7
#>    pol_num status issue_date term_date cal_qtr    cal_qtr_end exposure
#>      <int> <fct>  <date>     <date>    <date>     <date>         <dbl>
#>  1       2 Active 2011-05-27 NA        2011-04-01 2011-06-30     0.385
#>  2       2 Active 2011-05-27 NA        2011-07-01 2011-09-30     1    
#>  3       2 Active 2011-05-27 NA        2011-10-01 2011-12-31     1    
#>  4       2 Active 2011-05-27 NA        2012-01-01 2012-03-31     1    
#>  5       2 Active 2011-05-27 NA        2012-04-01 2012-06-30     1    
#>  6       2 Active 2011-05-27 NA        2012-07-01 2012-09-30     1    
#>  7       2 Active 2011-05-27 NA        2012-10-01 2012-12-31     1    
#>  8       2 Active 2011-05-27 NA        2013-01-01 2013-03-31     1    
#>  9       2 Active 2011-05-27 NA        2013-04-01 2013-06-30     1    
#> 10       2 Active 2011-05-27 NA        2013-07-01 2013-09-30     1    
#> # ℹ 28 more rows
```

### Convenience functions

The following functions are convenience wrappers around
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
that target a specific exposure type without specifying `cal_expo` and
`expo_length`.

- [`expose_py()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  = exposures by policy year
- [`expose_pq()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  = exposures by policy quarter
- [`expose_pm()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  = exposures by policy month
- [`expose_pw()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  = exposures by policy week
- [`expose_cy()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  = exposures by calendar year
- [`expose_cq()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  = exposures by calendar quarter
- [`expose_cm()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  = exposures by calendar month
- [`expose_cw()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  = exposures by calendar week

## Split exposures by calendar period and policy year

A common technique used in experience studies is to split calendar years
into two records: a pre-anniversary record and a post-anniversary
record. In actxps, this can be accomplished using the
[`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md)
function.

Let’s continue examining the second policy. `exposed_cal`, which
contains calendar year exposures, is passed into
[`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md).
The resulting data frame now contains 19 records instead of 10. There is
one record for 2011 and 2 records for all other years. The year 2011
only has a single record because the policy was issued in this year, so
there can only be a post-anniversary record.

``` r

split <- expose_split(exposed_cal)

split |>
  filter(pol_num == 2) |>
  select(cal_yr, cal_yr_end, pol_yr, exposure_pol, exposure_cal)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: split_year
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 19 × 5
#>    cal_yr     cal_yr_end pol_yr exposure_pol exposure_cal
#>    <date>     <date>      <int>        <dbl>        <dbl>
#>  1 2011-05-27 2011-12-31      1        0.598        0.6  
#>  2 2012-01-01 2012-05-26      1        0.402        0.402
#>  3 2012-05-27 2012-12-31      2        0.6          0.598
#>  4 2013-01-01 2013-05-26      2        0.4          0.4  
#>  5 2013-05-27 2013-12-31      3        0.6          0.6  
#>  6 2014-01-01 2014-05-26      3        0.4          0.4  
#>  7 2014-05-27 2014-12-31      4        0.6          0.6  
#>  8 2015-01-01 2015-05-26      4        0.4          0.4  
#>  9 2015-05-27 2015-12-31      5        0.598        0.6  
#> 10 2016-01-01 2016-05-26      5        0.402        0.402
#> 11 2016-05-27 2016-12-31      6        0.6          0.598
#> 12 2017-01-01 2017-05-26      6        0.4          0.4  
#> 13 2017-05-27 2017-12-31      7        0.6          0.6  
#> 14 2018-01-01 2018-05-26      7        0.4          0.4  
#> 15 2018-05-27 2018-12-31      8        0.6          0.6  
#> 16 2019-01-01 2019-05-26      8        0.4          0.4  
#> 17 2019-05-27 2019-12-31      9        0.598        0.6  
#> 18 2020-01-01 2020-05-26      9        0.402        0.402
#> 19 2020-05-27 2020-12-31     10        0.304        0.303
```

The output of
[`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md)
contains two exposure columns.

- `exposure_pol` contains policy year exposures
- `exposure_cal` contains calendar year exposures

The two exposure bases will often not match for two reasons:

1.  Calendar years and policy years have different start and end dates
    that may or may not include a leap day. In the first row, the
    calendar year exposure is 0.6 years of the year 2011, which does not
    include a leap day. In the second row, the policy year exposure is
    0.5984 years of the policy year spanning 2011-05-27 to 2012-05-26,
    which does include a leap day.

2.  Application of the annual exposure method. If the termination event
    of interest appears on a post-anniversary record, policy exposures
    will be 1 and calendar exposures will be the fraction of the year
    spanning the anniversary to December 31st. Conversely, if the
    termination event of interest appears on a pre-anniversary record,
    calendar exposures will be 1 and policy exposures will be the
    fraction of the policy year from January 1st to the last day of the
    current policy year. While it may sound confusing at first, these
    rules are important to ensure that the termination event of interest
    always has an exposure of 1 when the data is grouped on a calendar
    year or policy year basis.

Some downstream functions like
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
expect `exposed_df` objects to have a single column for exposures. For
split exposures, the exposure basis must be specified using the
`col_exposure` argument.

``` r

exp_stats(split)
```

    #> ✖ A `split_exposed_df` was passed without clarifying which exposure basis should be used to summarize results.
    #> ℹ Pass "exposure_pol" to `col_exposure` for policy year exposures.
    #> ℹ Pass "exposure_cal" to `col_exposure` for calendar exposures.

``` r

exp_stats(split, col_exposure = "exposure_pol")
#> 
#> ── Experience study results ──
#> 
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 1 × 4
#>   n_claims claims exposure  q_obs
#>      <int>  <int>    <dbl>  <dbl>
#> 1        1      1     35.3 0.0283
```

[`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md)
doesn’t just work with calendar year exposures. Calendar quarters,
months, or weeks can also be split. For periods shorter than a year, a
record is only split into pre- and post-anniversary segments if a policy
anniversary appears in the middle of the period.

``` r

expose_cq(toy_census, "2022-12-31", target_status = "Surrender") |>
  expose_split() |>
  filter(pol_num == 2) |>
  select(cal_qtr, cal_qtr_end, pol_yr, exposure_pol, exposure_cal)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: split_quarter
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 47 × 5
#>    cal_qtr    cal_qtr_end pol_yr exposure_pol exposure_cal
#>    <date>     <date>       <int>        <dbl>        <dbl>
#>  1 2011-05-27 2011-06-30       1       0.0956        0.385
#>  2 2011-07-01 2011-09-30       1       0.251         1    
#>  3 2011-10-01 2011-12-31       1       0.251         1    
#>  4 2012-01-01 2012-03-31       1       0.249         1    
#>  5 2012-04-01 2012-05-26       1       0.153         0.615
#>  6 2012-05-27 2012-06-30       2       0.0959        0.385
#>  7 2012-07-01 2012-09-30       2       0.252         1    
#>  8 2012-10-01 2012-12-31       2       0.252         1    
#>  9 2013-01-01 2013-03-31       2       0.247         1    
#> 10 2013-04-01 2013-05-26       2       0.153         0.615
#> # ℹ 37 more rows
```

Note, however, that calendar period exposures will always be expressed
in the original units and policy exposures will always be expressed in
years. Above, calendar exposures are quarters whereas policy exposures
are years.

## Tidymodels recipe step

For machine learning feature engineering, the actxps package contains a
function called
[`step_expose()`](https://mattheaphy.github.io/actxps/reference/step_expose.md)
that is compatible with the recipes package from tidymodels. This
function applies the
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
function within a recipe.

``` r

library(recipes)
#> 
#> Attaching package: 'recipes'
#> The following object is masked from 'package:stats':
#> 
#>     step

expo_rec <- recipe(status ~ ., toy_census) |>
  step_expose(
    end_date = "2022-12-31",
    target_status = "Surrender",
    options = list(expo_length = "month")
  ) |>
  prep()

expo_rec
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> outcome:   1
#> predictor: 3
#> 
#> ── Training information
#> Training data contained 3 data points and 1 incomplete row.
#> 
#> ── Operations
#> • Exposed data based on policy months for target status Surrender: <none> |
#>   Trained

tidy(expo_rec, number = 1)
#> # A tibble: 1 × 4
#>   exposure_type target_status start_date end_date  
#>   <chr>         <chr>         <date>     <chr>     
#> 1 policy_month  Surrender     1900-01-01 2022-12-31

bake(expo_rec, new_data = NULL)
#> # A tibble: 416 × 7
#>    issue_date term_date status pol_mth pol_date_mth pol_date_mth_end exposure
#>    <date>     <date>    <fct>    <int> <date>       <date>              <dbl>
#>  1 2010-01-01 NA        Active       1 2010-01-01   2010-01-31              1
#>  2 2010-01-01 NA        Active       2 2010-02-01   2010-02-28              1
#>  3 2010-01-01 NA        Active       3 2010-03-01   2010-03-31              1
#>  4 2010-01-01 NA        Active       4 2010-04-01   2010-04-30              1
#>  5 2010-01-01 NA        Active       5 2010-05-01   2010-05-31              1
#>  6 2010-01-01 NA        Active       6 2010-06-01   2010-06-30              1
#>  7 2010-01-01 NA        Active       7 2010-07-01   2010-07-31              1
#>  8 2010-01-01 NA        Active       8 2010-08-01   2010-08-31              1
#>  9 2010-01-01 NA        Active       9 2010-09-01   2010-09-30              1
#> 10 2010-01-01 NA        Active      10 2010-10-01   2010-10-31              1
#> # ℹ 406 more rows
```

## Miscellaneous

### Column names

As a default, the
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
functions assume the census data frame uses the following naming
conventions:

- The policy number column is called `pol_num`
- The status column is called `status`
- The issue date column is called `issue_date`
- The termination date column is called `term_date`

These default names can be overridden using the `col_pol_num`,
`col_status`, `col_issue_date`, and `col_term_date` arguments.

For example, if the policy number column was called `id` in our
census-level data, we could write:

``` r

expose(
  toy_census,
  end_date = "2022-12-31",
  target_status = "Surrender",
  col_pol_num = "id"
)
```

### Treatment of additional columns in the census data

If the census-level data contains other policy attributes like plan type
or policy values, they will be broadcast across all exposure periods.
Depending on the nature of the data, this may or may not be desirable.
Constant policy attributes like plan type make sense to broadcast, but
numeric values may or may not depending on the circumstances.

``` r

toy_census2 <- toy_census |>
  mutate(plan_type = c("X", "Y", "Z"), policy_value = c(100, 125, 90))

expose(toy_census2, end_date = "2022-12-31", target_status = "Surrender")
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 36 × 10
#>    pol_num status issue_date term_date plan_type policy_value pol_yr
#>      <int> <fct>  <date>     <date>    <chr>            <dbl>  <int>
#>  1       1 Active 2010-01-01 NA        X                  100      1
#>  2       1 Active 2010-01-01 NA        X                  100      2
#>  3       1 Active 2010-01-01 NA        X                  100      3
#>  4       1 Active 2010-01-01 NA        X                  100      4
#>  5       1 Active 2010-01-01 NA        X                  100      5
#>  6       1 Active 2010-01-01 NA        X                  100      6
#>  7       1 Active 2010-01-01 NA        X                  100      7
#>  8       1 Active 2010-01-01 NA        X                  100      8
#>  9       1 Active 2010-01-01 NA        X                  100      9
#> 10       1 Active 2010-01-01 NA        X                  100     10
#> # ℹ 26 more rows
#> # ℹ 3 more variables: pol_date_yr <date>, pol_date_yr_end <date>,
#> #   exposure <dbl>
```

If your experience study requires a numeric feature that varies over
time (ex: policy values, crediting rates, etc.), you can always attach
it to an `exposed_df` object using a join function.

``` r


# Illustrative example - assume `values` is a data frame containing the columns pol_num and pol_yr.

exposed_data |>
  left_join(values, by = c("pol_num", "pol_yr"))
```

### Stacking `exposed_df` objects

If you need to stack two `exposed_df` objects,
[`vctrs::vec_rbind()`](https://vctrs.r-lib.org/reference/vec_bind.html)
is recommended over [`rbind()`](https://rdrr.io/r/base/cbind.html) or
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
The advantage of
[`vctrs::vec_rbind()`](https://vctrs.r-lib.org/reference/vec_bind.html)
is that it will combine attributes across all `exposed_df` objects
passed to the function. The study end date will be updated to maximum
study end date. Similarly, the study start date will be set to the
earliest study start date. Target statuses and transactions types will
become a super set of all observed values. The other two functions will
retain attributes from only the first object passed to them.

For example, below `exposed_data2` contains study start and end dates
that are before and after the study range in `exposed_data`. In
addition, this object contains a target status of “Surrender” whereas
`exposed_data` has none.

When
[`vctrs::vec_rbind()`](https://vctrs.r-lib.org/reference/vec_bind.html)
is used to combine `exposed_data` and `exposed_data2`, the result
combines attributes across both objects.

``` r

exposed_data2 <- expose(
  toy_census,
  end_date = "2023-12-31",
  start_date = "1890-01-01",
  target_status = "Surrender"
)

vctrs::vec_rbind(exposed_data, exposed_data2)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status: Surrender
#> • Study range: 1890-01-01 to 2023-12-31
#> 
#> # A tibble: 73 × 8
#>    pol_num status issue_date term_date pol_yr pol_date_yr pol_date_yr_end
#>      <int> <fct>  <date>     <date>     <int> <date>      <date>         
#>  1       1 Active 2010-01-01 NA             1 2010-01-01  2010-12-31     
#>  2       1 Active 2010-01-01 NA             2 2011-01-01  2011-12-31     
#>  3       1 Active 2010-01-01 NA             3 2012-01-01  2012-12-31     
#>  4       1 Active 2010-01-01 NA             4 2013-01-01  2013-12-31     
#>  5       1 Active 2010-01-01 NA             5 2014-01-01  2014-12-31     
#>  6       1 Active 2010-01-01 NA             6 2015-01-01  2015-12-31     
#>  7       1 Active 2010-01-01 NA             7 2016-01-01  2016-12-31     
#>  8       1 Active 2010-01-01 NA             8 2017-01-01  2017-12-31     
#>  9       1 Active 2010-01-01 NA             9 2018-01-01  2018-12-31     
#> 10       1 Active 2010-01-01 NA            10 2019-01-01  2019-12-31     
#> # ℹ 63 more rows
#> # ℹ 1 more variable: exposure <dbl>
```

If
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
were used instead, the attributes of `exposed_data` only are shown,
which is likely incorrect.

``` r

dplyr::bind_rows(exposed_data, exposed_data2)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status:
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 73 × 8
#>    pol_num status issue_date term_date pol_yr pol_date_yr pol_date_yr_end
#>      <int> <fct>  <date>     <date>     <int> <date>      <date>         
#>  1       1 Active 2010-01-01 NA             1 2010-01-01  2010-12-31     
#>  2       1 Active 2010-01-01 NA             2 2011-01-01  2011-12-31     
#>  3       1 Active 2010-01-01 NA             3 2012-01-01  2012-12-31     
#>  4       1 Active 2010-01-01 NA             4 2013-01-01  2013-12-31     
#>  5       1 Active 2010-01-01 NA             5 2014-01-01  2014-12-31     
#>  6       1 Active 2010-01-01 NA             6 2015-01-01  2015-12-31     
#>  7       1 Active 2010-01-01 NA             7 2016-01-01  2016-12-31     
#>  8       1 Active 2010-01-01 NA             8 2017-01-01  2017-12-31     
#>  9       1 Active 2010-01-01 NA             9 2018-01-01  2018-12-31     
#> 10       1 Active 2010-01-01 NA            10 2019-01-01  2019-12-31     
#> # ℹ 63 more rows
#> # ℹ 1 more variable: exposure <dbl>
```

In order to stack `exposed_df` objects, the exposure period types and
lengths must match. If they do not, an error will be thrown. For
example, policy year exposure records cannot be combined with calendar
month records.

Ordinary data frames can be stacked with `exposed_df` objects using
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
and [`rbind()`](https://rdrr.io/r/base/cbind.html) (assuming all column
names match). If the `exposed_df` object is the first argument, the
`exposed_df` class will be preserved with its original attributes.

### dplyr verb methods and `exposed_df` class persistence

The actxps package includes `exposed_df` methods for the dplyr verbs
listed below. These methods ensure that the functions below will always
return an `exposed_df` object.

- [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
- [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
- [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
- [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
- [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
- [`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)
- [`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html)
- [`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
- [`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
- [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
- [`dplyr::right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
- [`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
- [`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
- [`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
- [`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)

Generally speaking, any dplyr verbs that aren’t listed that return data
frames will preserve the `exposed_df` class **as long as the data is not
grouped.** If the data is grouped, the `exposed_df` class may not
persist. If this creates problems with your code, there are two options:

- If groups don’t matter when the function is applied to the data,
  [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html) the
  data, call the function, and restore the groups with
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
- If groups do matter when the function is applied to the data, convert
  the data to an ordinary data frame or tibble, call the function, and
  convert the data to an `exposed_df` using
  [`as_exposed_df()`](https://mattheaphy.github.io/actxps/reference/is_exposed_df.md).

### Limitations

The
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
family does not support studies with multiple changes between an active
status and an inactive status.
