# Transaction summary helper functions

Convert aggregate transaction experience studies to the `trx_df` class.

## Usage

``` r
as_trx_df(
  x,
  col_trx_amt = "trx_amt",
  col_trx_n = "trx_n",
  col_trx_flag = "trx_flag",
  col_exposure = "exposure",
  col_percent_of = NULL,
  col_percent_of_w_trx = NULL,
  col_trx_amt_sq = "trx_amt_sq",
  start_date = as.Date("1900-01-01"),
  end_date = NULL,
  conf_int = FALSE,
  conf_level = 0.95
)

is_trx_df(x)
```

## Arguments

- x:

  An object. For `as_trx_df()`, `x` must be a data frame.

- col_trx_amt:

  Optional. Name of the column in `x` containing transaction amounts.

- col_trx_n:

  Optional. Name of the column in `x` containing transaction counts.

- col_trx_flag:

  Optional. Name of the column in `x` containing the number of exposure
  records with transactions.

- col_exposure:

  Optional. Name of the column in `x` containing exposures.

- col_percent_of:

  Optional. Name of the column in `x` containing a numeric variable to
  use in "percent of" calculations.

- col_percent_of_w_trx:

  Optional. Name of the column in `x` containing a numeric variable to
  use in "percent of" calculations with transactions.

- col_trx_amt_sq:

  Optional and only required when `col_percent_of` is passed and
  `conf_int` is `TRUE`. Name of the column in `x` containing squared
  transaction amounts.

- start_date:

  Experience study start date. Default value = 1900-01-01.

- end_date:

  Experience study end date

- conf_int:

  If `TRUE`, future calls to
  [`summary()`](https://rdrr.io/r/base/summary.html) will include
  confidence intervals around the observed utilization rates and any
  `percent_of` output columns.

- conf_level:

  Confidence level for confidence intervals

## Value

For `is_trx_df()`, a length-1 logical vector. For `as_trx_df()`, a
`trx_df` object.

## Details

`is_trx_df()` will return `TRUE` if `x` is a `trx_df` object.

`as_trx_df()` will coerce a data frame to a `trx_df` object if that data
frame has the required columns for transaction studies listed below.

`as_trx_df()` is most useful for working with aggregate summaries of
experience that were not created by actxps where individual policy
information is not available. After converting the data to the `trx_df`
class, [`summary()`](https://rdrr.io/r/base/summary.html) can be used to
summarize data by any grouping variables, and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
and
[`autotable()`](https://mattheaphy.github.io/actxps/reference/autotable.md)
are available for reporting.

At a minimum, the following columns are required:

- Transaction amounts (`trx_amt`)

- Transaction counts (`trx_n`)

- The number of exposure records with transactions (`trx_flag`). This
  number is not necessarily equal to transaction counts. If multiple
  transactions are allowed per exposure period, `trx_flag` will be less
  than `trx_n`.

- Exposures (`exposure`)

If transaction amounts should be expressed as a percentage of another
variable (i.e. to calculate utilization rates or actual-to-expected
ratios), additional columns are required:

- A denominator "percent of" column. For example, the sum of account
  values.

- A denominator "percent of" column for exposure records with
  transactions. For example, the sum of account values across all
  records with non-zero transaction amounts.

If confidence intervals are desired and "percent of" columns are passed,
an additional column for the sum of squared transaction amounts
(`trx_amt_sq`) is also required.

The names in parentheses above are expected column names. If the data
frame passed to `as_trx_df()` uses different column names, these can be
specified using the `col_*` arguments.

`start_date`, and `end_date` are optional arguments that are only used
for printing the resulting `trx_df` object.

Unlike
[`trx_stats()`](https://mattheaphy.github.io/actxps/reference/trx_stats.md),
`as_trx_df()` only permits a single transaction type and a single
`percent_of` column.

## See also

[`trx_stats()`](https://mattheaphy.github.io/actxps/reference/trx_stats.md)
for information on how `trx_df` objects are typically created from
individual exposure records.

## Examples

``` r
# convert pre-aggregated experience into a trx_df object
dat <- as_trx_df(agg_sim_dat,
                 col_exposure = "n",
                 col_trx_amt = "wd",
                 col_trx_n = "wd_n",
                 col_trx_flag = "wd_flag",
                 col_percent_of = "av",
                 col_percent_of_w_trx = "av_w_wd",
                 col_trx_amt_sq = "wd_sq",
                 start_date = 2005, end_date = 2019,
                 conf_int = TRUE)
dat
#> 
#> ── Transaction study results ──
#> 
#> • Study range: 2005 to 2019
#> • Transaction types: wd
#> • Transactions as % of: av
#> 
#> # A tibble: 180 × 17
#>    pol_yr inc_guar qual  product exposure_n claims_n      av exposure_amt
#>     <int> <lgl>    <lgl> <fct>        <dbl>    <int>   <dbl>        <dbl>
#>  1      1 FALSE    FALSE a             880.        3 1204699     1168118.
#>  2      1 FALSE    FALSE b             885.        9 1209902     1154044.
#>  3      1 FALSE    FALSE c            1685.       12 2265030     2170944.
#>  4      1 FALSE    TRUE  a            1073.       11 1433862     1375173.
#>  5      1 FALSE    TRUE  b            1086.        8 1449080     1396136.
#>  6      1 FALSE    TRUE  c            2111.       13 2907067     2794199.
#>  7      1 TRUE     FALSE a            1299.        9 1796934     1744069.
#>  8      1 TRUE     FALSE b            1230.        2 1732269     1662742.
#>  9      1 TRUE     FALSE c            2702.       11 3700302     3590893.
#> 10      1 TRUE     TRUE  a            1601.        5 2234728     2147418.
#> # ℹ 170 more rows
#> # ℹ 9 more variables: claims_amt <dbl>, av_sq <dbl>, exposure <int>,
#> #   trx_amt <dbl>, trx_n <dbl>, trx_flag <int>, trx_amt_sq <dbl>,
#> #   av_w_trx <dbl>, trx_type <chr>
is_trx_df(dat)
#> [1] TRUE

# summary by policy year
summary(dat, pol_yr)
#> 
#> ── Transaction study results ──
#> 
#> • Groups: pol_yr
#> • Study range: 2005 to 2019
#> • Transaction types: wd
#> • Transactions as % of: av
#> 
#> # A tibble: 15 × 21
#>    pol_yr trx_type trx_n trx_flag trx_amt exposure avg_trx avg_all trx_freq
#>     <int> <chr>    <dbl>    <int>   <dbl>    <int>   <dbl>   <dbl>    <dbl>
#>  1      1 wd       16942     7921  419763    19995    53.0    21.0     2.14
#>  2      2 wd       16900     7923  440919    18434    55.7    23.9     2.13
#>  3      3 wd       16679     7796  449349    16806    57.6    26.7     2.14
#>  4      4 wd       16193     7575  440510    15266    58.2    28.9     2.14
#>  5      5 wd       15353     7153  420462    13618    58.8    30.9     2.15
#>  6      6 wd       14382     6708  411073    12067    61.3    34.1     2.14
#>  7      7 wd       13248     6183  396732    10541    64.2    37.6     2.14
#>  8      8 wd       12034     5603  369098     9130    65.9    40.4     2.15
#>  9      9 wd       10461     4868  330507     7591    67.9    43.5     2.15
#> 10     10 wd        8978     4155  290363     6185    69.9    46.9     2.16
#> 11     11 wd        7470     3448  243606     4897    70.7    49.7     2.17
#> 12     12 wd        5079     2314  176383     3093    76.2    57.0     2.19
#> 13     13 wd        3310     1495  119153     1937    79.7    61.5     2.21
#> 14     14 wd        2119      946   85073     1182    89.9    72.0     2.24
#> 15     15 wd         982      430   40328      510    93.8    79.1     2.28
#> # ℹ 12 more variables: trx_util <dbl>, av_w_trx <dbl>, av <dbl>,
#> #   pct_of_av_w_trx <dbl>, pct_of_av_all <dbl>, trx_util_lower <dbl>,
#> #   trx_util_upper <dbl>, pct_of_av_w_trx_lower <dbl>,
#> #   pct_of_av_w_trx_upper <dbl>, pct_of_av_all_lower <dbl>,
#> #   pct_of_av_all_upper <dbl>, trx_amt_sq <dbl>
```
