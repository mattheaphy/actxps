# Termination summary helper functions

Convert aggregate termination experience studies to the `exp_df` class.

## Usage

``` r
as_exp_df(
  x,
  expected = NULL,
  wt = NULL,
  col_claims,
  col_exposure,
  col_n_claims,
  col_weight_sq,
  col_weight_n,
  target_status = NULL,
  start_date = as.Date("1900-01-01"),
  end_date = NULL,
  credibility = FALSE,
  conf_level = 0.95,
  cred_r = 0.05,
  conf_int = FALSE
)

is_exp_df(x)
```

## Arguments

- x:

  An object. For `as_exp_df()`, `x` must be a data frame.

- expected:

  A character vector containing column names in x with expected values

- wt:

  Optional. Length 1 character vector. Name of the column in `x`
  containing weights to use in the calculation of claims, exposures,
  partial credibility, and confidence intervals.

- col_claims:

  Optional. Name of the column in `x` containing claims. The assumed
  default is "claims".

- col_exposure:

  Optional. Name of the column in `x` containing exposures. The assumed
  default is "exposure".

- col_n_claims:

  Optional and only used used when `wt` is passed. Name of the column in
  `x` containing the number of claims.

- col_weight_sq:

  Optional and only used used when `wt` is passed. Name of the column in
  `x` containing the sum of squared weights.

- col_weight_n:

  Optional and only used used when `wt` is passed. Name of the column in
  `x` containing exposure record counts.

- target_status:

  Character vector of target status values. Default value = `NULL`.

- start_date:

  Experience study start date. Default value = 1900-01-01.

- end_date:

  Experience study end date

- credibility:

  If `TRUE`, future calls to
  [`summary()`](https://rdrr.io/r/base/summary.html) will include
  partial credibility weights and credibility-weighted termination
  rates.

- conf_level:

  Confidence level used for the Limited Fluctuation credibility method
  and confidence intervals

- cred_r:

  Error tolerance under the Limited Fluctuation credibility method

- conf_int:

  If `TRUE`, future calls to
  [`summary()`](https://rdrr.io/r/base/summary.html) will include
  confidence intervals around the observed termination rates and any
  actual-to-expected ratios.

## Value

For `is_exp_df()`, a length-1 logical vector. For `as_exp_df()`, an
`exp_df` object.

## Details

`is_exp_df()` will return `TRUE` if `x` is an `exp_df` object.

`as_exp_df()` will coerce a data frame to an `exp_df` object if that
data frame has columns for exposures and claims.

`as_exp_df()` is most useful for working with aggregate summaries of
experience that were not created by actxps where individual policy
information is not available. After converting the data to the `exp_df`
class, [`summary()`](https://rdrr.io/r/base/summary.html) can be used to
summarize data by any grouping variables, and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
and
[`autotable()`](https://mattheaphy.github.io/actxps/reference/autotable.md)
are available for reporting.

If nothing is passed to `wt`, the data frame `x` must include columns
containing:

- Exposures (`exposure`)

- Claim counts (`claims`)

If `wt` is passed, the data must include columns containing:

- Weighted exposures (`exposure`)

- Weighted claims (`claims`)

- Claim counts (`n_claims`)

- The raw sum of weights **NOT** multiplied by exposures

- Exposure record counts (`.weight_n`)

- The raw sum of squared weights (`.weight_sq`)

The names in parentheses above are expected column names. If the data
frame passed to `as_exp_df()` uses different column names, these can be
specified using the `col_*` arguments.

When a column name is passed to `wt`, the columns `.weight`,
`.weight_n`, and `.weight_sq` are used to calculate credibility and
confidence intervals. If credibility and confidence intervals aren't
required, then it is not necessary to pass anything to `wt`. The results
of `as_exp_df()` and any downstream summaries will still be weighted as
long as the exposures and claims are pre-weighted.

`target_status`, `start_date`, and `end_date` are optional arguments
that are only used for printing the resulting `exp_df` object.

## See also

[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
for information on how `exp_df` objects are typically created from
individual exposure records.

## Examples

``` r
# convert pre-aggregated experience into an exp_df object
dat <- as_exp_df(agg_sim_dat, col_exposure = "exposure_n",
                 col_claims = "claims_n",
                 target_status = "Surrender",
                 start_date = 2005, end_date = 2019,
                 conf_int = TRUE)
dat
#> 
#> ── Experience study results ──
#> 
#> • Target status: Surrender
#> • Study range: 2005 to 2019
#> 
#> # A tibble: 180 × 17
#>    pol_yr inc_guar qual  product exposure claims      av exposure_amt claims_amt
#>     <int> <lgl>    <lgl> <fct>      <dbl>  <int>   <dbl>        <dbl>      <dbl>
#>  1      1 FALSE    FALSE a           880.      3 1204699     1168118.       1236
#>  2      1 FALSE    FALSE b           885.      9 1209902     1154044.       8472
#>  3      1 FALSE    FALSE c          1685.     12 2265030     2170944.       9245
#>  4      1 FALSE    TRUE  a          1073.     11 1433862     1375173.       8851
#>  5      1 FALSE    TRUE  b          1086.      8 1449080     1396136.       7095
#>  6      1 FALSE    TRUE  c          2111.     13 2907067     2794199.      12468
#>  7      1 TRUE     FALSE a          1299.      9 1796934     1744069.       5272
#>  8      1 TRUE     FALSE b          1230.      2 1732269     1662742.       1059
#>  9      1 TRUE     FALSE c          2702.     11 3700302     3590893.       9647
#> 10      1 TRUE     TRUE  a          1601.      5 2234728     2147418.       3354
#> # ℹ 170 more rows
#> # ℹ 8 more variables: av_sq <dbl>, n <int>, wd <dbl>, wd_n <dbl>,
#> #   wd_flag <int>, wd_sq <dbl>, av_w_wd <dbl>, n_claims <int>
is_exp_df(dat)
#> [1] TRUE

# summary by policy year
summary(dat, pol_yr)
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr
#> • Target status: Surrender
#> • Study range: 2005 to 2019
#> 
#> # A tibble: 15 × 7
#>    pol_yr n_claims claims exposure   q_obs q_obs_lower q_obs_upper
#>     <int>    <int>  <int>    <dbl>   <dbl>       <dbl>       <dbl>
#>  1      1      102    102   19252. 0.00530     0.00431     0.00634
#>  2      2      160    160   17715. 0.00903     0.00768     0.0104 
#>  3      3      124    124   16097. 0.00770     0.00640     0.00907
#>  4      4      168    168   14536. 0.0116      0.00984     0.0133 
#>  5      5      164    164   12916. 0.0127      0.0108      0.0146 
#>  6      6      152    152   11376. 0.0134      0.0113      0.0155 
#>  7      7      164    164    9917. 0.0165      0.0141      0.0191 
#>  8      8      190    190    8448. 0.0225      0.0194      0.0257 
#>  9      9      181    181    6960. 0.0260      0.0223      0.0297 
#> 10     10      152    152    5604. 0.0271      0.0230      0.0314 
#> 11     11      804    804    4390. 0.183       0.172       0.195  
#> 12     12      330    330    2663. 0.124       0.112       0.137  
#> 13     13       99     99    1620. 0.0611      0.0500      0.0728 
#> 14     14       62     62     872. 0.0711      0.0551      0.0883 
#> 15     15       17     17     268. 0.0634      0.0373      0.0932 

# repeat the prior exercise on a weighted basis
dat_wt <- as_exp_df(agg_sim_dat, wt = "av",
                    col_exposure = "exposure_amt",
                    col_claims = "claims_amt",
                    col_n_claims = "claims_n",
                    col_weight_sq = "av_sq",
                    col_weight_n = "n",
                    target_status = "Surrender",
                    start_date = 2005, end_date = 2019,
                    conf_int = TRUE)
dat_wt
#> 
#> ── Experience study results ──
#> 
#> • Target status: Surrender
#> • Study range: 2005 to 2019
#> • Weighted by: av
#> 
#> # A tibble: 180 × 16
#>    pol_yr inc_guar qual  product exposure_n n_claims .weight exposure claims
#>     <int> <lgl>    <lgl> <fct>        <dbl>    <int>   <dbl>    <dbl>  <dbl>
#>  1      1 FALSE    FALSE a             880.        3 1204699 1168118.   1236
#>  2      1 FALSE    FALSE b             885.        9 1209902 1154044.   8472
#>  3      1 FALSE    FALSE c            1685.       12 2265030 2170944.   9245
#>  4      1 FALSE    TRUE  a            1073.       11 1433862 1375173.   8851
#>  5      1 FALSE    TRUE  b            1086.        8 1449080 1396136.   7095
#>  6      1 FALSE    TRUE  c            2111.       13 2907067 2794199.  12468
#>  7      1 TRUE     FALSE a            1299.        9 1796934 1744069.   5272
#>  8      1 TRUE     FALSE b            1230.        2 1732269 1662742.   1059
#>  9      1 TRUE     FALSE c            2702.       11 3700302 3590893.   9647
#> 10      1 TRUE     TRUE  a            1601.        5 2234728 2147418.   3354
#> # ℹ 170 more rows
#> # ℹ 7 more variables: .weight_sq <dbl>, .weight_n <int>, wd <dbl>, wd_n <dbl>,
#> #   wd_flag <int>, wd_sq <dbl>, av_w_wd <dbl>

# summary by policy year
summary(dat_wt, pol_yr)
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr
#> • Target status: Surrender
#> • Study range: 2005 to 2019
#> • Weighted by: av
#> 
#> # A tibble: 15 × 10
#>    pol_yr n_claims  claims  exposure   q_obs q_obs_lower q_obs_upper  .weight
#>     <int>    <int>   <dbl>     <dbl>   <dbl>       <dbl>       <dbl>    <dbl>
#>  1      1      102   83223 25312813. 0.00329     0.00193     0.00465 26301746
#>  2      2      160  175428 24018741. 0.00730     0.00545     0.00916 24966449
#>  3      3      124  132261 22435291. 0.00590     0.00409     0.00770 23442831
#>  4      4      168  192473 20859830. 0.00923     0.00691     0.0115  21861723
#>  5      5      164  197240 19114538. 0.0103      0.00773     0.0129  20104670
#>  6      6      152  192088 17327030. 0.0111      0.00823     0.0139  18388593
#>  7      7      164  186649 15496358. 0.0120      0.00862     0.0155  16440062
#>  8      8      190  220055 13628469. 0.0161      0.0118      0.0205  14647775
#>  9      9      181  232704 11562793. 0.0201      0.0149      0.0253  12603553
#> 10     10      152  209249  9510599. 0.0220      0.0161      0.0279  10531703
#> 11     11      804 1153832  7654553. 0.151       0.134       0.167    8501473
#> 12     12      330  517875  4895865. 0.106       0.0879      0.124    5713464
#> 13     13       99  179358  3134823. 0.0572      0.0413      0.0731   3678776
#> 14     14       62  114390  1727042. 0.0662      0.0423      0.0902   2343501
#> 15     15       17   26265   520946. 0.0504      0.00836     0.0925   1019390
#> # ℹ 2 more variables: .weight_sq <dbl>, .weight_n <int>

```
