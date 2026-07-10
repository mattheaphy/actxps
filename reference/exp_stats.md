# Summarize experience study records

Create a summary data frame of termination experience for a given target
status.

## Usage

``` r
exp_stats(
  .data,
  target_status = attr(.data, "target_status"),
  expected,
  col_exposure = "exposure",
  col_status = "status",
  wt = NULL,
  credibility = FALSE,
  conf_level = 0.95,
  cred_r = 0.05,
  conf_int = FALSE,
  control_vars,
  control_distinct_max = 25L
)

# S3 method for class 'exp_df'
summary(object, ...)
```

## Arguments

- .data:

  A data frame with exposure-level records, ideally of type `exposed_df`

- target_status:

  A character vector of target status values

- expected:

  A character vector containing column names in `.data` with expected
  values

- col_exposure:

  Name of the column in `.data` containing exposures

- col_status:

  Name of the column in `.data` containing the policy status

- wt:

  Optional. Length 1 character vector. Name of the column in `.data`
  containing weights to use in the calculation of claims, exposures,
  partial credibility, and confidence intervals.

- credibility:

  If `TRUE`, the output will include partial credibility weights and
  credibility-weighted termination rates.

- conf_level:

  Confidence level used for the Limited Fluctuation credibility method
  and confidence intervals

- cred_r:

  Error tolerance under the Limited Fluctuation credibility method

- conf_int:

  If `TRUE`, the output will include confidence intervals around the
  observed termination rates and any actual-to-expected ratios.

- control_vars:

  `".none"` or a character vector containing column names in `.data` to
  use as control variables

- control_distinct_max:

  Maximum number of unique values allowed for control variables

- object:

  An `exp_df` object

- ...:

  Groups to retain after
  [`summary()`](https://rdrr.io/r/base/summary.html) is called

## Value

A tibble with class `exp_df`, `tbl_df`, `tbl`, and `data.frame`. The
results include columns for any grouping variables, claims, exposures,
and observed termination rates (`q_obs`).

- If any values are passed to `expected` or `control_vars`, additional
  columns are added for expected termination rates and
  actual-to-expected (A/E) ratios. A/E ratios are prefixed by `ae_`.

- If `credibility` is set to `TRUE`, additional columns are added for
  partial credibility and credibility-weighted termination rates
  (assuming values are passed to `expected`). Credibility-weighted
  termination rates are prefixed by `adj_`.

- If `conf_int` is set to `TRUE`, additional columns are added for lower
  and upper confidence interval limits around the observed termination
  rates and any actual-to-expected ratios. Additionally, if
  `credibility` is `TRUE` and expected values are passed to `expected`,
  the output will contain confidence intervals around
  credibility-weighted termination rates. Confidence interval columns
  include the name of the original output column suffixed by either
  `_lower` or `_upper`.

- If a value is passed to `wt`, additional columns are created
  containing the the sum of weights (`.weight`), the sum of squared
  weights (`.weight_qs`), and the number of records (`.weight_n`).

## Details

If `.data` is grouped, the resulting data frame will contain one row per
group.

If `target_status` isn't provided, `exp_stats()` will use the same
target status from `.data` if it has the class `exposed_df`. Otherwise,
all status values except the first level will be assumed. This will
produce a warning message.

## Expected values

The `expected` argument is optional. If provided, this argument must be
a character vector with values corresponding to column names in `.data`
containing expected experience. More than one expected basis can be
provided.

## Control variables

The `control_vars` argument is optional. If provided, this argument must
be `".none"` (more on this below) or a character vector with values
corresponding to column names in `.data`. Control variables are used to
estimate the impact of any grouping variables on observed experience
*after accounting for* the impact of control variables.

Mechanically, when values are passed to `control_vars`, a separate call
is made to `exp_stats()` using the control variables as grouping
variables. This is used to derive a new expected values basis called
`control`, which is both added to `.data` and appended to the `expected`
argument. In the final output, a column called `ae_control` shows the
relative impact of any grouping variables after accounting for the
control variables.

**About `".none"`**: If `".none"` is passed to `control_vars`, a single
aggregate termination rate is calculated for the entire data set and
used to compute `control` and `ae_control`.

The `control_distinct_max` argument places an upper limit on the number
of unique values that a control variable is allowed to have. This limit
exists to prevent an excessive number of groups on continuous or
high-cardinality features.

It should be noted that usage of control variables is a rough
approximation and not a substitute for rigorous statistical models. The
impact of control variables is calculated in isolation and does consider
other features or possible confounding variables. As such, control
variables are most useful for exploratory data analysis.

## Credibility

If `credibility` is set to `TRUE`, the output will contain a
`credibility` column equal to the partial credibility estimate under the
Limited Fluctuation credibility method (also known as Classical
Credibility) assuming a binomial distribution of claims.

## Confidence intervals

If `conf_int` is set to `TRUE`, the output will contain lower and upper
confidence interval limits for the observed termination rate and any
actual-to-expected ratios. The confidence level is dictated by
`conf_level`. If no weighting variable is passed to `wt`, confidence
intervals will be constructed assuming a binomial distribution of
claims. Otherwise, confidence intervals will be calculated assuming that
the aggregate claims distribution is normal with a mean equal to
observed claims and a variance equal to:

`Var(S) = E(N) * Var(X) + E(X)^2 * Var(N)`,

Where `S` is the aggregate claim random variable, `X` is the weighting
variable assumed to follow a normal distribution, and `N` is a binomial
random variable for the number of claims.

If `credibility` is `TRUE` and expected values are passed to `expected`,
the output will also contain confidence intervals for any
credibility-weighted termination rates.

## [`summary()`](https://rdrr.io/r/base/summary.html) Method

Applying [`summary()`](https://rdrr.io/r/base/summary.html) to a
`exp_df` object will re-summarize the data while retaining any grouping
variables passed to the "dots" (`...`).

## References

Herzog, Thomas (1999). Introduction to Credibility Theory

## Examples

``` r
toy_census |> expose("2022-12-31", target_status = "Surrender") |>
    exp_stats()
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

exp_res <- census_dat |>
           expose("2019-12-31", target_status = "Surrender") |>
           group_by(pol_yr, inc_guar) |>
           exp_stats(control_vars = "product")

exp_res
#> 
#> ── Experience study results ──
#> 
#> • Groups: pol_yr and inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Control variables: product
#> • Expected values: control
#> 
#> # A tibble: 30 × 8
#>    pol_yr inc_guar n_claims claims exposure   q_obs control ae_control
#>     <int> <lgl>       <int>  <int>    <dbl>   <dbl>   <dbl>      <dbl>
#>  1      1 FALSE          56     56    7720. 0.00725  0.0217      0.335
#>  2      1 TRUE           46     46   11532. 0.00399  0.0217      0.184
#>  3      2 FALSE          92     92    7103. 0.0130   0.0216      0.598
#>  4      2 TRUE           68     68   10612. 0.00641  0.0216      0.296
#>  5      3 FALSE          67     67    6447. 0.0104   0.0216      0.480
#>  6      3 TRUE           57     57    9650. 0.00591  0.0216      0.273
#>  7      4 FALSE         123    123    5799. 0.0212   0.0216      0.980
#>  8      4 TRUE           45     45    8737. 0.00515  0.0216      0.238
#>  9      5 FALSE          97     97    5106. 0.0190   0.0216      0.878
#> 10      5 TRUE           67     67    7810. 0.00858  0.0216      0.396
#> # ℹ 20 more rows
summary(exp_res)
#> 
#> ── Experience study results ──
#> 
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Control variables: product
#> • Expected values: control
#> 
#> # A tibble: 1 × 6
#>   n_claims claims exposure  q_obs control ae_control
#>      <int>  <int>    <dbl>  <dbl>   <dbl>      <dbl>
#> 1     2869   2869  132634. 0.0216  0.0216          1
summary(exp_res, inc_guar)
#> 
#> ── Experience study results ──
#> 
#> • Groups: inc_guar
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Control variables: product
#> • Expected values: control
#> 
#> # A tibble: 2 × 7
#>   inc_guar n_claims claims exposure  q_obs control ae_control
#>   <lgl>       <int>  <int>    <dbl>  <dbl>   <dbl>      <dbl>
#> 1 FALSE        1601   1601   52123. 0.0307  0.0216      1.42 
#> 2 TRUE         1268   1268   80511. 0.0157  0.0216      0.728
```
