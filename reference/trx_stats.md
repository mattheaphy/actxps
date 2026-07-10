# Summarize transactions and utilization rates

Create a summary data frame of transaction counts, amounts, and
utilization rates.

## Usage

``` r
trx_stats(
  .data,
  trx_types,
  percent_of = NULL,
  combine_trx = FALSE,
  col_exposure = "exposure",
  full_exposures_only = TRUE,
  conf_int = FALSE,
  conf_level = 0.95
)

# S3 method for class 'trx_df'
summary(object, ...)
```

## Arguments

- .data:

  A data frame with exposure-level records of type `exposed_df` with
  transaction data attached. If necessary, use
  [`as_exposed_df()`](https://mattheaphy.github.io/actxps/reference/is_exposed_df.md)
  to convert a data frame to an `exposed_df` object, and use
  [`add_transactions()`](https://mattheaphy.github.io/actxps/reference/add_transactions.md)
  to attach transactions to an `exposed_df` object.

- trx_types:

  A character vector of transaction types to include in the output. If
  none is provided, all available transaction types in `.data` will be
  used.

- percent_of:

  A optional character vector containing column names in `.data` to use
  as denominators in the calculation of utilization rates or
  actual-to-expected ratios.

- combine_trx:

  If `FALSE` (default), the results will contain output rows for each
  transaction type. If `TRUE`, the results will contains aggregated
  experience across all transaction types.

- col_exposure:

  Name of the column in `.data` containing exposures

- full_exposures_only:

  If `TRUE` (default), partially exposed records will be excluded from
  `data`.

- conf_int:

  If `TRUE`, the output will include confidence intervals around the
  observed utilization rate and any `percent_of` output columns.

- conf_level:

  Confidence level for confidence intervals

- object:

  A `trx_df` object

- ...:

  Groups to retain after
  [`summary()`](https://rdrr.io/r/base/summary.html) is called

## Value

A tibble with class `trx_df`, `tbl_df`, `tbl`, and `data.frame`. The
results include columns for any grouping variables and transaction
types, plus the following:

- `trx_n`: the number of unique transactions.

- `trx_amt`: total transaction amount

- `trx_flag`: the number of observation periods with non-zero
  transaction amounts.

- `exposure`: total exposures

- `avg_trx`: mean transaction amount (`trx_amt / trx_flag`)

- `avg_all`: mean transaction amount over all records
  (`trx_amt / exposure`)

- `trx_freq`: transaction frequency when a transaction occurs
  (`trx_n / trx_flag`)

- `trx_util`: transaction utilization per observation period
  (`trx_flag / exposure`)

If `percent_of` is provided, the results will also include:

- The sum of any columns passed to `percent_of` with non-zero
  transactions. These columns include the suffix `_w_trx`.

- The sum of any columns passed to `percent_of`

- `pct_of_{*}_w_trx`: total transactions as a percentage of column
  `{*}_w_trx`. In other words, total transactions divided by the sum of
  a column including only records utilizing transactions.

- `pct_of_{*}_all`: total transactions as a percentage of column `{*}`.
  In other words, total transactions divided by the sum of a column
  regardless of whether or not transactions were utilized.

If `conf_int` is set to `TRUE`, additional columns are added for lower
and upper confidence interval limits around the observed utilization
rate and any `percent_of` output columns. Confidence interval columns
include the name of the original output column suffixed by either
`_lower` or `_upper`.

- If values are passed to `percent_of`, an additional column is created
  containing the the sum of squared transaction amounts (`trx_amt_sq`).

## Details

Unlike
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md),
this function requires `data` to be an `exposed_df` object.

If `.data` is grouped, the resulting data frame will contain one row per
transaction type per group.

Any number of transaction types can be passed to the `trx_types`
argument, however each transaction type **must** appear in the
`trx_types` attribute of `.data`. In addition, `trx_stats()` expects to
see columns named `trx_n_{*}` (for transaction counts) and `trx_amt_{*}`
for (transaction amounts) for each transaction type. To ensure `.data`
is in the appropriate format, use the functions
[`as_exposed_df()`](https://mattheaphy.github.io/actxps/reference/is_exposed_df.md)
to convert an existing data frame with transactions or
[`add_transactions()`](https://mattheaphy.github.io/actxps/reference/add_transactions.md)
to attach transactions to an existing `exposed_df` object.

## "Percentage of" calculations

The `percent_of` argument is optional. If provided, this argument must
be a character vector with values corresponding to columns in `.data`
containing values to use as denominators in the calculation of
utilization rates or actual-to-expected ratios. Example usage:

- In a study of partial withdrawal transactions, if `percent_of` refers
  to account values, observed withdrawal rates can be determined.

- In a study of recurring claims, if `percent_of` refers to a column
  containing a maximum benefit amount, utilization rates can be
  determined.

## Confidence intervals

If `conf_int` is set to `TRUE`, the output will contain lower and upper
confidence interval limits for the observed utilization rate and any
`percent_of` output columns. The confidence level is dictated by
`conf_level`.

- Intervals for the utilization rate (`trx_util`) assume a binomial
  distribution.

- Intervals for transactions as a percentage of another column with
  non-zero transactions (`pct_of_{*}_w_trx`) are constructed using a
  normal distribution

- Intervals for transactions as a percentage of another column
  regardless of transaction utilization (`pct_of_{*}_all`) are
  calculated assuming that the aggregate distribution is normal with a
  mean equal to observed transactions and a variance equal to:

  `Var(S) = E(N) * Var(X) + E(X)^2 * Var(N)`,

  Where `S` is the aggregate transactions random variable, `X` is an
  individual transaction amount assumed to follow a normal distribution,
  and `N` is a binomial random variable for transaction utilization.

## Default removal of partial exposures

As a default, partial exposures are removed from `.data` before
summarizing results. This is done to avoid complexity associated with a
lopsided skew in the timing of transactions. For example, if
transactions can occur on a monthly basis or annually at the beginning
of each policy year, partial exposures may not be appropriate. If a
policy had an exposure of 0.5 years and was taking withdrawals annually
at the beginning of the year, an argument could be made that the
exposure should instead be 1 complete year. If the same policy was
expected to take withdrawals 9 months into the year, it's not clear if
the exposure should be 0.5 years or 0.5 / 0.75 years. To override this
treatment, set `full_exposures_only` to `FALSE`.

## [`summary()`](https://rdrr.io/r/base/summary.html) Method

Applying [`summary()`](https://rdrr.io/r/base/summary.html) to a
`trx_df` object will re-summarize the data while retaining any grouping
variables passed to the "dots" (`...`).

## Examples

``` r
expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender") |>
  add_transactions(withdrawals)

res <- expo |> group_by(inc_guar) |> trx_stats(percent_of = "premium")
res
#> 
#> ── Transaction study results ──
#> 
#> • Groups: inc_guar
#> • Study range: 1900-01-01 to 2019-12-31
#> • Transaction types: Base and Rider
#> • Transactions as % of: premium
#> 
#> # A tibble: 4 × 14
#>   inc_guar trx_type trx_n trx_flag trx_amt exposure avg_trx avg_all trx_freq
#>   <lgl>    <chr>    <dbl>    <int>   <dbl>    <dbl>   <dbl>   <dbl>    <dbl>
#> 1 FALSE    Base     52939    24703  952629    48938    38.6   19.5      2.14
#> 2 FALSE    Rider        0        0       0    48938   NaN      0      NaN   
#> 3 TRUE     Base      7561     3521  141270    75235    40.1    1.88     2.15
#> 4 TRUE     Rider    77321    35941 2842729    75235    79.1   37.8      2.15
#> # ℹ 5 more variables: trx_util <dbl>, premium_w_trx <dbl>, premium <dbl>,
#> #   pct_of_premium_w_trx <dbl>, pct_of_premium_all <dbl>

summary(res)
#> 
#> ── Transaction study results ──
#> 
#> • Study range: 1900-01-01 to 2019-12-31
#> • Transaction types: Base and Rider
#> • Transactions as % of: premium
#> 
#> # A tibble: 2 × 13
#>   trx_type trx_n trx_flag trx_amt exposure avg_trx avg_all trx_freq trx_util
#>   <chr>    <dbl>    <int>   <dbl>    <dbl>   <dbl>   <dbl>    <dbl>    <dbl>
#> 1 Base     60500    28224 1093899   124173    38.8    8.81     2.14    0.227
#> 2 Rider    77321    35941 2842729   124173    79.1   22.9      2.15    0.289
#> # ℹ 4 more variables: premium_w_trx <dbl>, premium <dbl>,
#> #   pct_of_premium_w_trx <dbl>, pct_of_premium_all <dbl>

expo |> group_by(inc_guar) |>
  trx_stats(percent_of = "premium", combine_trx = TRUE, conf_int = TRUE)
#> 
#> ── Transaction study results ──
#> 
#> • Groups: inc_guar
#> • Study range: 1900-01-01 to 2019-12-31
#> • Transaction types: Base and Rider
#> • Transactions as % of: premium
#> 
#> # A tibble: 2 × 21
#>   inc_guar trx_type trx_n trx_flag trx_amt exposure avg_trx avg_all trx_freq
#>   <lgl>    <chr>    <dbl>    <int>   <dbl>    <dbl>   <dbl>   <dbl>    <dbl>
#> 1 FALSE    All      52939    24703  952629    48938    38.6    19.5     2.14
#> 2 TRUE     All      84882    39462 2983999    75235    75.6    39.7     2.15
#> # ℹ 12 more variables: trx_util <dbl>, premium_w_trx <dbl>, premium <dbl>,
#> #   pct_of_premium_w_trx <dbl>, pct_of_premium_all <dbl>, trx_util_lower <dbl>,
#> #   trx_util_upper <dbl>, pct_of_premium_w_trx_lower <dbl>,
#> #   pct_of_premium_w_trx_upper <dbl>, pct_of_premium_all_lower <dbl>,
#> #   pct_of_premium_all_upper <dbl>, trx_amt_sq <dbl>
```
