# Additional plotting functions for transaction studies

These functions create additional experience study plots that are not
available or difficult to produce using the
[`autoplot.trx_df()`](https://mattheaphy.github.io/actxps/reference/autoplot_exp.md)
function.

## Usage

``` r
plot_utilization_rates(object, ...)
```

## Arguments

- object:

  An object of class `trx_df` created by the function
  [`trx_stats()`](https://mattheaphy.github.io/actxps/reference/trx_stats.md).

- ...:

  Additional arguments passed to
  [`autoplot.trx_df()`](https://mattheaphy.github.io/actxps/reference/autoplot_exp.md).

## Value

a `ggplot` object

## Details

`plot_utilization_rates()` - Create a plot of transaction frequency and
severity. Frequency is represented by utilization rates (`trx_util`).
Severity is represented by transaction amounts as a percentage of one or
more other columns in the data (`{*}_w_trx`). All severity series begin
with the prefix "pct_of\_" and end with the suffix "\_w_trx". The suffix
refers to the fact that the denominator only includes records with
non-zero transactions. Severity series are based on column names passed
to the `percent_of` argument in
[`trx_stats()`](https://mattheaphy.github.io/actxps/reference/trx_stats.md).
If no "percentage of" columns exist in `object`, this function will only
plot utilization rates.

## See also

[`autoplot.trx_df()`](https://mattheaphy.github.io/actxps/reference/autoplot_exp.md)

## Examples

``` r

study_py <- expose_py(census_dat, "2019-12-31",
                      target_status = "Surrender") |>
  add_transactions(withdrawals) |>
  left_join(account_vals, by = c("pol_num", "pol_date_yr"))

trx_res <- study_py |> group_by(pol_yr) |>
  trx_stats(percent_of = "av_anniv", combine_trx = TRUE)

plot_utilization_rates(trx_res)

```
