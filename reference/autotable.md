# Tabular experience study summary

`autotable()` is a generic function used to create a table from an
object of a particular class. Tables are constructed using the `gt`
package.

`autotable.exp_df()` is used to convert experience study results to a
presentation-friendly format.

`autotable.trx_df()` is used to convert transaction study results to a
presentation-friendly format.

## Usage

``` r
autotable(object, ...)

# S3 method for class 'exp_df'
autotable(
  object,
  fontsize = 100,
  decimals = 1,
  colorful = TRUE,
  color_q_obs = "RColorBrewer::GnBu",
  color_ae_ = "RColorBrewer::RdBu",
  rename_cols = rlang::list2(...),
  show_conf_int = FALSE,
  show_cred_adj = FALSE,
  decimals_amt = 0,
  suffix_amt = FALSE,
  show_total = FALSE,
  ...
)

# S3 method for class 'trx_df'
autotable(
  object,
  fontsize = 100,
  decimals = 1,
  colorful = TRUE,
  color_util = "RColorBrewer::GnBu",
  color_pct_of = "RColorBrewer::RdBu",
  rename_cols = rlang::list2(...),
  show_conf_int = FALSE,
  decimals_amt = 0,
  suffix_amt = FALSE,
  show_total = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class `exp_df` usually created by the function
  [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
  or an object of class `trx_df` created by the
  [`trx_stats()`](https://mattheaphy.github.io/actxps/reference/trx_stats.md)
  function.

- ...:

  Additional arguments passed to
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html).

- fontsize:

  Font size percentage multiplier.

- decimals:

  Number of decimals to display for percentages

- colorful:

  If `TRUE`, color will be added to the the observed termination rate
  and actual-to-expected columns for termination studies, and the
  utilization rate and "percentage of" columns for transaction studies.

- color_q_obs:

  Color palette used for the observed termination rate.

- color_ae\_:

  Color palette used for actual-to-expected rates.

- rename_cols:

  An optional list consisting of key-value pairs. This can be used to
  relabel columns on the output table. This parameter is most useful for
  renaming grouping variables that will appear under their original
  variable names if left unchanged. See
  [`gt::cols_label()`](https://gt.rstudio.com/reference/cols_label.html)
  for more information.

- show_conf_int:

  If `TRUE` confidence intervals will be displayed assuming they are
  available on `object`.

- show_cred_adj:

  If `TRUE` credibility-weighted termination rates will be displayed
  assuming they are available on `object`.

- decimals_amt:

  Number of decimals to display for amount columns (number of claims,
  claim amounts, exposures, transaction counts, total transactions, and
  average transactions)

- suffix_amt:

  This argument has the same meaning as the `suffixing` argument in
  [`gt::fmt_number()`](https://gt.rstudio.com/reference/fmt_number.html)
  for amount columns. If `FALSE` (the default), no scaling or suffixing
  are applied to amount columns. If `TRUE`, all amount columns are
  automatically scaled and suffixed by "K" (thousands), "M" (millions),
  "B" (billions), or "T" (trillions). See
  [`gt::fmt_number()`](https://gt.rstudio.com/reference/fmt_number.html)
  for more information.

- show_total:

  If `TRUE` the table will include grand total row(s).

- color_util:

  Color palette used for utilization rates.

- color_pct_of:

  Color palette used for "percentage of" columns.

## Value

a `gt` object

## Details

The `color_q_obs`, `color_ae_`, `color_util`, and `color_pct_of`
arguments must be strings referencing a discrete color palette available
in the `paletteer` package. Palettes must be in the form
"package::palette". For a full list of available palettes, see
[paletteer::palettes_d_names](https://emilhvitfeldt.github.io/paletteer/reference/palettes_d_names.html).

## Examples

``` r

if (interactive()) {
  study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
  expected_table <- c(seq(0.005, 0.03, length.out = 10), 0.2, 0.15, rep(0.05, 3))

  study_py <- study_py |>
    mutate(expected_1 = expected_table[pol_yr],
           expected_2 = ifelse(inc_guar, 0.015, 0.03)) |>
    add_transactions(withdrawals) |>
    left_join(account_vals, by = c("pol_num", "pol_date_yr"))

  exp_res <- study_py |> group_by(pol_yr) |>
    exp_stats(expected = c("expected_1", "expected_2"), credibility = TRUE,
              conf_int = TRUE)
  autotable(exp_res)

  trx_res <- study_py |> group_by(pol_yr) |>
    trx_stats(percent_of = "av_anniv", conf_int = TRUE)
  autotable(trx_res)
}
```
