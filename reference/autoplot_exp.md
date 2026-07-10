# Plot experience study results

Plot experience study results

## Usage

``` r
# S3 method for class 'exp_df'
autoplot(
  object,
  ...,
  x = NULL,
  y = NULL,
  color = NULL,
  mapping,
  second_axis = FALSE,
  second_y = NULL,
  scales = "fixed",
  geoms = c("lines", "bars", "points"),
  y_labels = scales::label_percent(accuracy = 0.1),
  second_y_labels = scales::label_comma(accuracy = 1),
  y_log10 = FALSE,
  conf_int_bars = FALSE
)

# S3 method for class 'trx_df'
autoplot(
  object,
  ...,
  x = NULL,
  y = NULL,
  color = NULL,
  mapping,
  second_axis = FALSE,
  second_y = NULL,
  scales = "fixed",
  geoms = c("lines", "bars", "points"),
  y_labels = scales::label_percent(accuracy = 0.1),
  second_y_labels = scales::label_comma(accuracy = 1),
  y_log10 = FALSE,
  conf_int_bars = FALSE
)
```

## Arguments

- object:

  An object of class `exp_df` created by the function
  [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
  or an object of class `trx_df` created by the function
  [`trx_stats()`](https://mattheaphy.github.io/actxps/reference/trx_stats.md).

- ...:

  Faceting variables passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- x:

  An unquoted column name in `object` or expression to use as the `x`
  variable.

- y:

  An unquoted column name in `object` or expression to use as the `y`
  variable. If unspecified, `y` will default to the observed termination
  rate (`q_obs`) for `exp_df` objects and the observed utilization rate
  (`trx_util`) for `trx_df` objects.

- color:

  An unquoted column name in `object` or expression to use as the
  `color` and `fill` variables.

- mapping:

  Aesthetic mapping passed to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
  NOTE: If `mapping` is supplied, the `x`, `y`, and `color` arguments
  will be ignored.

- second_axis:

  Logical. If `TRUE`, the variable specified by `second_y` (default =
  exposure) is plotted on a second y-axis using an area geometry.

- second_y:

  An unquoted column name in `object` to use as the `y` variable on the
  second y-axis. If unspecified, this will default to `exposure`.

- scales:

  The `scales` argument passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- geoms:

  Type of geometry. If "lines" is passed, the plot will display lines
  and points. If "bars", the plot will display bars. If "points", the
  plot will display points only.

- y_labels:

  Label function passed to
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).

- second_y_labels:

  Same as `y_labels`, but for the second y-axis.

- y_log10:

  If `TRUE`, the y-axes are plotted on a log-10 scale.

- conf_int_bars:

  If `TRUE`, confidence interval error bars are included in the plot.
  For `exp_df` objects, this option is available for termination rates
  and actual-to-expected ratios. For `trx_df` objects, this option is
  available for utilization rates and any `pct_of` columns.

## Value

a `ggplot` object

## Details

If no aesthetic map is supplied, the plot will use the first grouping
variable in `object` on the x axis and `q_obs` on the y axis. In
addition, the second grouping variable in `object` will be used for
color and fill.

If no faceting variables are supplied, the plot will use grouping
variables 3 and up as facets. These variables are passed into
[`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
Specific to `trx_df` objects, transaction type (`trx_type`) will also be
added as a faceting variable.

## See also

[`plot_termination_rates()`](https://mattheaphy.github.io/actxps/reference/plot_special.md),
[`plot_actual_to_expected()`](https://mattheaphy.github.io/actxps/reference/plot_special.md)

## Examples

``` r

study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")

study_py <- study_py |>
  add_transactions(withdrawals)

exp_res <- study_py |> group_by(pol_yr) |> exp_stats()
autoplot(exp_res)


trx_res <- study_py |> group_by(pol_yr) |> trx_stats()
autoplot(trx_res)

```
