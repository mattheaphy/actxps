# Interactively explore experience data

Launch a Shiny application to interactively explore drivers of
experience.

`dat` must be an `exposed_df` object. An error will be thrown is any
other object type is passed. If `dat` has transactions attached, the app
will contain features for both termination and transaction studies.
Otherwise, the app will only support termination studies.

If nothing is passed to `predictors`, all columns names in `dat` will be
used (excluding the policy number, status, termination date, exposure,
transaction counts, and transaction amounts columns).

The `expected` argument is optional. As a default, any column names
containing the word "expected" are used.

## Usage

``` r
exp_shiny(
  dat,
  predictors = names(dat),
  expected = names(dat)[grepl("expected", names(dat))],
  distinct_max = 25L,
  title,
  credibility = TRUE,
  conf_level = 0.95,
  cred_r = 0.05,
  theme = "shiny",
  col_exposure = "exposure"
)
```

## Arguments

- dat:

  An `exposed_df` object.

- predictors:

  A character vector of independent variables in `dat` to include in the
  Shiny app.

- expected:

  A character vector of expected values in `dat` to include in the Shiny
  app.

- distinct_max:

  Maximum number of distinct values allowed for `predictors` to be
  included as "Color" and "Facets" grouping variables. This input
  prevents the drawing of overly complex plots. Default value = 25.

- title:

  Optional. Title of the Shiny app. If no title is provided, a
  descriptive title will be generated based on attributes of `dat`.

- credibility:

  If `TRUE`, the output will include partial credibility weights and
  credibility-weighted termination rates.

- conf_level:

  Confidence level used for the Limited Fluctuation credibility method
  and confidence intervals

- cred_r:

  Error tolerance under the Limited Fluctuation credibility method

- theme:

  The name of a theme passed to the `preset` argument of
  [`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html).
  Alternatively, a complete Bootstrap theme created using
  [`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html).

- col_exposure:

  Name of the column in `dat` containing exposures. This input is only
  used to clarify the exposure basis when `dat` is a `split_exposed_df`
  object. For more information on split exposures, see
  [`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md).

## Value

No return value. This function is called for the side effect of
launching a Shiny application.

## Layout

### Filters

The sidebar contains filtering widgets organized by data type for all
variables passed to the `predictors` argument.

At the top of the sidebar, information is shown on the percentage of
records remaining after applying filters. A description of all active
filters is also provided.

The top of the sidebar also includes a "play / pause" switch that can
pause reactivity of the application. Pausing is a good option when
multiple changes are made in quick succession, especially when the
underlying data set is large.

### Grouping variables

This box includes widgets to select grouping variables for summarizing
experience. The "x" widget determines the x variable in the plot output.
Similarly, the "Color" and "Facets" widgets are used for color and
facets. Multiple faceting variable selections are allowed. For the table
output, "x", "Color", and "Facets" have no particular meaning beyond the
order in which grouping variables are displayed.

### Study type

This box includes a toggle to switch between termination studies and
transaction studies (if available). Different options are available for
each study type.

#### Termination studies

The expected values checkboxes are used to activate and deactivate
expected values passed to the `expected` argument. These checkboxes also
include a a "control" item for expected values derived using control
variables. These boxes impact the table output directly and the
available "y" variables for the plot. The "Weight by" widget is used to
specify which column, if any, contains weights for summarizing
experience. The "Control variables" widget is used to specify which
columns, if any, are used as control variables ( see
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
for more information).

#### Transaction studies

The transaction types checkboxes are used to activate and deactivate
transaction types that appear in the plot and table outputs. The
available transaction types are taken from the `trx_types` attribute of
`dat`. In the plot output, transaction type will always appear as a
faceting variable. The "Transactions as % of" selector will expand the
list of available "y" variables for the plot and impact the table output
directly. Lastly, a toggle exists that allows for all transaction types
to be aggregated into a single group.

### Output

#### Plot

This tab includes a plot and various options for customization:

- y: y variable

- Geometry: plotting geometry

- Second y-axis: activate to enable a second y-axis

- Second axis y: y variable to plot on the second axis

- Add Smoothing: activate to plot loess curves

- Confidence intervals: If available, add error bars for confidence
  intervals around the selected y variable

- Free y Scales: activate to enable separate y scales in each plot

- Log y-axis: activate to plot all y-axes on a log-10 scale

The gear icon above the plot contains a pop-up menu that can be used to
change the size of the plot for exporting.

#### Table

This tab includes a data table.

The gear icon above the table contains a pop-up menu that can be used to
change the appearance of the table:

- The "Total row", "Confidence intervals", and "Credibility-weighted
  termination rates" switches add these outputs to the table. These
  values are hidden as a default to prevent over-crowding.

- The "Include color scales" switch disables or re-enables conditional
  color formatting.

- The "Decimals" slider controls the number of decimals displayed for
  percentage fields.

- The "Font size multiple" slider impacts the table's font size

#### Export

This pop-up menu contains options for saving summarized experience data,
the plot, or the table. Data is saved as a CSV file. The plot and table
are saved as png files.

## Examples

``` r

if (interactive()) {
  study_py <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
  expected_table <- c(seq(0.005, 0.03, length.out = 10),
                      0.2, 0.15, rep(0.05, 3))

  study_py <- study_py |>
    mutate(expected_1 = expected_table[pol_yr],
           expected_2 = ifelse(inc_guar, 0.015, 0.03)) |>
    add_transactions(withdrawals) |>
    left_join(account_vals, by = c("pol_num", "pol_date_yr"))

  exp_shiny(study_py)
}
```
