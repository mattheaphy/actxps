# Changelog

## actxps 1.6.1

CRAN release: 2025-09-24

- Fixed small compatibility issue with `gt` 1.1.0.
- Fixed issue with
  [`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md)
  and newer versions of the `bslib` package. Version 0.9.0 of `bslib` is
  now required to use
  [`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md).

## actxps 1.6.0

CRAN release: 2025-01-07

- New feature - control variables

  - [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
    has two new arguments: `control_vars` and `control_distinct_max`.
  - Control variables are used to estimate the impact of any grouping
    variables on observed experience after accounting for the impact of
    control variables. The idea here is that experience is first
    summarized across the control variables to derive a new expected
    values basis. This expected values basis is then used to calculate
    actual-to-expected ratios.
  - The `control_distinct_max` argument places an upper limit on the
    maximum number of unique values allowed in any control variable.
  - [`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md)
    now includes a control variable selection widget.

- New feature -
  [`autotable()`](https://mattheaphy.github.io/actxps/reference/autotable.md)
  methods for termination and transaction studies now include a
  `show_total` argument that appends grand total rows.

- Added the “cli” package to replace “rlang” for certain errors,
  warnings, and messages.

- Various small typo fixes.

## actxps 1.5.0

CRAN release: 2024-06-25

- [`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md)
  bug fixes:

  - [`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md)
    was updated to respect the values of `start_date` and `end_date`
    originally passed to the
    [`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
    function.
  - Future policy anniversary dates falling on February 29th leap days
    are now consistent with
    [`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  - New tests were added to verify that the sum of policy year exposures
    (`exposure_pol`) after calling
    [`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md)
    match exposures produced by
    [`expose_py()`](https://mattheaphy.github.io/actxps/reference/expose.md).

- The
  [`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  family of functions and
  [`add_transactions()`](https://mattheaphy.github.io/actxps/reference/add_transactions.md)
  now allow date columns to be passed as character vectors in YYYY-MM-DD
  format. Any character vectors are converted to dates
  behind-the-scenes, and any missing values will results in an error
  message.

- To improve the speed of date calculations, lubridate was replaced with
  the clock package. Lubridate is no longer included in Imports.

- **Breaking change** - The `pol_interval()` function is no longer
  exported. As part of the removal of lubridate, this function’s
  `dur_length` argument only accepts, “year”, “quarter”, “month”, or
  “week”.

- Shiny app layout updates

- Small vignette and documentation clean-ups

## actxps 1.4.0

CRAN release: 2023-11-26

- actxps now supports split exposures that divide calendar periods
  crossing policy anniversaries into pre-anniversary and
  post-anniversary records. The function
  [`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md)
  can convert any `exposed_df` object with calendar period exposures
  (yearly, quarterly, monthly, or weekly) into a `split_exposed_df`
  object. Split exposure data frames contain columns for exposures both
  on a calendar period and policy year basis.
- [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
  and
  [`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md)
  now require clarification as to which exposure basis should be used
  when passed a `split_exposed_df` object.
- All `expose_df` objects now contains a `default_status` attribute.
- [`autotable()`](https://mattheaphy.github.io/actxps/reference/autotable.md)
  functions now contain the arguments `decimals_amt` and `suffix_amt`.
  The former allows one to specify the number of decimals appearing
  after amount columns. The latter is used to automatically scale large
  numbers into by thousands, millions, billions, or trillions.
- Corrected an error in the calculation of the standard deviations of
  claims when
  [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
  is passed a weighting variable.
- Added a [`summary()`](https://rdrr.io/r/base/summary.html) method for
  `exposed_df` objects that calls
  [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md).
- The assumed default status in
  [`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  functions was changed from the first observed status to the most
  common status.
- The functions
  [`as_exp_df()`](https://mattheaphy.github.io/actxps/reference/as_exp_df.md)
  and
  [`as_trx_df()`](https://mattheaphy.github.io/actxps/reference/as_trx_df.md)
  were added to convert pre-aggregated experience studies to the
  `exp_df` and `trx_df` formats, respectively.
- `agg_sim_dat` - a new simulated data set of pre-aggregated experience
  was added for testing
  [`as_exp_df()`](https://mattheaphy.github.io/actxps/reference/as_exp_df.md)
  and
  [`as_trx_df()`](https://mattheaphy.github.io/actxps/reference/as_trx_df.md).
- [`is_exp_df()`](https://mattheaphy.github.io/actxps/reference/as_exp_df.md)
  and
  [`as_trx_df()`](https://mattheaphy.github.io/actxps/reference/as_trx_df.md)
  were added to test for the `exp_df` and `trx_df` classes.

## actxps 1.3.0

CRAN release: 2023-09-15

- A new `conf_int` argument was added to
  [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
  that creates confidence intervals around observed termination rates,
  credibility-weighted termination rates, and any actual-to-expected
  ratios.

- Similarly, `conf_int` was added to
  [`trx_stats()`](https://mattheaphy.github.io/actxps/reference/trx_stats.md)
  to create confidence intervals around utilization rates and any
  “percentage of” output columns. A `conf_level` argument was also added
  to this function.

- [`autoplot.exp_df()`](https://mattheaphy.github.io/actxps/reference/autoplot_exp.md)
  and
  [`autoplot.trx_df()`](https://mattheaphy.github.io/actxps/reference/autoplot_exp.md)
  now have a `conf_int_bars` argument that plots confidence intervals
  (if available) as error bars for the selected y-variable

- [`autoplot.exp_df()`](https://mattheaphy.github.io/actxps/reference/autoplot_exp.md)
  and
  [`autoplot.trx_df()`](https://mattheaphy.github.io/actxps/reference/autoplot_exp.md)
  can now create scatter plots if “points” is passed to the `geoms`
  argument.

- The second y-axis in the
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods was updated to use an area geometry instead of bars for
  discrete x-axis variables. In addition, when a log-10 y-scale is used,
  areas will always be positive quantities. Previously, it was observed
  that areas were drawn as negative values for y-values on the main
  scale less than 1.

- [`autotable.exp_df()`](https://mattheaphy.github.io/actxps/reference/autotable.md)
  and
  [`autotable.trx_df()`](https://mattheaphy.github.io/actxps/reference/autotable.md)
  were updated to format intervals.

- [`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md)
  updates

  - The layout and theme were updated in to align with changes made in
    shiny 1.7.5 and bslib 0.5.1
  - The function now includes the ability to customize the Bootstrap
    theme
  - Plots can now be re-sized and viewed in full screen mode
  - Tables contain new customization options and can be viewed in full
    screen mode
  - Tables and plots can be exported
  - Both the plots and tables optionally include confidence intervals
  - Tooltips were added throughout to explain the UI
  - A play / pause button was added to suspend interactivity on demand
  - A description of filters was added to the sidebar

- **Breaking change** - The confidence level argument `cred_p` was
  renamed to `conf_level`. This change was made because the confidence
  level is no longer strictly used for credibility calculations. This
  change impacts the functions
  [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
  and
  [`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md).

## actxps 1.2.0

CRAN release: 2023-08-09

- [`autoplot.exp_df()`](https://mattheaphy.github.io/actxps/reference/autoplot_exp.md)
  and
  [`autoplot.trx_df()`](https://mattheaphy.github.io/actxps/reference/autoplot_exp.md)
  now include new options for adding a second y-axis and plotting
  results on a log-10 scale. The second y-axis defaults to plotting
  exposures using an area geometry.
- New plotting functions were added to create common experience analysis
  plots that were not simple to create using
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods. These include
  [`plot_termination_rates()`](https://mattheaphy.github.io/actxps/reference/plot_special.md)
  and
  [`plot_actual_to_expected()`](https://mattheaphy.github.io/actxps/reference/plot_special.md)
  for termination studies and
  [`plot_utilization_rates()`](https://mattheaphy.github.io/actxps/reference/plot_special_trx.md)
  for transaction studies
- The
  [`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md)
  function received a handful of updates to accommodate new plotting
  functions and options. A small performance improvement was added in
  filtering logic as well. New options include a title input,
  credibility options taken from
  [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md),
- A new vignette was added on data visualization.
- The miscellaneous vignette was updated to include examples for
  [`add_predictions()`](https://mattheaphy.github.io/actxps/reference/add_predictions.md)
  and
  [`step_expose()`](https://mattheaphy.github.io/actxps/reference/step_expose.md).
- Examples were added to
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and
  [`autotable()`](https://mattheaphy.github.io/actxps/reference/autotable.md)
  methods
- Help documentation was added for the package itself
  ([`?actxps`](https://mattheaphy.github.io/actxps/reference/actxps-package.md))

## actxps 1.1.0

CRAN release: 2023-05-07

- New
  [`add_predictions()`](https://mattheaphy.github.io/actxps/reference/add_predictions.md)
  function that attaches one or more columns of model predictions to an
  `exposed_df` object or any other data frame.
- Small updates to
  [`add_transactions()`](https://mattheaphy.github.io/actxps/reference/add_transactions.md)
  and
  [`autotable()`](https://mattheaphy.github.io/actxps/reference/autotable.md)
  functions for compatibility with the dplyr 1.1.1 and gt 0.9.0.

## actxps 1.0.1

CRAN release: 2023-04-11

- Minor patch to a single test for compatibility with a future release
  of the recipes package.
- Various small documentation typo fixes

## actxps 1.0.0

CRAN release: 2023-03-04

The actxps package now contains support for transaction studies.

- The
  [`add_transactions()`](https://mattheaphy.github.io/actxps/reference/add_transactions.md)
  function adds transactions to `exposed_df` objects.
- The
  [`trx_stats()`](https://mattheaphy.github.io/actxps/reference/trx_stats.md)
  function summarizes transaction results and returns a `trx_df` object.
- New transaction summary (`trx_df`) S3 methods were added for for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and
  [`autotable()`](https://mattheaphy.github.io/actxps/reference/autotable.md).
- The
  [`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md)
  function was updated to support transaction studies.
- New sample data sets were added with transactions (`withdrawals`) and
  sample policy values (`account_vals`). These are meant to be paired
  with `census_dat`.
- Added
  [`vignette("transactions")`](https://mattheaphy.github.io/actxps/articles/transactions.md).

Other changes

- A new family of functions were added to calculate policy durations.
  These include `pol_interval()` (a generic version),
  [`pol_yr()`](https://mattheaphy.github.io/actxps/reference/pol_yr.md),
  [`pol_qtr()`](https://mattheaphy.github.io/actxps/reference/pol_yr.md),
  [`pol_mth()`](https://mattheaphy.github.io/actxps/reference/pol_yr.md),
  and
  [`pol_wk()`](https://mattheaphy.github.io/actxps/reference/pol_yr.md).
  See
  [`vignette("misc")`](https://mattheaphy.github.io/actxps/articles/misc.md).
- Several updates were made to the
  [`as_exposed_df()`](https://mattheaphy.github.io/actxps/reference/is_exposed_df.md)
  function to include stricter input requirements and helpful error
  messages.
- S3 methods for several dplyr functions were added for `exposed_df`
  objects to ensure class persistence, especially on grouped data
  frames. These include:
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  and
  [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`select()`](https://dplyr.tidyverse.org/reference/select.html),
  [`slice()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
  [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
  [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
  and
  [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html).
- The conditional formatting for color in
  [`autotable.exp_df()`](https://mattheaphy.github.io/actxps/reference/autotable.md)
  was updated to be consistent across like columns.
- The `pol_val` column in `census_dat` was renamed to `premium`.

## actxps 0.2.1

CRAN release: 2023-02-11

[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
functions now include a new column for period end dates.

Fixed issues with
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
dropping records:

- Handling of leap days / years
- Correction to date addition to always rollback dates to the last day
  of the month.

Fixed 2 R CMD check problems.

## actxps 0.2.0

CRAN release: 2022-08-31

First version submitted to CRAN.

Added
[`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md)
function.

## actxps 0.1.0

Added
[`step_expose()`](https://mattheaphy.github.io/actxps/reference/step_expose.md)
recipe step function.

## actxps 0.0.9000

First developmental version
