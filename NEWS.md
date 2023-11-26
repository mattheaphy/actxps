# actxps 1.4.0

- actxps now supports split exposures that divide calendar periods crossing 
policy anniversaries into pre-anniversary and post-anniversary records. The 
function `expose_split()` can convert any `exposed_df` object with calendar 
period exposures (yearly, quarterly, monthly, or weekly) into a 
`split_exposed_df` object. Split exposure data frames contain columns for 
exposures both on a calendar period and policy year basis.
- `exp_stats()` and `exp_shiny()` now require clarification as to which exposure
basis should be used when passed a `split_exposed_df` object.
- All `expose_df` objects now contains a `default_status` attribute.
- `autotable()` functions now contain the arguments `decimals_amt` and 
`suffix_amt`. The former allows one to specify the number of decimals appearing
after amount columns. The latter is used to automatically scale large numbers 
into by thousands, millions, billions, or trillions.
- Corrected an error in the calculation of the standard deviations of claims 
when `exp_stats()` is passed a weighting variable.
- Added a `summary()` method for `exposed_df` objects that calls `exp_stats()`.
- The assumed default status in `expose()` functions was changed from the first
observed status to the most common status.
- The functions `as_exp_df()` and `as_trx_df()` were added to convert 
pre-aggregated experience studies to the `exp_df` and `trx_df` formats, 
respectively.
- `agg_sim_dat` - a new simulated data set of pre-aggregated experience was 
added for testing `as_exp_df()` and `as_trx_df()`.
- `is_exp_df()` and `as_trx_df()` were added to test for the `exp_df` and 
`trx_df` classes.

# actxps 1.3.0

- A new `conf_int` argument was added to `exp_stats()` that creates confidence 
intervals around observed termination rates, credibility-weighted termination 
rates, and any actual-to-expected ratios.
- Similarly, `conf_int` was added to `trx_stats()` to create confidence intervals around utilization rates and any "percentage of" output columns. A `conf_level`
argument was also added to this function.
- `autoplot.exp_df()` and `autoplot.trx_df()` now have a `conf_int_bars` argument 
that plots confidence intervals (if available) as error bars for the selected 
y-variable
- `autoplot.exp_df()` and `autoplot.trx_df()` can now create scatter plots if
"points" is passed to the `geoms` argument.
- The second y-axis in the `autoplot()` methods was updated to use an area 
geometry instead of bars for discrete x-axis variables. In addition, when a 
log-10 y-scale is used, areas will always be positive quantities. Previously,
it was observed that areas were drawn as negative values for y-values on the 
main scale less than 1.
- `autotable.exp_df()` and `autotable.trx_df()` were updated to format 
intervals.
- `exp_shiny()` updates

  - The layout and theme were updated in to align with changes made in shiny 
    1.7.5 and bslib 0.5.1
  - The function now includes the ability to customize the Bootstrap theme
  - Plots can now be re-sized and viewed in full screen mode
  - Tables contain new customization options and can be viewed in full screen mode
  - Tables and plots can be exported
  - Both the plots and tables optionally include confidence intervals
  - Tooltips were added throughout to explain the UI
  - A play / pause button was added to suspend interactivity on demand
  - A description of filters was added to the sidebar

- **Breaking change** - The confidence level argument `cred_p` was renamed to 
`conf_level`. This change was made because the confidence level is no longer 
strictly used for credibility calculations. This change impacts the functions
`exp_stats()` and `exp_shiny()`.

# actxps 1.2.0

- `autoplot.exp_df()` and `autoplot.trx_df()` now include new options for adding a second y-axis and plotting results on a log-10 scale. The second y-axis defaults to plotting exposures using an area geometry.
- New plotting functions were added to create common experience analysis plots that were not simple to create using `autoplot()` methods. These include `plot_termination_rates()` and `plot_actual_to_expected()` for termination studies and `plot_utilization_rates()` for transaction studies
- The `exp_shiny()` function received a handful of updates to accommodate new plotting functions and options. A small performance improvement was added in filtering logic as well. New options include a title input, credibility options taken from `exp_stats()`, 
- A new vignette was added on data visualization.
- The miscellaneous vignette was updated to include examples for `add_predictions()` and `step_expose()`.
- Examples were added to `autoplot()` and `autotable()` methods
- Help documentation was added for the package itself (`?actxps`)

# actxps 1.1.0

- New `add_predictions()` function that attaches one or more columns of model predictions to an `exposed_df` object or any other data frame.
- Small updates to `add_transactions()` and `autotable()` functions for compatibility with the dplyr 1.1.1 and gt 0.9.0.

# actxps 1.0.1

- Minor patch to a single test for compatibility with a future release of the recipes package.
- Various small documentation typo fixes

# actxps 1.0.0

The actxps package now contains support for transaction studies.

- The `add_transactions()` function adds transactions to `exposed_df` objects.
- The `trx_stats()` function summarizes transaction results and returns a 
`trx_df` object.
- New transaction summary (`trx_df`) S3 methods were added for for `autoplot()` 
and `autotable()`.
- The `exp_shiny()` function was updated to support transaction studies.
- New sample data sets were added with transactions (`withdrawals`) and 
sample policy values (`account_vals`). These are meant to be paired with 
`census_dat`.
- Added `vignette("transactions")`.

Other changes

- A new family of functions were added to calculate policy durations. These
include `pol_interval()` (a generic version), `pol_yr()`, `pol_qtr()`, 
`pol_mth()`, and `pol_wk()`. See `vignette("misc")`.
- Several updates were made to the `as_exposed_df()` function to include 
stricter input requirements and helpful error messages.
- S3 methods for several dplyr functions were added for `exposed_df` objects to 
ensure class persistence, especially on grouped data frames. These include:
`group_by()` and `ungroup()`, `filter()`, `arrange()`, `mutate()`, `select()`, 
`slice()`, `rename()`, `relocate()`, `left_join()`, `right_join()`, 
`inner_join()`, `full_join()`, `semi_join()`, and `anti_join()`.
- The conditional formatting for color in `autotable.exp_df()` was updated to 
be consistent across like columns.
- The `pol_val` column in `census_dat` was renamed to `premium`.

# actxps 0.2.1

`expose()` functions now include a new column for period end dates.

Fixed issues with `expose()` dropping records: 

- Handling of leap days / years
- Correction to date addition to always rollback dates to the last day of the 
month.

Fixed 2 R CMD check problems.

# actxps 0.2.0

First version submitted to CRAN.

Added `exp_shiny()` function.

# actxps 0.1.0

Added `step_expose()` recipe step function.

# actxps 0.0.9000

First developmental version
