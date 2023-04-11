# actxps 1.0.1

- Minor patch to a single test for compatibility with a future relase of the recipes package.
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
