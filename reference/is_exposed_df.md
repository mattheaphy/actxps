# Exposed data frame helper functions

Test for and coerce to the `exposed_df` class.

## Usage

``` r
is_exposed_df(x)

as_exposed_df(
  x,
  end_date,
  start_date = as.Date("1900-01-01"),
  target_status = NULL,
  cal_expo = FALSE,
  expo_length = c("year", "quarter", "month", "week"),
  trx_types = NULL,
  col_pol_num,
  col_status,
  col_exposure,
  col_pol_per,
  cols_dates,
  col_trx_n_ = "trx_n_",
  col_trx_amt_ = "trx_amt_",
  default_status
)
```

## Arguments

- x:

  An object. For `as_exposed_df()`, `x` must be a data frame.

- end_date:

  Experience study end date

- start_date:

  Experience study start date. Default value = 1900-01-01.

- target_status:

  Character vector of target status values. Default value = `NULL`.

- cal_expo:

  Set to TRUE for calendar year exposures. Otherwise policy year
  exposures are assumed.

- expo_length:

  Exposure period length

- trx_types:

  Optional. Character vector containing unique transaction types that
  have been attached to `x`. For each value in `trx_types`,
  `as_exposed_df` requires that columns exist in `x` named `trx_n_{*}`
  and `trx_amt_{*}` containing transaction counts and amounts,
  respectively. The prefixes "trx_n\_" and "trx_amt\_" can be overridden
  using the `col_trx_n_` and `col_trx_amt_` arguments.

- col_pol_num:

  Optional. Name of the column in `x` containing the policy number. The
  assumed default is "pol_num".

- col_status:

  Optional. Name of the column in `x` containing the policy status. The
  assumed default is "status".

- col_exposure:

  Optional. Name of the column in `x` containing exposures. The assumed
  default is "exposure".

- col_pol_per:

  Optional. Name of the column in `x` containing policy exposure
  periods. Only necessary if `cal_expo` is `FALSE`. The assumed default
  is either "pol_yr", "pol_qtr", "pol_mth", or "pol_wk" depending on the
  value of `expo_length`.

- cols_dates:

  Optional. Names of the columns in `x` containing exposure start and
  end dates. Both date ranges are assumed to be exclusive. The assumed
  default is of the form *A*\_*B*. *A* is "cal" if `cal_expo` is `TRUE`
  or "pol" otherwise. *B* is either "yr", "qtr", "mth", or "wk"
  depending on the value of `expo_length`.

- col_trx_n\_:

  Optional. Prefix to use for columns containing transaction counts.

- col_trx_amt\_:

  Optional. Prefix to use for columns containing transaction amounts.

- default_status:

  Optional scalar character representing the default active status code.
  If not provided, the most common status is assumed.

## Value

For `is_exposed_df()`, a length-1 logical vector. For `as_exposed_df()`,
an `exposed_df` object.

## Details

`is_exposed_df()` will return `TRUE` if `x` is an `exposed_df` object.

`as_exposed_df()` will coerce a data frame to an `exposed_df` object if
that data frame has columns for policy numbers, statuses, exposures,
policy periods (for policy exposures only), and exposure start / end
dates. Optionally, if `x` has transaction counts and amounts by type,
these can be specified without calling
[`add_transactions()`](https://mattheaphy.github.io/actxps/reference/add_transactions.md).

## See also

[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
for information on how `exposed_df` objects are typically created from
census data.
