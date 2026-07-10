# Aggregate simulated annuity data

A pre-aggregated version of surrender and withdrawal experience from the
simulated data sets `census_dat`, `withdrawals`, and `account_vals`.
This data is theoretical only and does not represent the experience on
any specific product.

## Usage

``` r
agg_sim_dat
```

## Format

A data frame containing summarized experience study results grouped by
policy year, income guarantee presence, tax-qualified status, and
product.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 180
rows and 16 columns.

## Details

- pol_yr:

  Policy year

- inc_guar:

  Indicates whether the policy was issued with an income guarantee

- qual:

  Indicates whether the policy was purchased with tax-qualified funds

- product:

  Product: a, b, or c

- exposure_n:

  Sum of policy year exposures by count

- claims_n:

  Sum of claim counts

- av:

  Sum of account value

- exposure_amt:

  Sum of policy year exposures weighted by account value

- claims_amt:

  Sum of claims weighted by account value

- av_sq:

  Sum of squared account values

- n:

  Number of exposure records

- wd:

  Sum of partial withdrawal transactions

- wd_n:

  Count of partial withdrawal transactions

- wd_flag:

  Count of exposure records with partial withdrawal transactions

- wd_sq:

  Sum of squared partial withdrawal transactions

- av_w_wd:

  Sum of account value for exposure records with partial withdrawal
  transactions

## See also

[census_dat](https://mattheaphy.github.io/actxps/reference/sim_data.md)
