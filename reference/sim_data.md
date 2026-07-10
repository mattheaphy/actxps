# Simulated annuity data

Simulated data for a theoretical deferred annuity product with an
optional guaranteed income rider. This data is theoretical only and does
not represent the experience on any specific product.

## Usage

``` r
census_dat

withdrawals

account_vals
```

## Format

Three data frames containing census records (`census_dat`), withdrawal
transactions (`withdrawals`), and historical account values
(`account_vals`).

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
20000 rows and 11 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
160130 rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
141252 rows and 3 columns.

## Census data (`census_dat`)

- pol_num:

  Policy number

- status:

  Policy status: Active, Surrender, or Death

- issue_date:

  Issue date

- inc_guar:

  Indicates whether the policy was issued with an income guarantee

- qual:

  Indicates whether the policy was purchased with tax-qualified funds

- age:

  Issue age

- product:

  Product: a, b, or c

- gender:

  M (Male) or F (Female)

- wd_age:

  Age that withdrawals commence

- premium:

  Single premium deposit

- term_date:

  Termination date upon death or surrender

## Withdrawal data (`withdrawals`)

- pol_num:

  Policy number

- trx_date:

  Withdrawal transaction date

- trx_type:

  Withdrawal transaction type, either Base or Rider

- trx_amt:

  Withdrawal transaction amount

## Account values data (`account_vals`)

- pol_num:

  Policy number

- pol_date_yr:

  Policy anniversary date (beginning of year)

- av_anniv:

  Account value on the policy anniversary date

## See also

census_dat
