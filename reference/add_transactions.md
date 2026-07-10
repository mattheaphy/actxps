# Add transactions to an experience study

Attach summarized transactions to a data frame with exposure-level
records.

## Usage

``` r
add_transactions(
  .data,
  trx_data,
  col_pol_num = "pol_num",
  col_trx_date = "trx_date",
  col_trx_type = "trx_type",
  col_trx_amt = "trx_amt"
)
```

## Arguments

- .data:

  A data frame with exposure-level records with the class `exposed_df`.
  Use
  [`as_exposed_df()`](https://mattheaphy.github.io/actxps/reference/is_exposed_df.md)
  to convert a data frame to an `exposed_df` object if necessary.

- trx_data:

  A data frame containing transactions details. This data frame must
  have columns for policy numbers, transaction dates, transaction types,
  and transaction amounts.

- col_pol_num:

  Name of the column in `trx_data` containing the policy number

- col_trx_date:

  Name of the column in `trx_data` containing the transaction date

- col_trx_type:

  Name of the column in `trx_data` containing the transaction type

- col_trx_amt:

  Name of the column in `trx_data` containing the transaction amount

## Value

An `exposed_df` object with two new columns containing transaction
counts and amounts for each transaction type found in `trx_data`. The
`exposed_df`'s `trx_types` attributes will be updated to include the new
transaction types found in `trx_data.`

## Details

This function attaches transactions to an `exposed_df` object.
Transactions are grouped and summarized such that the number of rows in
the `exposed_df` object does not change. Two columns are added to the
output for each transaction type. These columns have names of the
pattern `trx_n_{*}` (transaction counts) and `trx_amt_{*}`
(transaction_amounts).

Transactions are associated with the `exposed_df` object by matching
transactions dates with exposure dates ranges found in `exposed_df`.

All columns containing dates must be in YYYY-MM-DD format.

## See also

[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md),
[`as_exposed_df()`](https://mattheaphy.github.io/actxps/reference/is_exposed_df.md)

## Examples

``` r
expo <- expose_py(census_dat, "2019-12-31", target_status = "Surrender")
add_transactions(expo, withdrawals)
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> • Transaction types: Base and Rider
#> 
#> # A tibble: 141,252 × 19
#>    pol_num status issue_date inc_guar qual    age product gender wd_age premium
#>      <int> <fct>  <date>     <lgl>    <lgl> <int> <fct>   <fct>   <int>   <dbl>
#>  1       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77     370
#>  2       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77     370
#>  3       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77     370
#>  4       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77     370
#>  5       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77     370
#>  6       1 Active 2014-12-17 TRUE     FALSE    56 b       F          77     370
#>  7       2 Active 2007-09-24 FALSE    FALSE    71 a       F          71     708
#>  8       2 Active 2007-09-24 FALSE    FALSE    71 a       F          71     708
#>  9       2 Active 2007-09-24 FALSE    FALSE    71 a       F          71     708
#> 10       2 Active 2007-09-24 FALSE    FALSE    71 a       F          71     708
#> # ℹ 141,242 more rows
#> # ℹ 9 more variables: term_date <date>, pol_yr <int>, pol_date_yr <date>,
#> #   pol_date_yr_end <date>, exposure <dbl>, trx_amt_Base <dbl>,
#> #   trx_amt_Rider <dbl>, trx_n_Base <dbl>, trx_n_Rider <dbl>
```
