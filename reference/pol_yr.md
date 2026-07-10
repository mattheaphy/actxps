# Calculate policy duration

Given a vector of dates and a vector of issue dates, calculate policy
years, quarters, months, or weeks.

## Usage

``` r
pol_yr(x, issue_date)

pol_qtr(x, issue_date)

pol_mth(x, issue_date)

pol_wk(x, issue_date)
```

## Arguments

- x:

  A vector of dates

- issue_date:

  A vector of issue dates

## Value

An integer vector

## Details

These functions assume the first day of each policy year is the
anniversary date (or issue date in the first year). The last day of each
policy year is the day before the next anniversary date. Analogous rules
are used for policy quarters, policy months, and policy weeks.

## Examples

``` r
pol_yr(as.Date("2021-02-28") + 0:2, "2020-02-29")
#> [1] 1 2 2

pol_mth(as.Date("2021-02-28") + 0:2, "2020-02-29")
#> [1] 12 13 13
```
