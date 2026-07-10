# Create exposure records from census records

Convert a data frame of census-level records to exposure-level records.

## Usage

``` r
expose(
  .data,
  end_date,
  start_date = as.Date("1900-01-01"),
  target_status = NULL,
  cal_expo = FALSE,
  expo_length = c("year", "quarter", "month", "week"),
  col_pol_num = "pol_num",
  col_status = "status",
  col_issue_date = "issue_date",
  col_term_date = "term_date",
  default_status
)

expose_py(...)

expose_pq(...)

expose_pm(...)

expose_pw(...)

expose_cy(...)

expose_cq(...)

expose_cm(...)

expose_cw(...)
```

## Arguments

- .data:

  A data frame with census-level records

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

- col_pol_num:

  Name of the column in `.data` containing the policy number

- col_status:

  Name of the column in `.data` containing the policy status

- col_issue_date:

  Name of the column in `.data` containing the issue date

- col_term_date:

  Name of the column in `.data` containing the termination date

- default_status:

  Optional scalar character representing the default active status code.
  If not provided, the most common status is assumed.

- ...:

  Arguments passed to `expose()`

## Value

A tibble with class `exposed_df`, `tbl_df`, `tbl`, and `data.frame`. The
results include all existing columns in `.data` plus new columns for
exposures and observation periods. Observation periods include counters
for policy exposures, start dates, and end dates. Both start dates and
end dates are inclusive bounds.

For policy year exposures, two observation period columns are returned.
Columns beginning with (`pol_`) are integer policy periods. Columns
beginning with (`pol_date_`) are calendar dates representing anniversary
dates, monthiversary dates, etc.

## Details

Census-level data refers to a data set wherein there is one row per
unique policy. Exposure-level data expands census-level data such that
there is one record per policy per observation period. Observation
periods could be any meaningful period of time such as a policy year,
policy month, calendar year, calendar quarter, calendar month, etc.

`target_status` is used in the calculation of exposures. The annual
exposure method is applied, which allocates a full period of exposure
for any statuses in `target_status`. For all other statuses, new
entrants and exits are partially exposed based on the time elapsed in
the observation period. This method is consistent with the Balducci
Hypothesis, which assumes that the probability of termination is
proportionate to the time elapsed in the observation period. If the
annual exposure method isn't desired, `target_status` can be ignored. In
this case, partial exposures are always applied regardless of status.

`default_status` is used to indicate the default active status that
should be used when exposure records are created.

## Policy period and calendar period variations

The functions `expose_py()`, `expose_pq()`, `expose_pm()`,
`expose_pw()`, `expose_cy()`, `expose_cq()`, `expose_cm()`,
`expose_cw()` are convenience functions for specific implementations of
`expose()`. The two characters after the underscore describe the
exposure type and exposure period, respectively.

For exposures types:

- `p` refers to policy years

- `c` refers to calendar years

For exposure periods:

- `y` = years

- `q` = quarters

- `m` = months

- `w` = weeks

All columns containing dates must be in YYYY-MM-DD format.

## References

Atkinson and McGarry (2016). Experience Study Calculations.
<https://www.soa.org/49378a/globalassets/assets/files/research/experience-study-calculations.pdf>

## See also

[`expose_split()`](https://mattheaphy.github.io/actxps/reference/expose_split.md)
for information on splitting calendar year exposures by policy year.

## Examples

``` r
toy_census |> expose("2020-12-31")
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status:
#> • Study range: 1900-01-01 to 2020-12-31
#> 
#> # A tibble: 33 × 8
#>    pol_num status issue_date term_date pol_yr pol_date_yr pol_date_yr_end
#>      <int> <fct>  <date>     <date>     <int> <date>      <date>         
#>  1       1 Active 2010-01-01 NA             1 2010-01-01  2010-12-31     
#>  2       1 Active 2010-01-01 NA             2 2011-01-01  2011-12-31     
#>  3       1 Active 2010-01-01 NA             3 2012-01-01  2012-12-31     
#>  4       1 Active 2010-01-01 NA             4 2013-01-01  2013-12-31     
#>  5       1 Active 2010-01-01 NA             5 2014-01-01  2014-12-31     
#>  6       1 Active 2010-01-01 NA             6 2015-01-01  2015-12-31     
#>  7       1 Active 2010-01-01 NA             7 2016-01-01  2016-12-31     
#>  8       1 Active 2010-01-01 NA             8 2017-01-01  2017-12-31     
#>  9       1 Active 2010-01-01 NA             9 2018-01-01  2018-12-31     
#> 10       1 Active 2010-01-01 NA            10 2019-01-01  2019-12-31     
#> # ℹ 23 more rows
#> # ℹ 1 more variable: exposure <dbl>

census_dat |> expose_py("2019-12-31", target_status = "Surrender")
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 141,252 × 15
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
#> # ℹ 5 more variables: term_date <date>, pol_yr <int>, pol_date_yr <date>,
#> #   pol_date_yr_end <date>, exposure <dbl>
```
