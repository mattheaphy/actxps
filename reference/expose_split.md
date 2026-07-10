# Split calendar exposures by policy year

Split calendar period exposures that cross a policy anniversary into a
pre-anniversary record and a post-anniversary record.

After splitting the data, the resulting data frame will contain both
calendar exposures and policy year exposures. These columns will be
named `exposure_cal` and `exposure_pol`, respectively. Calendar
exposures will be in the original units passed to `expose_split()`.
Policy exposures will always be expressed in years.

After splitting exposures, downstream functions like
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
and
[`exp_shiny()`](https://mattheaphy.github.io/actxps/reference/exp_shiny.md)
will require clarification as to which exposure basis should be used to
summarize results.

`is_split_exposed_df()` will return `TRUE` if `x` is a
`split_exposed_df` object.

## Usage

``` r
expose_split(.data)

is_split_exposed_df(x)
```

## Arguments

- .data:

  An `exposed_df` object with calendar period exposures.

- x:

  Any object

## Value

For `expose_split()`, a tibble with class `split_exposed_df`,
`exposed_df`, `tbl_df`, `tbl`, and `data.frame`. The results include all
columns in `.data` except that `exposure` has been renamed to
`exposure_cal`. Additional columns include:

- `exposure_pol` - policy year exposures

- `pol_yr` - policy year

For `is_split_exposed_df()`, a length-1 logical vector.

## Details

`.data` must be an `exposed_df` with calendar year, quarter, month, or
week exposure records. Calendar year exposures are created by the
functions
[`expose_cy()`](https://mattheaphy.github.io/actxps/reference/expose.md),
[`expose_cq()`](https://mattheaphy.github.io/actxps/reference/expose.md),
[`expose_cm()`](https://mattheaphy.github.io/actxps/reference/expose.md),
or
[`expose_cw()`](https://mattheaphy.github.io/actxps/reference/expose.md),
(or
[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
when `cal_expo = TRUE`).

## See also

[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
for information on creating exposure records from census data.

## Examples

``` r
toy_census |> expose_cy("2022-12-31") |> expose_split()
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: split_year
#> • Target status:
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 58 × 9
#>    pol_num status issue_date term_date cal_yr     cal_yr_end pol_yr
#>      <int> <fct>  <date>     <date>    <date>     <date>      <int>
#>  1       1 Active 2010-01-01 NA        2010-01-01 2010-12-31      1
#>  2       1 Active 2010-01-01 NA        2011-01-01 2011-12-31      2
#>  3       1 Active 2010-01-01 NA        2012-01-01 2012-12-31      3
#>  4       1 Active 2010-01-01 NA        2013-01-01 2013-12-31      4
#>  5       1 Active 2010-01-01 NA        2014-01-01 2014-12-31      5
#>  6       1 Active 2010-01-01 NA        2015-01-01 2015-12-31      6
#>  7       1 Active 2010-01-01 NA        2016-01-01 2016-12-31      7
#>  8       1 Active 2010-01-01 NA        2017-01-01 2017-12-31      8
#>  9       1 Active 2010-01-01 NA        2018-01-01 2018-12-31      9
#> 10       1 Active 2010-01-01 NA        2019-01-01 2019-12-31     10
#> # ℹ 48 more rows
#> # ℹ 2 more variables: exposure_cal <dbl>, exposure_pol <dbl>
```
