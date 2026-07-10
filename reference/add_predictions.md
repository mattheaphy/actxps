# Add predictions to a data frame

Attach predicted values from a model to a data frame with exposure-level
records.

## Usage

``` r
add_predictions(.data, model, ..., col_expected = NULL)
```

## Arguments

- .data:

  A data frame, preferably with the class `exposed_df`

- model:

  A model object that has an S3 method for
  [`predict()`](https://rdrr.io/r/stats/predict.html)

- ...:

  Additional arguments passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html)

- col_expected:

  `NULL` or a character vector containing column names for each value
  returned by [`predict()`](https://rdrr.io/r/stats/predict.html)

## Value

A data frame or `exposed_df` object with one of more new columns
containing predictions.

## Details

This function attaches predictions from a model to a data frame that
preferably has the class `exposed_df`. The `model` argument must be a
model object that has an S3 method for the
[`predict()`](https://rdrr.io/r/stats/predict.html) function. This
method must have new data for predictions as the second argument.

The `col_expected` argument is optional.

- If `NULL`, names from the result of
  [`predict()`](https://rdrr.io/r/stats/predict.html) will be used. If
  there are no names, a default name of "expected" is assumed. In the
  event that [`predict()`](https://rdrr.io/r/stats/predict.html) returns
  multiple values, the default name will be suffixed by "\_x", where x =
  1 to the number of values returned.

- If a value is passed, it must be a character vector of same length as
  the result of [`predict()`](https://rdrr.io/r/stats/predict.html)

## Examples

``` r
expo <- expose_py(census_dat, "2019-12-31") |>
  mutate(surrender = status == "Surrender")
mod <- glm(surrender ~ inc_guar + pol_yr, expo, family = 'binomial')
add_predictions(expo, mod, type = 'response')
#> 
#> ── Exposure data ──
#> 
#> • Exposure type: policy_year
#> • Target status:
#> • Study range: 1900-01-01 to 2019-12-31
#> 
#> # A tibble: 141,252 × 17
#>    pol_num status issue_date inc_guar qual    age product gender wd_age premium
#>  *   <int> <fct>  <date>     <lgl>    <lgl> <int> <fct>   <fct>   <int>   <dbl>
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
#> # ℹ 7 more variables: term_date <date>, pol_yr <int>, pol_date_yr <date>,
#> #   pol_date_yr_end <date>, exposure <dbl>, surrender <lgl>, expected <dbl>
```
