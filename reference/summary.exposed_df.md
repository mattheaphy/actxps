# Summarize experience study records

Create a summary data frame of termination experience for a given target
status.

## Usage

``` r
# S3 method for class 'exposed_df'
summary(object, ...)
```

## Arguments

- object:

  A data frame with exposure-level records

- ...:

  Additional arguments passed to
  [`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)

## Value

A tibble with class `exp_df`, `tbl_df`, `tbl`, and `data.frame`.

## Details

Calling [`summary()`](https://rdrr.io/r/base/summary.html) on an
`exposed_df` object will summarize results using
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md).
See
[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)
for more information.

## See also

[`exp_stats()`](https://mattheaphy.github.io/actxps/reference/exp_stats.md)

## Examples

``` r
toy_census |> expose("2022-12-31", target_status = "Surrender") |>
    summary()
#> 
#> ── Experience study results ──
#> 
#> • Target status: Surrender
#> • Study range: 1900-01-01 to 2022-12-31
#> 
#> # A tibble: 1 × 4
#>   n_claims claims exposure  q_obs
#>      <int>  <int>    <dbl>  <dbl>
#> 1        1      1     35.3 0.0283
```
