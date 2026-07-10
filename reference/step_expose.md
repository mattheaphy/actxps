# Create exposure records in a `recipes` step

`step_expose()` creates a *specification* of a recipe step that will
convert a data frame of census-level records to exposure-level records.

## Usage

``` r
step_expose(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  end_date,
  start_date = as.Date("1900-01-01"),
  target_status = NULL,
  options = list(cal_expo = FALSE, expo_length = "year"),
  drop_pol_num = TRUE,
  skip = TRUE,
  id = recipes::rand_id("expose")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- end_date:

  Experience study end date

- start_date:

  Experience study start date. Default value = 1900-01-01.

- target_status:

  Character vector of target status values. Default value = `NULL`.

- options:

  A named list of additional arguments passed to
  [`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md).

- drop_pol_num:

  Whether the `pol_num` column produced by
  [`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)
  should be dropped. Defaults to `TRUE`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new expose step added to the
sequence of any existing operations. For the `tidy` method, a `tibble`
with the columns `exposure_type`, `target_status`, `start_date`, and
`end_date`.

## Details

Policy year exposures are calculated as a default. To switch to calendar
exposures or another exposure length, use pass the appropriate arguments
to the `options` parameter.

Policy numbers are dropped as a default whenever the recipe is baked.
This is done to prevent unintentional errors when the model formula
includes all variables (`y ~ .`). If policy numbers are required for any
reason (mixed effect models, identification, etc.), set `drop_pol_num`
to `FALSE`.

## See also

[`expose()`](https://mattheaphy.github.io/actxps/reference/expose.md)

## Examples

``` r

expo_rec <- recipes::recipe(status ~ ., toy_census) |>
  step_expose(end_date = "2022-12-31", target_status = "Surrender",
              options = list(expo_length = "month")) |>
  prep()

recipes::juice(expo_rec)
#> # A tibble: 416 × 7
#>    issue_date term_date status pol_mth pol_date_mth pol_date_mth_end exposure
#>    <date>     <date>    <fct>    <int> <date>       <date>              <dbl>
#>  1 2010-01-01 NA        Active       1 2010-01-01   2010-01-31              1
#>  2 2010-01-01 NA        Active       2 2010-02-01   2010-02-28              1
#>  3 2010-01-01 NA        Active       3 2010-03-01   2010-03-31              1
#>  4 2010-01-01 NA        Active       4 2010-04-01   2010-04-30              1
#>  5 2010-01-01 NA        Active       5 2010-05-01   2010-05-31              1
#>  6 2010-01-01 NA        Active       6 2010-06-01   2010-06-30              1
#>  7 2010-01-01 NA        Active       7 2010-07-01   2010-07-31              1
#>  8 2010-01-01 NA        Active       8 2010-08-01   2010-08-31              1
#>  9 2010-01-01 NA        Active       9 2010-09-01   2010-09-30              1
#> 10 2010-01-01 NA        Active      10 2010-10-01   2010-10-31              1
#> # ℹ 406 more rows
```
