# actxps 1.0.0

# actxps 0.2.1

`expose()` functions now include a new column for period end dates.

Fixed issues with `expose()` dropping records: 

- Handling of leap days / years
- Correction to date addition to always rollback dates to the last day of the 
month.

Fixed 2 R CMD check problems.

# actxps 0.2.0

First version submitted to CRAN.

Added `exp_shiny()` function.

# actxps 0.1.0

Added `step_expose()` recipe step function.

# actxps 0.0.9000

First developmental version
