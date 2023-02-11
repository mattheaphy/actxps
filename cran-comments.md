## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

This is an update of an existing package in response to problems identified by
CRAN. https://cran.r-project.org/web/checks/check_results_actxps.html.


### Problem 1

Warning - ‘::’ or ‘:::’ import not declared from: ‘stringr’

**Response** - There was a usage of a function from stringr (`str_subset`) 
in a default argument to one of actxps's functions (`exp_shiny`). I replaced 
this function with a base R function call. There are no changes DESCRIPTION.

### Problem 2

Namespace in Imports field not imported from: ‘RColorBrewer’
All declared Imports should be used.

**Response** - I removed RColorBrewer from Imports to Suggests in the
DESCRIPTION file.
