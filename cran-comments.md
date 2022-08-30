## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

This is the submission of a new package.

## Resubmission

Thank you for the quick feedback on the last submission. 
All comments have been addressed; see below for responses.

1. "If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")""

The description file has been updated accordingly with citations in the 
description field.

2. Please always write package names, software names and API (application
programming interface) names in single quotes in title and description.
e.g: --> 'tidyverse'

This has been corrected.

3. Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
  Missing Rd-tags:
      step_expose.Rd: \value
      
`step_expose.Rd` has been corrected and now includes a `value` field.

4. \dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or replace
\dontrun{} with \donttest{}.

This has been corrected. There is no usage of \dontrun or \donttest. One
previous usage of \dontrun was wrapped with `if(interactive())` which seems
more appropriate.

5. Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir().

I had some difficultly locating the source of this comment. I suspect that it's
within the function `exp_shiny()`. This function launches a shiny web 
application that allows the user to download data to their computer. The 
download does not write by default and requires user input. In addition, the 
download directory should not default to the user's home filespace. 
The directory should be dictated by the user's web browser's default directory
for file downloads. To fully ensure compliance, I updated the function to 
route downloads to a `tempdir()` path.
