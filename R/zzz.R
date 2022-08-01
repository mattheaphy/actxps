# start-up message template
# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage("Message here")
# }

# start-up template for usage of options
# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.xps <- list(
#     xps.option = value,
#   )
#   toset <- !(names(op.xps) %in% names(op))
#   if(any(toset)) options(op.xps[toset])
#
#   invisible()
# }
