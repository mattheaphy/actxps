# start-up message template
# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage("Message here")
# }

# start-up template for usage of options
# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.actxps <- list(
#     actxps.option = value,
#   )
#   toset <- !(names(op.actxps) %in% names(op))
#   if(any(toset)) options(op.actxps[toset])
#
#   invisible()
# }
