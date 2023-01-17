#' Simulated census data
#'
#' Simulated census data for a theoretical deferred annuity product with
#' an optional guaranteed income rider. This data is theoretical only and
#' does not represent the experience on any specific product.
#'
#' @format A data frame with 20,000 rows and 11 columns:
#' \describe{
#'   \item{pol_num}{policy number}
#'   \item{status}{policy status: Active, Surrender, or Death}
#'   \item{issue_date}{issue date}
#'   \item{inc_guar}{indicates whether the policy was issued with an income guarantee}
#'   \item{qual}{indicates whether the policy was purchased with tax-qualified funds}
#'   \item{age}{issue age}
#'   \item{product}{product: a, b, or c}
#'   \item{gender}{M (Male) or F (Female)}
#'   \item{wd_age}{Age that withdrawals commence}
#'   \item{pol_val}{Policy value}
#'   \item{term_date}{termination date upon death or surrender}
#' }
"census_dat"
