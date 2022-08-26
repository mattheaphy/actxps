#' 2012 Individual Annuity Mortality Table and Projection Scale G2
#'
#' Mortality rates and mortality improvement rates from the 2012 Individual
#' Annuity Mortality Basic (IAMB) Table and Project Scale G2.
#'
#' @format For the 2012 IAMB table, a data frame with 242 rows and 3 columns:
#' \describe{
#'   \item{age}{attained age}
#'   \item{qx}{mortality rate}
#'   \item{gender}{Female or Male}
#' }
#' @source
#'  - <https://mort.soa.org/>
#'  - <https://www.actuary.org/sites/default/files/files/publications/Payout_Annuity_Report_09-28-11.pdf>
#' @md
"qx_iamb"

#' @rdname qx_iamb
#' @format
#' For the Project Scale G2 table, a data frame with 242 rows and 3 columns:
#' \describe{
#'   \item{age}{attained age}
#'   \item{mi}{mortality improvement rate}
#'   \item{gender}{Female or Male}
#' }
"scale_g2"
