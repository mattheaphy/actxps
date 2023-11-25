#' Simulated annuity data
#'
#' Simulated data for a theoretical deferred annuity product with
#' an optional guaranteed income rider. This data is theoretical only and
#' does not represent the experience on any specific product.
#'
#' @format Three data frames containing census records (`census_dat`),
#' withdrawal transactions (`withdrawals`), and historical account values
#' (`account_vals`).
#'
#' @details
#'
#' # Census data (`census_dat`)
#'
#' \describe{
#'   \item{pol_num}{Policy number}
#'   \item{status}{Policy status: Active, Surrender, or Death}
#'   \item{issue_date}{Issue date}
#'   \item{inc_guar}{Indicates whether the policy was issued with an income guarantee}
#'   \item{qual}{Indicates whether the policy was purchased with tax-qualified funds}
#'   \item{age}{Issue age}
#'   \item{product}{Product: a, b, or c}
#'   \item{gender}{M (Male) or F (Female)}
#'   \item{wd_age}{Age that withdrawals commence}
#'   \item{premium}{Single premium deposit}
#'   \item{term_date}{Termination date upon death or surrender}
#' }
#'
#' # Withdrawal data (`withdrawals`)
#'
#' \describe{
#'   \item{pol_num}{Policy number}
#'   \item{trx_date}{Withdrawal transaction date}
#'   \item{trx_type}{Withdrawal transaction type, either Base or Rider}
#'   \item{trx_amt}{Withdrawal transaction amount}
#' }
#'
#' # Account values data (`account_vals`)
#'
#' \describe{
#'   \item{pol_num}{Policy number}
#'   \item{pol_date_yr}{Policy anniversary date (beginning of year)}
#'   \item{av_anniv}{Account value on the policy anniversary date}
#' }
#'
#' @seealso [census_dat]
#' @name sim_data

NULL
#' @rdname sim_data
"census_dat"
#' @rdname sim_data
"withdrawals"
#' @rdname sim_data
"account_vals"
