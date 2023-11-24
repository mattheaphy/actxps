#' Aggregate simulated annuity data
#'
#' A pre-aggregated version of surrender and withdrawal experience from the
#' simulated data sets `census_dat`, `withdrawals`, and `account_vals`. This
#' data is theoretical only and does not represent the experience on any
#' specific product.
#'
#' @format A data frame containing summarized experience study results grouped
#' by policy year, income guarantee presence, tax-qualified status, and product.
#'
#' @details
#'
#' \describe{
#'   \item{pol_yr}{Policy year}
#'   \item{inc_guar}{Indicates whether the policy was issued with an income guarantee}
#'   \item{qual}{Indicates whether the policy was purchased with tax-qualified funds}
#'   \item{product}{Product: a, b, or c}
#'   \item{exposure_n}{Sum of policy year exposures by count}
#'   \item{claims_n}{Sum of claim counts}
#'   \item{wd}{Sum of partial withdrawal transactions}
#'   \item{av}{Sum of account value}
#'   \item{exposure_amt}{Sum of policy year exposures weighted by account value}
#'   \item{claims_amt}{Sum of claims weighted by account value}
#'   \item{av_sq}{Sum of squared account values}
#'   \item{n}{Sum of records}
#' }
#' @seealso [census_dat]
#' @name agg_sim_data

NULL
#' @rdname agg_sim_data
"agg_sim_dat"
