#' Termination summary helper functions
#'
#' Convert aggregate termination experience studies to the `exp_df` class.
#'
#' `is_exp_df()` will return `TRUE` if `x` is an `exp_df` object.
#'
#' `as_exp_df()` will coerce a data frame to an `exp_df` object if that
#' data frame has columns for exposures and claims.
#'
#' `as_exp_df()` is most useful for working with aggregate summaries of
#' experience that were not created by actxps where individual policy
#' information is not available. After converting the data to the `exp_df`
#' class, [summary()] can be used to summarize data by any grouping variables,
#' and [autoplot()] and [autotable()] are available for reporting.
#'
#' If nothing is passed to `wt`, the data frame `x` must include columns
#' containing:
#'
#' - Exposures (`exposure`)
#' - Claim counts (`claims`)
#'
#' If `wt` is passed, the data must include columns containing:
#'
#' - Weighted exposures (`exposure`)
#' - Weighted claims (`claims`)
#' - Claim counts (`n_claims`)
#' - The raw sum of weights **NOT** multiplied by exposures
#' - Exposure record counts (`.weight_n`)
#' - The raw sum of squared weights (`.weight_sq`)
#'
#' The names in parentheses above are expected column names. If the data
#' frame passed to `as_exp_df()` uses different column names, these can be
#' specified using the `col_*` arguments.
#'
#' When a column name is passed to `wt`, the columns `.weight`, `.weight_n`,
#' and `.weight_sq` are used to calculate credibility and confidence intervals.
#' If credibility and confidence intervals aren't required, then it is not
#' necessary to pass anything to `wt`. The results of `as_exp_df()` and any
#' downstream summaries will still be weighted as long as the exposures and
#' claims are pre-weighted.
#'
#' `target_status`, `start_date`, and `end_date` are optional arguments that are
#' only used for printing the resulting `exp_df` object.
#'
#' @param x An object. For `as_exp_df()`, `x` must be a data frame.
#' @param expected A character vector containing column names in x with
#' expected values
#' @param wt Optional. Length 1 character vector. Name of the column in `x`
#' containing weights to use in the calculation of claims, exposures, partial
#' credibility, and confidence intervals.
#' @param col_claims Optional. Name of the column in `x` containing claims. The
#' assumed default is "claims".
#' @param col_exposure Optional. Name of the column in `x` containing exposures.
#' The assumed default is "exposure".
#' @param col_n_claims Optional and only used used when `wt` is passed. Name of
#' the column in `x` containing the number of claims.
#' @param col_weight_sq Optional and only used used when `wt` is passed. Name of
#' the column in `x` containing the sum of squared weights.
#' @param col_weight_n Optional and only used used when `wt` is passed. Name of
#' the column in `x` containing exposure record counts.
#' @param credibility If `TRUE`, future calls to [summary()] will include
#' partial credibility weights and credibility-weighted termination rates.
#' @param conf_level Confidence level used for the Limited Fluctuation
#' credibility method and confidence intervals
#' @param cred_r Error tolerance under the Limited Fluctuation credibility
#' method
#' @param conf_int If `TRUE`, future calls to [summary()] will include
#' confidence intervals around the observed termination rates and any
#' actual-to-expected ratios.
#' @inheritParams expose
#'
#' @return For `is_exp_df()`, a length-1 logical vector. For `as_exp_df()`,
#' an `exp_df` object.
#'
#' @seealso [exp_stats()] for information on how `exp_df` objects are typically
#' created from individual exposure records.
#'
#' @examples
#' # convert pre-aggregated experience into an exp_df object
#' dat <- as_exp_df(agg_sim_dat, col_exposure = "exposure_n",
#'                  col_claims = "claims_n",
#'                  target_status = "Surrender",
#'                  start_date = 2005, end_date = 2019,
#'                  conf_int = TRUE)
#' dat
#' is_exp_df(dat)
#'
#' # summary by policy year
#' summary(dat, pol_yr)
#'
#' # repeat the prior exercise on a weighted basis
#' dat_wt <- as_exp_df(agg_sim_dat, wt = "av",
#'                     col_exposure = "exposure_amt",
#'                     col_claims = "claims_amt",
#'                     col_n_claims = "claims_n",
#'                     col_weight_sq = "av_sq",
#'                     col_weight_n = "n",
#'                     target_status = "Surrender",
#'                     start_date = 2005, end_date = 2019,
#'                     conf_int = TRUE)
#' dat_wt
#'
#' # summary by policy year
#' summary(dat_wt, pol_yr)
#'
#'
#' @export
as_exp_df <- function(x, expected = NULL, wt = NULL,
                      col_claims, col_exposure,
                      col_n_claims, col_weight_sq, col_weight_n,
                      target_status = NULL,
                      start_date = as.Date("1900-01-01"), end_date = NULL,
                      credibility = FALSE,
                      conf_level = 0.95, cred_r = 0.05, conf_int = FALSE) {

  if (is_exp_df(x)) return(x)

  if (!is.data.frame(x)) {
    rlang::abort("`x` must be a data frame.")
  }

  # column name alignment
  if (!missing(col_exposure)) x <- x |> rename(exposure = {{col_exposure}})
  if (!missing(col_claims)) x <- x |> rename(claims = {{col_claims}})

  if (is.null(wt)) {
    req_names <- c("exposure", "claims")
  } else {
    req_names <- c("exposure", "claims", "n_claims", ".weight",
                   ".weight_sq", ".weight_n")
    if (!missing(col_n_claims)) x <- x |> rename(n_claims = {{col_n_claims}})
    x <- x |> rename(.weight = {{wt}})
    if (!missing(col_weight_sq)) x <- x |>
        rename(.weight_sq = {{col_weight_sq}})
    if (!missing(col_weight_n)) x <- x |> rename(.weight_n = {{col_weight_n}})
  }

  # check required columns
  verify_col_names(names(x), req_names)

  if (is.null(wt)) x$n_claims <- x$claims

  new_exp_df(x,
             .groups = list(),
             target_status = target_status,
             start_date = start_date,
             expected = expected,
             end_date = end_date,
             wt = wt,
             credibility = credibility,
             conf_level = conf_level, cred_r = cred_r,
             conf_int = conf_int,
             control_vars = NULL)

}

#' @export
#' @rdname as_exp_df
is_exp_df <- function(x) {
  inherits(x, "exp_df")
}
