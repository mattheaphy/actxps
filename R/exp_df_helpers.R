#' Termination summary helper functions
#'
#' Test for and coerce to the `exp_df` class.
#'
#' `is_exp_df()` will return `TRUE` if `x` is an `exp_df` object.
#'
#' `as_exp_df()` will coerce a data frame to an `exp_df` object if that
#' data frame has columns for exposures and claims.
#'
#' `as_exp_df()` is most useful for converting existing aggregate summaries of
#' experience where individual policy information is not available.
#'
#' @param x An object. For `as_exp_df()`, `x` must be a data frame.
#' @param expected A character vector containing column names in x with
#' expected values
#' @param col_claims Optional. Name of the column in `x` containing claims. The
#' assumed default is "claims".
#' @param col_exposure Optional. Name of the column in `x` containing exposures.
#' The assumed default is "exposure".
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
#' @seealso exp_stats
#'
#' @export
as_exp_df <- function(x, expected = NULL, col_claims, col_exposure,
                      target_status = NULL,
                      start_date = NULL, end_date = NULL,
                      credibility = FALSE,
                      conf_level = 0.95, cred_r = 0.05, conf_int = FALSE) {

  if (is_exp_df(x)) return(x)

  if (!is.data.frame(x)) {
    rlang::abort("`x` must be a data frame.")
  }

  # column name alignment
  if (!missing(col_exposure)) x <- x |> rename(exposure = {{col_exposure}})
  if (!missing(col_claims)) x <- x |> rename(claims = {{col_claims}})

  # check required columns
  # pol_num, status, exposure, 2 date cols, policy period (policy expo only)
  req_names <- c("exposure", "claims")
  verify_col_names(names(x), req_names)

  x <- x |> mutate(n_claims = claims)

  new_exp_df(x,
             .groups = list(),
             target_status = target_status,
             start_date = start_date,
             expected = expected,
             end_date = end_date,
             wt = NULL,
             credibility = credibility,
             conf_level = conf_level, cred_r = cred_r,
             conf_int = conf_int)

}

#' @export
#' @rdname as_exp_df
is_exp_df <- function(x) {
  inherits(x, "exp_df")
}
