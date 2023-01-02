#' Summarize experience study records
#'
#' @description Create a summary data frame of experience for a given target
#' status.
#'
#' @details If `.data` is grouped, the resulting data frame will contain
#' one row per group.
#'
#' If `target_status` isn't provided, [exp_stats()] will use the same
#' target status from `.data` if it has the class `exposed_df`.
#' Otherwise, `.data` is not an `exposed_df` object, all status
#' values except the first level will be assumed. This will produce a
#' warning message.
#'
#' # Expected values
#'
#' The `expected` argument is optional. If provided, this argument must
#' be a character vector with values corresponding to columns in `.data`
#' containing expected experience. More than one expected basis can be provided.
#'
#' # Credibility
#'
#' If `credibility` is set to `TRUE`, the output will contain a
#' `credibility` column equal to the partial credibility estimate under
#' the Limited Fluctuation credibility method (also known as Classical
#' Credibility) assuming a binomial distribution of claims.
#'
#' # `summary()` Method
#'
#' Applying `summary()` to a `exp_df` object will re-summarize the
#' data while retaining any grouping variables passed to the "dots"
#' (`...`).
#'
#' @param .data a data frame with exposure-level records, ideally of type `exposed_df`
#' @param target_status a character vector of target status values
#' @param expected a character vector containing column names in `.data`
#' with expected values
#' @param col_exposure name of the column in `.data` containing exposures
#' @param col_status name of the column in `.data` containing the policy status
#' @param wt Optional. Length 1 character vector. Name of the column in
#' `.data` containing weights to use in the calculation of claims,
#' exposures, and partial credibility.
#' @param credibility whether the output should include partial credibility
#' weights and credibility-weighted decrement rates.
#' @param cred_p confidence level under the Limited Fluctuation credibility method
#' @param cred_r error tolerance under the Limited Fluctuation credibility
#' method
#' @param object an `exp_df` object
#' @param ... groups to retain after `summary()` is called
#'
#' @return A tibble with class `exp_df`, `tbl_df`, `tbl`,
#' and `data.frame`. The results include columns for any grouping
#' variables, claims, exposures, and observed decrement rates (`q_obs`).
#' If any values are passed to `expected`, additional columns will be
#' added for expected decrements and actual-to-expected ratios. If
#' `credibility` is set to `TRUE`, additional columns are added
#' for partial credibility and credibility-weighted decrement rates
#' (assuming values are passed to `expected`).
#'
#' @examples
#' toy_census |> expose("2020-12-31", target_status = "Surrender") |>
#'     exp_stats()
#'
#' exp_res <- census_dat |>
#'            expose("2019-12-31", target_status = "Surrender") |>
#'            dplyr::group_by(pol_yr, inc_guar) |>
#'            exp_stats()
#'
#' exp_res
#' summary(exp_res)
#' summary(exp_res, inc_guar)
#'
#' @references Herzog, Thomas (1999). Introduction to Credibility Theory
#'
#' @export
exp_stats <- function(.data, target_status = attr(.data, "target_status"),
                      expected, col_exposure = "exposure",
                      col_status = "status",
                      wt = NULL,
                      credibility = FALSE,
                      cred_p = 0.95, cred_r = 0.05) {

  .groups <- dplyr::groups(.data)
  start_date <- attr(.data, "start_date")
  end_date <- attr(.data, "end_date")

  if (is.null(target_status)) {
    target_status <- levels(.data$status)[-1]
    rlang::warn(c(x = "No target status was provided.",
                  i = glue::glue("{paste(target_status, collapse = ', ')} was assumed.")))
  }

  if (length(wt) > 1) {
    rlang::abort(c(x = glue::glue("Only 1 column can be passed to wt. You supplied {length(wt)} values.")))
  }

  res <- .data |>
    dplyr::rename(exposure = {{col_exposure}},
                  status = {{col_status}}) |>
    dplyr::mutate(n_claims = status %in% target_status)

  if (!is.null(wt)) {
    res <- res |>
      dplyr::rename(.weight = {{wt}}) |>
      dplyr::mutate(
        claims = n_claims * .weight,
        exposure = exposure * .weight,
        .weight_sq = .weight ^ 2,
        .weight_n = 1
      )
  } else {
    res$claims <- res$n_claims
  }

  finish_exp_stats(res, target_status, expected, .groups,
                   start_date, end_date, credibility,
                   cred_p, cred_r, wt)

}

#' @export
print.exp_df <- function(x, ...) {

  cat("Experience study results\n\n",
      "Groups:", paste(groups(x), collapse = ", "), "\n",
      "Target status:", paste(attr(x, "target_status"), collapse = ", "), "\n",
      "Study range:", as.character(attr(x, "start_date")), "to",
      as.character(attr(x, "end_date")), "\n")
  if (!is.null(attr(x, "expected"))) {
    cat(" Expected values:", paste(attr(x, "expected"), collapse = ", "), "\n")
  }
  if (is.null(attr(x, "wt"))) {
    cat("\n")
  } else {
    cat(" Weighted by:", attr(x, "wt"), "\n\n")
  }

  NextMethod()
}


#' @export
groups.exp_df <- function(x) {
  attr(x, "groups")
}

#' @export
#' @rdname exp_stats
summary.exp_df <- function(object, ...) {

  res <- dplyr::group_by(object, !!!rlang::enquos(...))

  .groups <- dplyr::groups(res)
  target_status <- attr(object, "target_status")
  start_date <- attr(object, "start_date")
  end_date <- attr(object, "end_date")
  expected <- attr(object, "expected")
  exp_params <- attr(object, "exp_params")
  wt <- attr(object, "wt")

  finish_exp_stats(res, target_status, expected, .groups,
                   start_date, end_date, exp_params$credibility,
                   exp_params$cred_p, exp_params$cred_r,
                   wt)

}


# support functions -------------------------------------------------------


finish_exp_stats <- function(.data, target_status, expected,
                             .groups, start_date, end_date,
                             credibility, cred_p, cred_r,
                             wt) {

  # expected value formulas. these are already weighted if applicable
  if (!missing(expected)) {
    ex_mean <- exp_form("weighted.mean({expected}, exposure)",
                        "{expected}", expected)
    ex_ae <- exp_form("q_obs / {expected}",
                      "ae_{expected}", expected)
  } else {
    ex_ae <- ex_mean <- expected <- NULL
  }

  # additional columns for weighted studies
  if (!is.null(wt)) {
    wt_forms <- rlang::exprs(
      .weight = sum(.weight),
      .weight_sq = sum(.weight_sq),
      .weight_n = sum(.weight_n),
      ex_wt = .weight / .weight_n,
      ex2_wt = .weight_sq / .weight_n,
    )
  } else {
    wt_forms <- NULL
  }

  # credibility formulas - varying by weights
  if (credibility) {

    y <- (stats::qnorm((1 + cred_p) / 2) / cred_r) ^ 2

    if (is.null(wt)) {
      cred <- rlang::exprs(
        credibility = pmin(1, sqrt(
          n_claims / (y * (1 - q_obs))
          )))
    } else {
      cred <- rlang::exprs(
        credibility = pmin(1, sqrt(
          n_claims /
            (y * ((ex2_wt - ex_wt ^ 2) * .weight_n / (.weight_n - 1) /
                    ex_wt ^ 2 + 1 - q_obs))
          )))
    }

    if(!is.null(expected)) {
      adj_q_exp <- exp_form("credibility * q_obs + (1 - credibility) * {expected}",
                            "adj_{expected}", expected)

      cred <- append(cred, adj_q_exp)
    }

  } else{
    cred <- NULL
  }

  res <- .data |>
    dplyr::summarize(n_claims = sum(n_claims),
                     claims = sum(claims),
                     !!!ex_mean,
                     exposure = sum(exposure),
                     q_obs = claims / exposure,
                     !!!ex_ae,
                     !!!wt_forms,
                     !!!cred,
                     .groups = "drop") |>
    dplyr::relocate(exposure, q_obs, .after = claims)

  if (!is.null(wt)) {
    res <- res |>
      dplyr::select(-ex_wt, -ex2_wt) |>
      dplyr::relocate(.weight, .weight_sq, .weight_n,
                      .after = dplyr::last_col())
  }

  structure(res, class = c("exp_df", class(res)),
            groups = .groups, target_status = target_status,
            start_date = start_date,
            expected = expected,
            end_date = end_date,
            wt = wt,
            exp_params = list(credibility = credibility,
                              cred_p = cred_p, cred_r = cred_r))
}

exp_form <- function(form, col_names, expected) {
  glue::glue(form) |>
    purrr::set_names(glue::glue(col_names)) |>
    rlang::parse_exprs()

}
