#' Summarize experience study records
#'
#' @description Create a summary data frame of termination experience for a
#' given target status.
#'
#' @details If `.data` is grouped, the resulting data frame will contain
#' one row per group.
#'
#' If `target_status` isn't provided, [exp_stats()] will use the same
#' target status from `.data` if it has the class `exposed_df`.
#' Otherwise, all status values except the first level will be assumed.
#' This will produce a warning message.
#'
#' # Expected values
#'
#' The `expected` argument is optional. If provided, this argument must
#' be a character vector with values corresponding to column names in `.data`
#' containing expected experience. More than one expected basis can be provided.
#'
#' # Control variables
#'
#' The `control_vars` argument is optional. If provided, this argument must
#' be `".none"` (more on this below) or a character vector with values
#' corresponding to column names in `.data`. Control variables are used to
#' estimate the impact of any grouping variables on observed experience
#' *after accounting for* the impact of control variables.
#'
#' Mechanically, when values are passed to `control_vars`, a separate call
#' is made to [exp_stats()] using the control variables as grouping variables.
#' This is used to derive a new expected values basis called `control`, which is
#' both added to `.data` and appended to the `expected` argument. In the final
#' output, a column called `ae_control` shows the relative impact of any
#' grouping variables after accounting for the control variables.
#'
#' **About `".none"`**: If `".none"` is passed to `control_vars`, a single
#' aggregate termination rate is calculated for the entire data set and used to
#' compute `control` and `ae_control`.
#'
#' The `control_distinct_max` argument places an upper limit on the number of
#' unique values that a control variable is allowed to have. This limit exists
#' to prevent an excessive number of groups on continuous or high-cardinality
#' features.
#'
#' It should be noted that usage of control variables is a rough approximation
#' and not a substitute for rigorous statistical models. The impact of control
#' variables is calculated in isolation and does consider other features or
#' possible confounding variables. As such, control variables are most useful
#' for exploratory data analysis.
#'
#' # Credibility
#'
#' If `credibility` is set to `TRUE`, the output will contain a
#' `credibility` column equal to the partial credibility estimate under
#' the Limited Fluctuation credibility method (also known as Classical
#' Credibility) assuming a binomial distribution of claims.
#'
#' # Confidence intervals
#'
#' If `conf_int` is set to `TRUE`, the output will contain lower and upper
#' confidence interval limits for the observed termination rate and any
#' actual-to-expected ratios. The confidence level is dictated
#' by `conf_level`. If no weighting variable is passed to `wt`, confidence
#' intervals will be constructed assuming a binomial distribution of claims.
#' Otherwise, confidence intervals will be calculated assuming that the
#' aggregate claims distribution is normal with a mean equal to observed claims
#' and a variance equal to:
#'
#' `Var(S) = E(N) * Var(X) + E(X)^2 * Var(N)`,
#'
#' Where `S` is the aggregate claim random variable, `X` is the weighting
#' variable assumed to follow a normal distribution, and `N` is a binomial
#' random variable for the number of claims.
#'
#' If `credibility` is `TRUE` and expected values are passed to `expected`,
#' the output will also contain confidence intervals for any
#' credibility-weighted termination rates.
#'
#' # `summary()` Method
#'
#' Applying `summary()` to a `exp_df` object will re-summarize the
#' data while retaining any grouping variables passed to the "dots"
#' (`...`).
#'
#' @param .data A data frame with exposure-level records, ideally of type
#' `exposed_df`
#' @param target_status A character vector of target status values
#' @param expected A character vector containing column names in `.data`
#' with expected values
#' @param col_exposure Name of the column in `.data` containing exposures
#' @param col_status Name of the column in `.data` containing the policy status
#' @param wt Optional. Length 1 character vector. Name of the column in
#' `.data` containing weights to use in the calculation of claims,
#' exposures, partial credibility, and confidence intervals.
#' @param credibility If `TRUE`, the output will include partial credibility
#' weights and credibility-weighted termination rates.
#' @param conf_level Confidence level used for the Limited Fluctuation
#' credibility method and confidence intervals
#' @param cred_r Error tolerance under the Limited Fluctuation credibility
#' method
#' @param conf_int If `TRUE`, the output will include confidence intervals
#' around the observed termination rates and any actual-to-expected ratios.
#' @param control_vars `".none"` or a character vector containing column names
#' in `.data` to use as control variables
#' @param control_distinct_max Maximum number of unique values allowed for
#' control variables
#' @param object An `exp_df` object
#' @param ... Groups to retain after `summary()` is called
#'
#' @return A tibble with class `exp_df`, `tbl_df`, `tbl`,
#' and `data.frame`. The results include columns for any grouping variables,
#' claims, exposures, and observed termination rates (`q_obs`).
#'
#' - If any values are passed to `expected` or `control_vars`, additional
#' columns are added for expected termination rates and actual-to-expected
#' (A/E) ratios. A/E ratios are prefixed by `ae_`.
#' - If `credibility` is set to `TRUE`, additional columns are added
#' for partial credibility and credibility-weighted termination rates
#' (assuming values are passed to `expected`). Credibility-weighted termination
#' rates are prefixed by `adj_`.
#' - If `conf_int` is set to `TRUE`, additional columns are added for lower and
#' upper confidence interval limits around the observed termination rates and
#' any actual-to-expected ratios. Additionally, if `credibility` is `TRUE` and
#' expected values are passed to `expected`, the output will contain confidence
#' intervals around credibility-weighted termination rates. Confidence interval
#' columns include the name of the original output column suffixed by either
#' `_lower` or `_upper`.
#' - If a value is passed to `wt`, additional columns are created containing
#' the the sum of weights (`.weight`), the sum of squared weights
#' (`.weight_qs`), and the number of records (`.weight_n`).
#'
#' @examples
#' toy_census |> expose("2022-12-31", target_status = "Surrender") |>
#'     exp_stats()
#'
#' exp_res <- census_dat |>
#'            expose("2019-12-31", target_status = "Surrender") |>
#'            group_by(pol_yr, inc_guar) |>
#'            exp_stats(control_vars = "product")
#'
#' exp_res
#' summary(exp_res)
#' summary(exp_res, inc_guar)
#'
#' @references Herzog, Thomas (1999). Introduction to Credibility Theory
#'
#' @export
exp_stats <- function(
  .data,
  target_status = attr(.data, "target_status"),
  expected,
  col_exposure = "exposure",
  col_status = "status",
  wt = NULL,
  credibility = FALSE,
  conf_level = 0.95,
  cred_r = 0.05,
  conf_int = FALSE,
  control_vars,
  control_distinct_max = 25L
) {
  .groups <- groups(.data)
  start_date <- attr(.data, "start_date")
  end_date <- attr(.data, "end_date")

  if (is.null(target_status)) {
    target_status <- levels(.data$status)[-1]
    cli::cli_warn(c(
      x = "No target status was provided.",
      i = "{.val {target_status}} {?was/were} assumed."
    ))
  }

  if (length(wt) > 1) {
    cli::cli_abort(c(
      x = "Only 1 column can be passed to `wt`. You supplied {length(wt)} values."
    ))
  }

  check_split_expose_basis(.data, col_exposure)

  res <- .data |>
    rename(exposure = {{ col_exposure }}, status = {{ col_status }}) |>
    mutate(n_claims = status %in% target_status)

  if (!is.null(wt)) {
    res <- res |>
      rename(.weight = {{ wt }}) |>
      mutate(
        claims = n_claims * .weight,
        exposure = exposure * .weight,
        .weight_sq = .weight^2,
        .weight_n = 1
      )
  } else {
    res$claims <- res$n_claims
  }

  if (missing(expected) || is.null(expected)) {
    expected <- NULL
  } else {
    verify_col_exist(names(res), expected, "expected values column")
  }

  if (!missing(control_vars) && !is.null(control_vars)) {
    # special handling for aggregates
    if (all(control_vars == ".none")) {
      if (".none" %in% names(res)) {
        cli::cli_abort(
          "Name conflict error: {.val .none} cannot be passed to `control_vars`
          because there is also column in the data with the same name."
        )
      }
      control_vars <- "None"
      res[["None"]] <- 1L
    } else {
      if (".none" %in% control_vars) {
        cli::cli_abort(
          "If {.val .none} is passed to `control_vars`, then no other values
          are allowed."
        )
      }
      verify_col_exist(names(res), control_vars, "control variable")
    }

    # throw an error if too many unique values
    nd_ctrl <- res |>
      ungroup() |>
      dplyr::summarize(dplyr::across(control_vars, dplyr::n_distinct))
    nd_ctrl <- colnames(nd_ctrl)[nd_ctrl > control_distinct_max]
    if (length(nd_ctrl) > 0) {
      cli::cli_abort(c(
        x = "There are too many distinct values in the {.val {nd_ctrl}} control variable{?s}.",
        i = paste0(
          "Limit = {control_distinct_max}. Update the `control_distinct_max` ",
          "argument to increase the limit, or consider techniques like ",
          "binning to reduce cardinality"
        )
      ))
    }

    # calculate observed rates across control variables
    ctrl_dat <- finish_exp_stats(
      res |> group_by(dplyr::across(dplyr::all_of(control_vars))),
      target_status,
      expected = NULL,
      .groups = control_vars,
      start_date = start_date,
      end_date = end_date,
      credibility = FALSE,
      conf_level = conf_level,
      cred_r = cred_r,
      wt = wt,
      conf_int = FALSE,
      control_vars = NULL
    ) |>
      select(dplyr::all_of(control_vars), control = q_obs)

    # join observed rates on control variables to the data and add these
    #   to expected values
    res <- left_join(res, ctrl_dat, by = control_vars)
    expected <- c(expected, "control")
  } else {
    control_vars <- NULL
  }

  finish_exp_stats(
    res,
    target_status,
    expected,
    .groups,
    start_date,
    end_date,
    credibility,
    conf_level,
    cred_r,
    wt,
    conf_int,
    control_vars
  )
}

#' @export
print.exp_df <- function(x, ...) {
  cli::cli_h2("Experience study results")
  if (length(groups(x)) > 0) {
    cli::cli_ul("{.field Groups}: {groups(x)}")
  }
  cli::cli_ul(c(
    "{.field Target status}: {attr(x, 'target_status')}",
    "{.field Study range}: {attr(x, 'start_date')} to {attr(x, 'end_date')}"
  ))
  if (!is.null(attr(x, "control_vars"))) {
    cli::cli_ul("{.field Control variables}: {attr(x, 'control_vars')}")
  }
  if (!is.null(attr(x, "expected"))) {
    cli::cli_ul("{.field Expected values}: {attr(x, 'expected')}")
  }
  if (!is.null(attr(x, "wt"))) {
    cli::cli_ul("{.field Weighted by}: {attr(x, 'wt')}\n")
  }

  cat("\n")
  NextMethod()
}


#' @export
groups.exp_df <- function(x) {
  attr(x, "groups")
}

#' @export
#' @rdname exp_stats
summary.exp_df <- function(object, ...) {
  res <- group_by(object, !!!rlang::enquos(...))

  .groups <- groups(res)
  target_status <- attr(object, "target_status")
  start_date <- attr(object, "start_date")
  end_date <- attr(object, "end_date")
  expected <- attr(object, "expected")
  xp_params <- attr(object, "xp_params")
  wt <- attr(object, "wt")
  control_vars <- attr(object, "control_vars")

  finish_exp_stats(
    res,
    target_status,
    expected,
    .groups,
    start_date,
    end_date,
    xp_params$credibility,
    xp_params$conf_level,
    xp_params$cred_r,
    wt,
    xp_params$conf_int,
    control_vars
  )
}


# support functions -------------------------------------------------------

finish_exp_stats <- function(
  .data,
  target_status,
  expected,
  .groups,
  start_date,
  end_date,
  credibility,
  conf_level,
  cred_r,
  wt,
  conf_int,
  control_vars
) {
  # expected value formulas. these are already weighted if applicable
  if (!is.null(expected)) {
    ex_mean <- exp_form("weighted.mean({.col}, exposure)", "{.col}", expected)
    ex_ae <- exp_form("q_obs / {.col}", "ae_{.col}", expected)
  } else {
    ex_ae <- ex_mean <- NULL
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
    y <- (stats::qnorm((1 + conf_level) / 2) / cred_r)^2

    if (is.null(wt)) {
      cred <- rlang::exprs(
        credibility = pmin(
          1,
          sqrt(
            n_claims / (y * (1 - q_obs))
          )
        )
      )
    } else {
      cred <- rlang::exprs(
        credibility = pmin(
          1,
          sqrt(
            n_claims /
              (y *
                ((ex2_wt - ex_wt^2) *
                  .weight_n /
                  (.weight_n - 1) /
                  ex_wt^2 +
                  1 -
                  q_obs))
          )
        )
      )
    }

    if (!is.null(expected)) {
      adj_q_exp <- exp_form(
        "credibility * q_obs + (1 - credibility) * {.col}",
        "adj_{.col}",
        expected
      )

      cred <- append(cred, adj_q_exp)
    }
  } else {
    cred <- NULL
  }

  # confidence interval formulas
  if (conf_int) {
    p <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)

    if (is.null(wt)) {
      ci <- rlang::exprs(
        q_obs_lower = stats::qbinom(p[[1]], exposure, q_obs) / exposure,
        q_obs_upper = stats::qbinom(p[[2]], exposure, q_obs) / exposure
      )
    } else {
      ci <- rlang::exprs(
        # For binomial N
        # Var(S) = n * p * (Var(X) + E(X)^2 * (1 - p))
        sd_agg = (n_claims * ((ex2_wt - ex_wt^2) + ex_wt^2 * (1 - q_obs)))^0.5,
        q_obs_lower = stats::qnorm(p[[1]], claims, sd_agg) / exposure,
        q_obs_upper = stats::qnorm(p[[2]], claims, sd_agg) / exposure
      )
    }

    if (!is.null(expected)) {
      ae_lower <- exp_form("q_obs_lower / {.col}", "ae_{.col}_lower", expected)
      ae_upper <- exp_form("q_obs_upper / {.col}", "ae_{.col}_upper", expected)
      ci <- append(ci, c(ae_lower, ae_upper))
      if (credibility) {
        ci <- append(
          ci,
          c(
            exp_form(
              "credibility * q_obs_lower + (1 - credibility) * {.col}",
              "adj_{.col}_lower",
              expected
            ),
            exp_form(
              "credibility * q_obs_upper + (1 - credibility) * {.col}",
              "adj_{.col}_upper",
              expected
            )
          )
        )
      }
    }
  } else {
    ci <- NULL
  }

  res <- .data |>
    dplyr::summarize(
      n_claims = sum(n_claims),
      claims = sum(claims),
      !!!ex_mean,
      exposure = sum(exposure),
      q_obs = claims / exposure,
      !!!ex_ae,
      !!!wt_forms,
      !!!cred,
      !!!ci,
      .groups = "drop"
    ) |>
    relocate(
      exposure,
      q_obs,
      dplyr::any_of(c("q_obs_lower", "q_obs_upper")),
      .after = claims
    )

  if (!is.null(wt)) {
    res <- res |>
      select(-ex_wt, -ex2_wt, -dplyr::any_of("sd_agg")) |>
      relocate(.weight, .weight_sq, .weight_n, .after = dplyr::last_col())
  }

  new_exp_df(
    res,
    .groups = .groups,
    target_status = target_status,
    start_date = start_date,
    expected = expected,
    end_date = end_date,
    wt = wt,
    credibility = credibility,
    conf_level = conf_level,
    cred_r = cred_r,
    conf_int = conf_int,
    control_vars = control_vars
  )
}

# low level class constructor
new_exp_df <- function(
  x,
  .groups,
  target_status,
  start_date,
  expected,
  end_date,
  wt,
  credibility,
  conf_level,
  cred_r = cred_r,
  conf_int,
  control_vars
) {
  tibble::new_tibble(
    x,
    class = "exp_df",
    groups = .groups,
    target_status = target_status,
    start_date = start_date,
    expected = expected,
    end_date = end_date,
    wt = wt,
    xp_params = list(
      credibility = credibility,
      conf_level = conf_level,
      cred_r = cred_r,
      conf_int = conf_int
    ),
    control_vars = control_vars
  )
}

# this function is used to create formula specifications passed to dplyr::mutate
# or dplyr::summarize using a common formula applied across several columns with
# a common naming structure.
# Note - this could be handled using across, but is not due to performance on
# grouped data frames
#' @param form A formula for new columns that will be passed into
#' `glue::glue()` that uses the placeholder "{.col}"
#' @param new_col The desired names of the new columns. This will also use the
#' placeholder "{.col}".
#' @param .col The names of existing columns that "{.col}" refers to
#' @noRd
exp_form <- function(form, new_col, .col) {
  gsub("\\{\\.col\\}", "`{.col}`", form) |>
    glue::glue() |>
    purrr::set_names(glue::glue(new_col)) |>
    rlang::parse_exprs()
}

verify_exp_df <- function(.data) {
  if (!inherits(.data, "exp_df")) {
    cli::cli_abort(c(
      x = "`{deparse(substitute(.data))}` must be an `exp_df` object.",
      i = "Hint: Use `exp_stats()` to create `exp_df` objects."
    ))
  }
}
