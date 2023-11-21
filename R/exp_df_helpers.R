#' Exposed data frame helper functions
#' @export
as_exp_df <- function(x, expected = NULL, col_claims = "claims",
                      col_exposure = "exposure",
                      target_status = NULL,
                      start_date = NULL, end_date = NULL,
                      credibility = FALSE,
                      conf_level = 0.95, cred_r = 0.05, conf_int = FALSE) {

  x <- x |>
    rename(exposure = {{col_exposure}},
           claims = {{col_claims}}) |>
    mutate(n_claims = claims)

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
