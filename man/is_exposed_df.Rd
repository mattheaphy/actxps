% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exposed_df_helpers.R
\name{is_exposed_df}
\alias{is_exposed_df}
\alias{as_exposed_df}
\title{Exposed data frame helper functions}
\usage{
is_exposed_df(x)

as_exposed_df(
  x,
  end_date,
  start_date = as.Date("1900-01-01"),
  target_status = NULL,
  cal_expo = FALSE,
  expo_length = "year"
)
}
\arguments{
\item{x}{an object. \code{x} must be a data frame when calling \code{as_exposed_df()}}

\item{end_date}{experience study end date}

\item{start_date}{experience study start date. Default value = 1900-01-01.}

\item{target_status}{character vector of target status values. Default value = \code{NULL}.}

\item{cal_expo}{set to TRUE for calendar year exposures. Otherwise policy year exposures are used.}

\item{expo_length}{exposure period length}
}
\value{
For \code{is_exposed_df()}, a length-1 logical vector. For
\code{as_exposed_df()}, an \code{exposed_df} object.
}
\description{
Test for and coerce to the \code{exposed_df} class.
}
\details{
These are behind-the-scenes functions that will generally not be called
by users.

\code{is_exposed_df()} will return \code{TRUE} if \code{x} is an \code{exposed_df} object.

\code{as_exposed_df()} will coerce a data frame to an \code{exposed_df} object.
}