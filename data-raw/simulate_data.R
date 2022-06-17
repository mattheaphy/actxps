library(tidyverse)
library(lubridate)

# simulation function -----------------------------------------------------

sim_data <- function(n_pol = 1) {

  initial_year <- 2005L

  # surrender charge years remaining
  pol_yr_dist <- tibble(
    value = 1:15,
    prob = 1 / 15)

  # income guarantee
  inc_guar_dist <- tibble(
    value = c(FALSE, TRUE),
    prob = c(0.4, 0.6))

  # qualified money
  qual_dist <- tibble(
    value = c(FALSE, TRUE),
    prob = c(0.45, 0.55))

  # issue age
  iss_age_dist <- tribble(~age, ~prob,
                          40,0.01,
                          45,0.02,
                          50,0.05,
                          55,0.15,
                          60,0.17,
                          65,0.225,
                          70,0.175,
                          75,0.1,
                          80,0.1)

  iss_age_dist <- tibble(value = 40:80) |>
    mutate(prob = approx(iss_age_dist$age, iss_age_dist$prob, xout = value)$y,
           prob = prob / sum(prob))

  # product
  product_dist <- tibble(
    value = letters[1:3] |> factor(),
    prob = c(0.25, 0.25, 0.5))

  # gender
  gender_dist <- tibble(value = c("F", "M") |> factor(), prob = 0.5)

  # withdrawal timing
  wd_time_dist <- tribble(~value, ~cprob,
                          40, 0,
                          50, 0,
                          55, 0.05,
                          60, 0.15,
                          65, 0.40,
                          70, 0.65,
                          75, 0.80,
                          80, 0.95,
                          85, 0.95)

  wd_time_dist <- tibble(value = 40:85,
                         cprob = approx(wd_time_dist$value, wd_time_dist$cprob, 40:85)$y) |>
    mutate(prob = diff(c(0, cprob)))

  # check that probabilities sum to 1
  stopifnot(near(sum(pol_yr_dist$prob), 1))
  stopifnot(near(sum(inc_guar_dist$prob), 1))
  stopifnot(near(sum(qual_dist$prob), 1))
  stopifnot(near(sum(iss_age_dist$prob), 1))
  stopifnot(near(sum(product_dist$prob), 1))
  stopifnot(sum(wd_time_dist$prob) <= 1)

  draw <- function(x) sample(x$value, n_pol, replace = TRUE, prob = x$prob)

  tibble(
    pol_num = 1:n_pol,
    pol_yr = draw(pol_yr_dist),
    issue_date = make_date(initial_year + (15 - pol_yr)) +
      sample(0:364, n_pol, replace = TRUE),
    inc_guar = draw(inc_guar_dist),
    qual = draw(qual_dist),
    age = draw(iss_age_dist),
    product = draw(product_dist),
    gender = draw(gender_dist),
    wd_age = draw(wd_time_dist) |> pmax(age)
  )

}

expand_sim <- function(dat) {


  # assumptions ---------------
  # surrender charge years remaining and income guarantee
  base_rates <- read_csv("data-raw/rates/base_rates.csv", col_types = "ild")

  # qualified money
  qual_mult <- function(qual, age) {

    case_when(qual & age >= 70 ~ 0.8,
              qual & age >= 60 ~ 0.9,
              qual ~ 1,
              TRUE ~ 1.1)
  }

  # attained age
  age_mult <- function(age, glwb) {

    if_else(!glwb,
            case_when(
              age < 50 ~ 1.3,
              age < 60 ~ 1,
              age < 65 ~ 0.9,
              age < 70 ~ 0.8,
              age < 80 ~ 1,
              age < 90 ~ 1.2,
              TRUE ~ 1.5),
            case_when(
              age < 50 ~ 1.2,
              age < 60 ~ 1,
              age < 65 ~ 0.9,
              age < 70 ~ 0.8,
              age < 80 ~ 0.7,
              age < 90 ~ 0.4,
              TRUE ~ 0.25)
    )
  }

  # product
  prod_mult <- c(a = 1.25, b = 0.9, c = 1)

  # gender
  gender_mult <- c(M = 1.1, F = 1)

  # withdrawal timing
  wd_time_mult <- function(exercised, age, inc_guar) {

    case_when(
      inc_guar == "FALSE" ~ 1,
      exercised ~ 1,
      age > 80 ~ 3,
      age > 70 ~ 2.25,
      age < 60 ~ 1.5,
      TRUE ~ 2
    )

  }

  qx_iamb$gender <- str_sub(qx_iamb$gender, 1, 1)
  scale_g2$gender <- str_sub(scale_g2$gender, 1, 1)

  dat <- dat |>
    slice(rep(row_number(), pol_yr)) |>
    group_by(pol_num) |>
    mutate(pol_yr = row_number()) |>
    ungroup() |>
    mutate(t = pol_yr,
           age = age + t - 1,
           exercised = age >= wd_age & inc_guar,
           pol_yr2 = pmin(max(base_rates$pol_yr), pol_yr)) |>
    left_join(base_rates, by = c("pol_yr2" = "pol_yr", "inc_guar")) |>
    left_join(qx_iamb, by = c("age", "gender")) |>
    left_join(scale_g2, by = c("age", "gender")) |>
    mutate(
      qual_mult = qual_mult(qual, age),
      age_mult = age_mult(age, inc_guar),
      prod_mult = prod_mult[product],
      gender_mult = gender_mult[gender],
      wd_time_mult = wd_time_mult(exercised, age, inc_guar),
      q_w = pmin(qual_mult * age_mult * prod_mult * base_rate * wd_time_mult,
                   0.99),
      cal_yr = year(issue_date) + pol_yr - 1,
      qx = qx * (1 - mi) ^ (cal_yr - 2012)
    ) |>
    select(-pol_yr2, -t, -cal_yr, -mi) |>
    rename(q_d = qx)

  persist <- function(x) {
    # default to and "already terminated" status
    quit_status <- dim(x)[[2]] + 1
    res <- rep_len(quit_status, dim(x)[[1]])

    for (i in seq_len(dim(x)[[1]])) {

      # cycle through each decrement and determine terminations
      for (j in seq_len(dim(x)[[2]])) {
        if (runif(1) < x[[i, j]]) {
          res[[i]] <- j
          break
        }
      }

      if (res[[i]] < quit_status) break

      # otherwise set to status 0 = active
      res[[i]] <- 0L
    }

    res
  }

  # status mapping
  status_map <- c(`0` = "Active", `1` = "Death", `2` = "Surrender")

  # add claims
  set.seed(4375)
  dat <- dat |>
    group_by(pol_num) |>
    mutate(status = persist(cbind(Death = q_d, Surrender = q_w))) |>
    ungroup() |>
    filter(status <= 2) |>
    mutate(status = factor(status_map[as.character(status)],
                           levels = status_map))

  dat |>
    mutate(term_date = if_else(status == "Active",
                               NA_Date_,
                               issue_date %m+% years(pol_yr - 1) +
                                 sample(0:364, nrow(dat), replace = TRUE))) |>
    select(-base_rate, -ends_with("_mult"))

}


set.seed(123)
census_dat <- sim_data(2E4)
expo_dat <- expand_sim(census_dat)

final_status <- expo_dat |>
  group_by(pol_num) |>
  filter(pol_yr == max(pol_yr)) |>
  ungroup() |>
  select(pol_num, status, pol_yr, term_date)

# add final policy statuses and termination times back to the exposure data
census_dat <- census_dat |>
  select(-pol_yr) |>
  inner_join(final_status, by = "pol_num") |>
  select(pol_num, status, pol_yr, everything())
