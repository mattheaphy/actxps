## code to prepare examples datasets goes here
library(tidyverse)

status_list <- c("Active", "Death", "Surrender")

toy_census <- data.frame(
  pol_num = 1:3,
  status = c("Active", "Death", "Surrender") |> factor(levels = status_list),
  issue_date = as.Date(c("2010-01-01", "2011-05-27", "2009-11-10")),
  term_date = as.Date(c(NA, "2020-09-14", "2022-02-25"))
)

usethis::use_data(toy_census, overwrite = TRUE)

qx_iamb <- read_csv("data-raw/rates/qx_iamb.csv", col_types = "idc")
scale_g2 <- read_csv("data-raw/rates/scaleG2.csv", col_types = "idc")

usethis::use_data(qx_iamb, overwrite = TRUE)
usethis::use_data(scale_g2, overwrite = TRUE)

source("data-raw/simulate_data.R")

usethis::use_data(census_dat, overwrite = TRUE)
