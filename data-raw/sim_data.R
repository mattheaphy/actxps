## code to prepare examples datasets goes here

status_list <- c("Active", "Death", "Surrender")

toy_census <- data.frame(
  pol_num = 1:3,
  status = c("Active", "Death", "Surrender") |> factor(levels = status_list),
  iss_date = as.Date(c("2010-01-01", "2011-05-27", "2009-11-10")),
  term_date = as.Date(c(NA, "2020-09-14", "2022-02-25"))
)

usethis::use_data(toy_census, overwrite = TRUE)

# usethis::use_data(sim_data, overwrite = TRUE)
