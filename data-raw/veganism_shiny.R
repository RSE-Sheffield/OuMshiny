source("data-raw/utils.R")

veganism_data <- readr::read_csv("data-raw/veganism_shiny.csv",
                                col_types = get_colspec())

usethis::use_data(veganism_data, overwrite = TRUE)
