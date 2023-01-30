source("data-raw/utils.R")

brexit_data <- readr::read_csv("data-raw/brexit_shiny.csv",
                               col_types = get_colspec())

usethis::use_data(brexit_data, overwrite = TRUE)
