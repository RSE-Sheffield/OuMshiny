#' get_data
#'
#' @description Loads and prepares data for a specific label
#'
#' @param label String indicating which data-set(s) to get
#'
#' @return Returns data-frame ready to be presented in Shiny app
#'
#' @export
get_data <- function(label, path = "data/") {

  files <- list.files(path, label, full.names = TRUE)

  full_data <- aggregate_itt_data(files)

  return(full_data)
}

#' aggregate_itt_data
#'
#' @description function that aggregates data-sets and adds IIT passed col
#'
#' @param filepath Path to a specific data-set
#'
#' @return Aggregated data-set
#'
#' @import dplyr
aggregate_itt_data <- function(filepath) {

  raw_data <- readr::read_csv(filepath, col_types = get_colspec())

  aggregated_data <- aggregate_response_ratings(recoded_factors)


  data_out <- mutate(aggregated_data,
                     ITT_Passed = dplyr::case_when(
                       condition != "ITT" ~ NA,
                       mean_rating >= 5.5 ~ TRUE,
                       mean_rating < 5.5 ~ FALSE
                       )
                     )

  return(data_out)
}


#' get_colspec
#'
#' @description function detailing the columns to be loaded form the data-sets
#' and their data types
#'
#' @return Column specification for columns to be loaded from data
get_colspec <- function() {

  column_spec <- readr::cols_only(
    Arguments = "c",
    argument_Position = "f",
    Arguer_Position = "f",
    rater_position = "f",
    response_ratings = "i",
    argument_ID = "i",
    argument_index = "i"
  )

  return(column_spec)
}

#' aggregate_response_ratings
#'
#' @description Summaries data-set by calculating the mean response rating for
#' each argument
#'
#' @param data_in Data frame to be aggregated
#'
#' @return Aggregated data frame
#'
#' @import dplyr
aggregate_response_ratings <- function(data_in) {

  grouped_data <- group_by(data_in, arguments)

  mean_data <- summarise(
    grouped_data,
    arguments = first(arguments),
    arguer_position = first(arguer_position),
    argument_position = first(argument_position),
    rater_position = first(rater_position),
    condition = first(condition),
    mean_rating = mean(response_ratings),
    .groups = "drop")

  return(mean_data)
}
