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

  full_data <- purrr::list_rbind(
    purrr::map(files, wrangle_raw_itt_data)
  )

  return(full_data)
}

#' wrangle_raw_ITT_data
#'
#' @description function that loads, cleans and aggregates individual data-sets
#' before they are joined
#'
#' @param filepath Path to a specific data-set
#'
#' @return Cleaned and aggregated data-sets
#'
#' @import dplyr
wrangle_raw_itt_data <- function(filepath) {

  raw_data <- readr::read_csv(filepath, col_types = get_colspec())
  cleaned_headers <- rename_with(raw_data, stringr::str_to_lower)

  recoded_factors <- mutate(cleaned_headers,
                            argument_position = forcats::fct_recode(
                              argument_position,
                              Pro = "For",
                              Anti = "Against"),
                            arguer_position = forcats::fct_recode(
                              arguer_position,
                              Pro = "PRO",
                              Anti = "ANTI"))

  aggregated_data <- aggregate_response_ratings(recoded_factors)

  added_condition <- mutate(aggregated_data,
                            condition = extract_condition(filepath),
                            .after = arguments)

  data_out <- mutate(added_condition,
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

#' extract_condition
#'
#' @description Gets the condition for the data-set from the file path
#'
#' @param filepath Path to a specific data-set
#'
#' @return Condition for the data-set as a factor
extract_condition <- function(filepath) {

  condition <- stringr::str_extract(filepath, "(?<=_)[:alpha:]+(?=Data)")
  cond_factor <- factor(condition)

  return(cond_factor)
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
    argument_position = first(argument_position),
    arguer_position = first(arguer_position),
    rater_position = first(rater_position),
    mean_rating = mean(response_ratings),
    .groups = "drop")

  return(mean_data)
}
