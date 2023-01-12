#' get_data
#'
#' @description Loads and prepares data for a specific label
#'
#' @param label String indicating which data-set(s) to get
#'
#' @return Returns data-frame ready to be presented in Shiny app
#'
#' @export
get_data <- function(label) {

  files <- list.files("data/", label, full.names = TRUE)

  full_data <- purrr::list_rbind(
    purrr::map(files, wrangle_raw_ITT_data)
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
wrangle_raw_ITT_data <- function(filepath) {

  raw_data <- readr::read_csv(filepath, col_types = get_colspec())
  cleaned_headers <- dplyr::rename_with(raw_data, stringr::str_to_lower)

  recoded_factors <- dplyr::mutate(cleaned_headers,
                                   argument_position = forcats::fct_recode(
                                     argument_position,
                                     Pro = "For",
                                     Anti = "Against"),
                                   arguer_position = forcats::fct_recode(
                                     arguer_position,
                                     Pro = "PRO",
                                     Anti = "ANTI"))

  aggregated_data <- aggregate_response_ratings(recoded_factors)

  added_condition <- dplyr::mutate(aggregated_data,
                                   condition = extract_condition(filepath),
                                   .after = arguments)

  data_out <- dplyr::mutate(added_condition,
                            ITT_Passed = dplyr::case_when(
                              condition != "ITT" ~ NA,
                              mean_rating >= 5.5 ~ TRUE,
                              mean_rating < 5.5 ~ FALSE))

  return(data_out)
}


#' get_colspec
#'
#' @description function detailing the columns to be loaded form the data-sets
#' and their data types
#'
#' @return Column specification for columns to be loaded from data

get_colspec <- function() {

  column_spec = readr::cols_only(
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
