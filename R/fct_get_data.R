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

  raw_data <- switch(label,
                     "brexit" = OuMshiny::brexit_data,
                     "vaccine" = OuMshiny::vaccine_data,
                     "veganism" = OuMshiny::veganism_data)


  full_data <- aggregate_itt_data(raw_data)

  return(full_data)
}

#' aggregate_itt_data
#'
#' @description function that aggregates data-sets and adds IIT passed col
#'
#' @param raw_data unaggregated data-set
#'
#' @return Aggregated data-set
#'
#' @import dplyr
aggregate_itt_data <- function(raw_data) {

  aggregated_data <- aggregate_response_ratings(raw_data)

  data_out <- mutate(aggregated_data,
                     ITT_Passed = dplyr::case_when(
                       condition != "ITT" ~ NA,
                       mean_rating >= 5.5 ~ TRUE,
                       mean_rating < 5.5 ~ FALSE
                       )
                     )

  return(data_out)
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
#' @importFrom rlang .data
aggregate_response_ratings <- function(data_in) {

  grouped_data <- group_by(data_in, .data$argument_index)

  mean_data <- summarise(
    grouped_data,
    arguments = first(.data$arguments),
    arguer_position = first(.data$arguer_position),
    argument_position = first(.data$argument_position),
    rater_position = first(.data$rater_position),
    condition = first(.data$condition),
    mean_rating = mean(.data$response_ratings),
    .groups = "drop")

  mean_data <- select(mean_data, -argument_index)

  return(mean_data)
}
