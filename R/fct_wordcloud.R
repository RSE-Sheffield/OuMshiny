#' generate_word_freqs
#'
#' @description
#' Takes a data frame of tokens and summarises them into counts per token.
#' Then extracts the top N entries (default: 25).
#'
#' @param data_in dataframe of tokens
#' @param top_n number of rows to extract from the top of the summarised data frame
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
generate_word_freqs <- function(data_in, top_n = 25) {
  count(data_in, word, sort = T) %>%
    slice_head(n = top_n)
}

#' generate_tokens
#'
#' @description
#' Takes a data frame of arguments and tokenises them, removing stop words
#'
#' @param data_in A data frame with (at least) an arguments column.
#' Usually also has arguer_position, argument_position, rater_position, and mean_rating columns
#'
#' @return data frame of tokens and additional info
#'
#' @noRd
#'
#' @import dplyr tidytext
generate_tokens <- function(data_in) {
  unnest_tokens(data_in, word, arguments) %>%
    anti_join(tidytext::stop_words, by = "word")
}

#' get_extra_stopwords
#'
#' @description
#' Takes a string and effectively tokenises it, splitting it into a list of words
#'
#' @param input A string
#'
#' @return A data frame of tokens with the column name "word"
#'
#' @noRd
get_extra_stopwords <- function(input) {
  as.data.frame(
    strsplit(tolower(input),
           "([[:punct:]]*[[:space:]]+)"),
    col.names = "word")
}

#' generate_input_list
#'
#' @description
#' Takes a topic and returns meaningful names for Pro- and Anti- argument positions
#'
#' @param topic String indicating which topic to select
#'
#' @return A vector of named values
#'
#' @noRd
generate_input_list <- function(topic) {

  switch(topic,
         "brexit" = c("Leave" = "Pro", "Remain" = "Anti"),
         "vaccine" = c("Pro-Vaccine" = "Pro", "Anti-Vaccine" = "Anti"),
         "veganism" = c("Vegan" = "Pro", "Non-Vegan" = "Anti"))
}


#' generate_description
#'
#' @description
#' Takes a topic, position, and condition and returns a description of that
#' combination
#'
#' @param topic String indicating which topic to select
#' @param position String indicating arguer position (Pro or Anti)
#' @param condition Sting indicating condition (ITT or Baseline)
#'
#' @return A string describing the combination
#'
#' @importFrom stringr str_glue
#'
#' @noRd
generate_description <- function(topic, position, condition) {

  arguer <- topic_list[[topic]][[position]][[condition]][['arguer']]
  argument <- topic_list[[topic]][[position]][[condition]][['argument']]
  rater <- topic_list[[topic]][[position]][[condition]][['rater']]

  template_string <- "Arguer: {arguer}\nArgument: {argument}\nRaters: {rater}"
  str_glue(template_string)
}

#' generate_subtext
#'
#' @description
#' Takes a topic and position to generate the subtext for the different
#' conditions
#'
#' @param topic String indicating which topic to select
#' @param position String indicating arguer position (Pro or Anti)
#'
#' @return A vector of strings to be used as subtexts
#'
#' @importFrom stringr str_glue
#'
#' @noRd
generate_subtext <- function(topic, position) {

  topic_sublist <- data.frame(topic_list[[topic]][[position]])

  str_glue("Argument: {argument}, Raters: {rater}",
           argument = topic_sublist["argument",],
           rater = topic_sublist["rater",])

}

topic_list <- list(
  "brexit" = list(
    "Pro" = list(
      "Baseline" = c("arguer" = "Leave", "argument" = "Leave", "rater" = "Leave"),
      "ITT" = c("arguer" = "Leave", "argument" = "Remain", "rater" = "Remain")),
    "Anti" = list(
      "Baseline" = c("arguer" = "Remain", "argument" = "Remain", "rater" = "Remain"),
      "ITT" = c("arguer" = "Remain", "argument" = "Leave", "rater" = "Leave"))
  ),
  "vaccine" = list(
    "Pro" = list(
      "Baseline" = c("arguer" = "Pro Covid Vaccine", "argument" = "Pro Covid Vaccine", "rater" = "Pro Covid Vaccine"),
      "ITT" = c("arguer" = "Pro Covid Vaccine", "argument" = "Anti Covid Vaccine", "rater" = "Anti Covid Vaccine")),
    "Anti" = list(
      "Baseline" = c("arguer" = "Anti Covid Vaccine", "argument" = "Anti Covid Vaccine", "rater" = "Anti Covid Vaccine"),
      "ITT" = c("arguer" = "Anti Covid Vaccine", "argument" = "Pro Covid Vaccine", "rater" = "Pro Covid Vaccine"))
  ),
  "veganism" = list(
    "Pro" = list(
      "Baseline" = c("arguer" = "Vegan", "argument" = "Vegan", "rater" = "Vegan"),
      "ITT" = c("arguer" = "Vegan", "argument" = "Non-Vegan", "rater" = "Non-Vegan")),
    "Anti" = list(
      "Baseline" = c("arguer" = "Non-Vegan", "argument" = "Non-Vegan", "rater" = "Non-Vegan"),
      "ITT" = c("arguer" = "Non-Vegan", "argument" = "Vegan", "rater" = "Vegan"))
  )
)
