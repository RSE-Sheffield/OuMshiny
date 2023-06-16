#' wordcloud
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import tidytext dplyr
generate_word_freqs <- function(data_in, top_n = 25) {
  count(data_in, word, sort = T) %>%
    slice_head(n = top_n)
}

generate_tokens <- function(data_in) {
  unnest_tokens(data_in, word, arguments) %>%
    anti_join(tidytext::stop_words, by = "word")
}


generate_input_list <- function(topic) {

  switch(topic,
         "brexit" = c("Leave" = "Pro", "Remain" = "Anti"),
         "vaccine" = c("Pro-Vaccine" = "Pro", "Anti-Vaccine" = "Anti"),
         "veganism" = c("Vegan" = "Pro", "Non-Vegan" = "Anti"))
}
