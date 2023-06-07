#' wordcloud
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import tidytext dplyr
generate_word_freqs <- function(data_in) {
  tidy_text_df <- data_in %>%
    tidytext::unnest_tokens(word, arguments) %>%
    rowwise() %>%
    mutate(word_len = nchar(word)) %>%
    filter(word_len > 2) %>%
    ungroup() %>%
    #mutate_at("word", funs(wordStem((.), language = "en")))
    anti_join(tidytext::stop_words) %>%
    count(word, sort = T) %>%
    top_n(100)

  return(tidy_text_df)
}


generate_input_list <- function(topic) {

  switch(topic,
         "brexit" = c("Leave" = "Pro", "Remain" = "Anti"),
         "vaccine" = c("Pro-Vaccine" = "Pro", "Anti-Vaccine" = "Anti"),
         "veganism" = c("Vegan" = "Pro", "Non-Vegan" = "Anti"))
}
