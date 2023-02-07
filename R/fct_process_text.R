#' process_text
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
process_text <- function(title, positions) {

  raw_md <- "# ${title}

  ### Positions:

  Pro/For = ${positions[1]}

  Anti/Against = ${positions[2]}

  ### Ratings:

  1 = Strongly Agree

  7 = Strongly Disagree

  "

  title <- stringr::str_to_title(title)
  page_text <- stringr::str_interp(raw_md)

  return(page_text)
}
