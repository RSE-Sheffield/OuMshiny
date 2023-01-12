#' neaten_headings
#'
#' `neaten_headings` is to neaten the headings of a data frame in preparation
#' for displaying on a Shiny App
#'
#' Strings are neatened by removing any leading/following underscores, replacing
#' any sequence of underscores within the string with a single space, putting
#' the rest of the sting to title case, and ensuring that ITT remains
#' capitalised
#'
#' @param column_names A string or vector of strings
#'
#' @return A string or vector of strings
#'
#' @export
neaten_headings <- function(column_names) {
  # remove underscores at beginning and end
  underscore_replaced <- stringr::str_remove_all(column_names, "^_+|_+$")

  # replace any sequence of underscores in middle of the string with a single space
  underscore_replaced <- stringr::str_replace_all(underscore_replaced,
                                                  "(?<=\\w)_+(?=\\w)", " ")

  title_case <- stringr::str_to_title(underscore_replaced)

  ITT_all_caps <- stringr::str_replace(title_case, "(?<=\\b)Itt(?=\\b)", "ITT")

  return(ITT_all_caps)
}
