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
