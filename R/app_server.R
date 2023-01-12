#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_DTouput_server("vaccine")
  mod_DTouput_server("vegan")
  mod_DTouput_server("brexit")
}
