#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_dtouput_server("vaccine")
  mod_dtouput_server("veganism")
  mod_dtouput_server("brexit")
}
