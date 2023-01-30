#' DTouput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tabPanel
mod_dtouput_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title <- stringr::str_to_title(id),

    h2(title),
    DT::DTOutput(ns("argument_table"))

  )
}

#' DTouput Server Functions
#'
#' @noRd
mod_dtouput_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    ns <- session$ns
    data <- get_data(id)

    output$argument_table <- DT::renderDT(
      data,
      filter = "top",
      selection = "none",
      rownames = FALSE,
      colnames = neaten_headings(colnames(data)),
      options = list(
        paging = FALSE,
        dom = "t",
        autoWidth = TRUE
      )
    )

  })
}
