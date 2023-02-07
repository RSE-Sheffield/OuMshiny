#' DTouput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tabPanel markdown
mod_dtouput_ui <- function(id, positions) {
  ns <- NS(id)

  if (id == "vaccine") {
    id <- "Covid-19 Vaccine"
  }
  tabPanel(

    title <- stringr::str_to_title(id),

    markdown(process_text(id, positions)),
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
