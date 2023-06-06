#' mod_wordcloud UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import wordcloud2
mod_wordcloud_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title <- id,
    selectInput(ns("dataset"), "Select Dataset", c("vaccine", "brexit", "veganism")),
    fluidRow(
      column(5,
            selectInput(ns("arg_pos1"), "Select Argument position", c("For", "Against")),
            selectInput(ns("arguer_pos1"), "Select Arguer position", c("Pro", "Anti") ),
            wordcloud2Output(ns("plot1"))
      )
    )
  )
}


#' mod_wordcloud Server Functions
#'
#' @noRd
#'
#' @import wordcloud2
mod_wordcloud_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot1 <- renderWordcloud2({
      data <- get_data(input$dataset)
      data <- filter(data, argument_position == input$arg_pos1)
      word_freqs <- generate_word_freqs(data)
      wordcloud2(word_freqs, rotateRatio = 0)
    })

  })
}

## To be copied in the UI
# mod_wordcloud_ui("mod_wordcloud_1")

## To be copied in the server
# mod_wordcloud_server("mod_wordcloud_1")
