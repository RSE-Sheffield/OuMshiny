#' mod_wordcloud UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_wordcloud_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title <- id,
    selectInput(ns("dataset"), "Select Dataset", c("vaccine", "brexit", "veganism")),
    plotOutput(ns("plot"))
  )
}

#' mod_wordcloud Server Functions
#'
#' @noRd
#'
#' @import ggplot ggwordcloud
mod_wordcloud_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    word_freqs <- reactive({
      data <- get_data(input$dataset)
      word_freqs <- generate_word_freqs(data)
    })

    output$plot <- renderPlot({
      ggplot(word_freqs()) +
        geom_text_wordcloud_area(aes(label = word, size = n)) +
        scale_size_area(max_size = 15)
      })

  })
}

## To be copied in the UI
# mod_wordcloud_ui("mod_wordcloud_1")

## To be copied in the server
# mod_wordcloud_server("mod_wordcloud_1")
