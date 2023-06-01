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

    plotOutput(ns("plot"))
  )
}

#' mod_wordcloud Server Functions
#'
#' @noRd
#'
#' @import ggplot
mod_wordcloud_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #data <- get_data("vaccines")

    #word_freqs <- generate_word_freqs()

    output$plot <- renderPlot({
      ggplot(mtcars, aes(wt, mpg)) + geom_point()
    })

  })
}

## To be copied in the UI
# mod_wordcloud_ui("mod_wordcloud_1")

## To be copied in the server
# mod_wordcloud_server("mod_wordcloud_1")
