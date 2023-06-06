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
            selectInput(ns("arg_pos"), "Filter by Arguer position", c("All", "Pro", "Anti")),
            selectInput(ns("condition"), "Filter by Condition", c("All", "ITT", "Baseline") ),
            sliderInput(ns("rating"), "Filter by Rating", min = 1, max = 7, value = c(1,7)),
            wordcloud2Output(ns("plot"))
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

    filtered_data <- reactive({
      data <- get_data(input$dataset)
      data <- filter(data, mean_rating >= input$rating[1] & mean_rating <= input$rating[2])
      if (input$arg_pos != "All") data <- filter(data, arguer_position == input$arg_pos)
      if (input$condition != "All") data <- filter(data, condition == input$condition)
      return(data)
    })

    output$plot <- renderWordcloud2({
      data <- filtered_data()
      word_freqs <- generate_word_freqs(data)
      wordcloud2(word_freqs, rotateRatio = 0)
    })

  })
}

## To be copied in the UI
# mod_wordcloud_ui("mod_wordcloud_1")

## To be copied in the server
# mod_wordcloud_server("mod_wordcloud_1")
