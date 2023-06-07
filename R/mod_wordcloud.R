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
    selectInput(ns("dataset"), "Select Dataset", c("Covid-19 Vaccine" = "vaccine",
                                                   "Brexit" = "brexit",
                                                   "Veganism" = "veganism")),
    hr(),
    fluidRow(
      column(5,
            selectInput(ns("arg_pos"), "Filter by Arguer position", choices = NULL),
            selectInput(ns("condition"), "Filter by Condition", c("ITT", "Baseline") ),
            sliderInput(ns("rating"), "Filter by Rating", min = 1, max = 7, value = c(1,7), step = 0.5),
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

    observeEvent(input$dataset, {
      updateSelectInput(session, "arg_pos", choices = generate_input_list(input$dataset))
    })

    filtered_data <- reactive({
      data <- get_data(input$dataset)
      data <- filter(data,
                     mean_rating >= input$rating[1] & mean_rating <= input$rating[2],
                     arguer_position == input$arg_pos, condition == input$condition)
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
