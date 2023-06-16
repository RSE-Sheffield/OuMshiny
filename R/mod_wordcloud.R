#' mod_wordcloud UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tabPanel selectInput hr fluidRow column
mod_wordcloud_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title <- id,
    selectInput(ns("dataset"), "Select Dataset", c("Covid-19 Vaccine" = "vaccine",
                                                   "Brexit" = "brexit",
                                                   "Veganism" = "veganism")),
    hr(),
    fluidRow(
      column(6, wordcloud_ui(ns("left"))),
      column(6, wordcloud_ui(ns("right")))
    )
  )
}


#' wordcloud UI sub-function
#'
#' @param id, input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList wellPanel uiOutput selectInput sliderInput
#' @importFrom wordcloud2 wordcloud2Output
wordcloud_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("controls")),
      selectInput(ns("condition"), "Filter by Condition", c("ITT", "Baseline") ),
      sliderInput(ns("rating"), "Filter by Rating", min = 1, max = 7, value = c(1,7), step = 0.5),
      wordcloud2Output(ns("wc_plot")))
  )
}


#' mod_wordcloud Server Functions
#'
#' @noRd
#'
#' @importFrom shiny reactive
mod_wordcloud_server <- function(id){
  moduleServer( id, function(input, output, session){

    ds_name <- reactive(input$dataset)
    tokenised_data <- reactive({
      data <- get_data(input$dataset)
      generate_tokens(data)
    })

    wordcloud_server("left", ds_name, data)
    wordcloud_server("right", ds_name, data)

  })
}

#' wordcloud Server sub-Functions
#'
#' @noRd
#'
#' @importFrom shiny renderUI selectInput reactive req
#' @importFrom dplyr filter
#' @importFrom wordcloud2 renderWordcloud2 wordcloud2
wordcloud_server <- function(id, ds_name, data) {
  moduleServer( id, function(input, output, session){

    output$controls <- renderUI({
      ns <- session$ns
      selectInput(ns("arg_pos"), "Filter by Arguer position", choices = generate_input_list(ds_name()))
    })


    filtered_data <- reactive({
      req(input$arg_pos)
      filter(data(),
             arguer_position == input$arg_pos,
             condition == input$condition,
             mean_rating >= input$rating[1] & mean_rating <= input$rating[2])
    })

    output$wc_plot <- renderWordcloud2({
      word_freqs <- generate_word_freqs(filtered_data())
      wordcloud2(word_freqs, rotateRatio = 0)
    })
  })
}

double_wordcloud_demo <- function() {

  ui <- fluidPage(mod_wordcloud_ui("wordcloud"))
  server <- function(input, output, session) {
    mod_wordcloud_server("wordcloud")
  }

  shinyApp(ui, server)
}

double_wordcloud_demo()

## To be copied in the UI
# mod_wordcloud_ui("mod_wordcloud_1")

## To be copied in the server
# mod_wordcloud_server("mod_wordcloud_1")
