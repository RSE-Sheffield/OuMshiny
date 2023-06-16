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
    numericInput(ns("N_words"), "Please select the number of words you'd like displayed",
                 value = 25, max = 50, min = 0),
    textAreaInput(ns("extra_stopwords"),
                  "Please enter any additional words you'd like to exclude from the wordcloud\n(seperate each word by a space or newline)"),

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
      wordcloud2Output(ns("wc_plot")),
      plotOutput(ns("wf_plot")))
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

    extra_stopwords <- reactive({ get_extra_stopwords(input$extra_stopwords) })

    data <- reactive({
      anti_join(tokenised_data(), extra_stopwords(), by = "word")
    })

    N_words <- reactive(input$N_words)

    wordcloud_server("left", ds_name, data, N_words)
    wordcloud_server("right", ds_name, data, N_words)

  })
}

#' wordcloud Server sub-Functions
#'
#' @noRd
#'
#' @import ggplot2
#'
#' @importFrom shiny renderUI selectInput reactive req
#' @importFrom dplyr filter
#' @importFrom wordcloud2 renderWordcloud2 wordcloud2
wordcloud_server <- function(id, ds_name, data, n_words) {
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

    word_freqs <- reactive(generate_word_freqs(filtered_data(), n_words()))


    output$wc_plot <- renderWordcloud2({
      wordcloud2(word_freqs(), rotateRatio = 0)
    })

    output$wf_plot <- renderPlot({
      ggplot(word_freqs(), aes(x = n, y = reorder(word, n))) +
        geom_col() +
        ylab("Word") +
        scale_x_continuous(name = "Frequency",
                           limits = c(0, 100),
                           expand = c(0,0)) +
        theme_bw() +
        theme(text = element_text(size = 18))
    }, height = 700)
  })
}

## To be copied in the UI
# mod_wordcloud_ui("mod_wordcloud_1")

## To be copied in the server
# mod_wordcloud_server("mod_wordcloud_1")
