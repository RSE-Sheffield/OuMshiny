#' mod_wordcloud UI Function
#'
#' @description A shiny Module that produces the wordcloud UI
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tabPanel fluidRow column
#' @importFrom shinyWidgets pickerInput
mod_wordcloud_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title <- id,
    wellPanel(
      pickerInput(ns("dataset"),
                  "Select Dataset",
                  c("Covid-19 Vaccine" = "vaccine",
                    "Brexit" = "brexit",
                    "Veganism" = "veganism")),
      uiOutput(ns("ex_sw")),
      numericInput(ns("N_words"),
                   "Maximum number of words to display",
                   value = 20, max = 50, min = 1)),
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
#' @importFrom shiny NS tagList wellPanel uiOutput sliderInput plotOutput
wordcloud_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("controls")),
      pickerInput(inputId = ns("condition"),
                  label = "Filter by Argument/Rater position",
                  choices = c("Baseline", "ITT")),
      sliderInput(ns("rating"), "Filter by Rating", min = 1, max = 7, value = c(1,7), step = 0.5)
    ),
    wellPanel(
      plotOutput(ns("wc_plot")),
      plotOutput(ns("wf_plot"), height = 700)
    )
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

    output$ex_sw <- renderUI({
      ns <- session$ns
      default_wrds <- paste(default_words(input$dataset), collapse = " ")
      textAreaInput(ns("extra_stopwords"),
                    "Additional words to exclude from the wordcloud\n(seperate each word by a space or newline)",
                    value = default_wrds, width = "100%")
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
#' @importFrom shiny renderUI reactive req
#' @importFrom dplyr filter
#' @importFrom ggwordcloud geom_text_wordcloud
#' @importFrom shinyWidgets pickerInput
wordcloud_server <- function(id, ds_name, data, n_words) {
  moduleServer( id, function(input, output, session){

    output$controls <- renderUI({
      ns <- session$ns
      inputList <- generate_input_list(ds_name())
      select_idx <- switch(id, "left" = 1, "right" = 2)

      pickerInput(inputId = ns("arg_pos"),
                  label = "Filter by Arguer position",
                  choices = inputList,
                  selected = inputList[select_idx])
    })


    filtered_data <- reactive({
      req(input$arg_pos)
      filter(data(),
             arguer_position == input$arg_pos,
             condition == input$condition,
             mean_rating >= input$rating[1] & mean_rating <= input$rating[2])
    })

    word_freqs <- reactive(generate_word_freqs(filtered_data(), n_words()))

    description <- reactive({
      req(input$arg_pos)
      generate_description(ds_name(), input$arg_pos, input$condition)
    })

    output$wc_plot <- renderPlot({
      ggplot(data = word_freqs(), mapping = aes(label = word, size = n, colour = word)) +
        geom_text_wordcloud(shape = "square", grid_margin = 1.5, seed = 2806) +
        scale_size_area(max_size = 25) +
        ggtitle(description()) +
        theme_bw() +
        theme(title = element_text(size = 15))
    })

    output$wf_plot <- renderPlot({
      ggplot(word_freqs(), aes(x = n, y = reorder(word, n), fill = word)) +
        geom_col(show.legend = F) +
        geom_text(aes(label = n), hjust = 1.2, size = 5, colour = "white") +
        ylab(NULL) +
        scale_x_continuous(name = "Frequency",
                           limits = c(0, 100),
                           expand = c(0,0)) +
        theme_bw() +
        theme(text = element_text(size = 18),
              plot.margin = margin(5.5,15,5.5,5.5, "points"))
    }, height = 700)
  })
}

## To be copied in the UI
# mod_wordcloud_ui("mod_wordcloud_1")

## To be copied in the server
# mod_wordcloud_server("mod_wordcloud_1")
