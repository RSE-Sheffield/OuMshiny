#' DTouput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DTouput_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' DTouput Server Functions
#'
#' @noRd 
mod_DTouput_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_DTouput_ui("DTouput_1")
    
## To be copied in the server
# mod_DTouput_server("DTouput_1")
