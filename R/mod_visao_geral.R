#' visao_geral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visao_geral_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      max_height = "150px",
      h6("Filtros"),
      dateRangeInput(
        inputId = ns("periodo"),
        label = "Selecione um período",
        start = Sys.Date(),
        end = Sys.Date(),
        min = "2019-01-01",
        max = Sys.Date(),
        format = "dd/mm/yy",
        separator = " a ",
        language = "pt-BR",
        width = "25%"
      )
    ),
    bslib::card(
      h6("Série histórica do número de vôos")
    ),
    bslib::layout_columns(
      bslib::card(
        h6("Aeroportos com mais vôos")
      ),
      bslib::card(
        h6("Empresas aéreas com mais vôos")
      )
    )
  )
}
    
#' visao_geral Server Functions
#'
#' @noRd 
mod_visao_geral_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

 
  })
}
    
## To be copied in the UI
# mod_visao_geral_ui("visao_geral_1")
    
## To be copied in the server
# mod_visao_geral_server("visao_geral_1")
