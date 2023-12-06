#' filtros UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filtros_ui <- function(id){
  ns <- NS(id)
  tagList(
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
      language = "pt-BR"
    ),
    picker_input(
      inputId = ns("natureza_voo"),
      label = "Natureza do voo",
      choices = c("Carregando..." = ""),
      multiple = TRUE
    ),
    picker_input(
      inputId = ns("tipo_servico"),
      label = "Tipo de serviço",
      choices = c("Carregando..." = ""),
      multiple = TRUE
    ),
    actionButton(
      inputId = ns("filtrar"),
      label = "Aplicar filtros"
    )
  )
}

#' filtros Server Functions
#'
#' @noRd 
mod_filtros_server <- function(id, con) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns

    tab_voos <- dplyr::tbl(con, "tab_voos")

    datas <- tab_voos |>
      dplyr::summarise(
        min = min(dt_partida_real, na.rm = TRUE),
        max = max(dt_partida_real, na.rm = TRUE)
      ) |> 
      dplyr::collect()

    updateDateRangeInput(
      session = session,
      inputId = "periodo",
      start = datas$min,
      end = datas$max,
      min = datas$min,
      max = datas$max
    )

    opcoes_natureza <- tab_voos |> 
        dplyr::distinct(ds_natureza_tipo_linha) |> 
        dplyr::pull(ds_natureza_tipo_linha)

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "natureza_voo",
      choices = opcoes_natureza,
      selected = opcoes_natureza
    )

    opcoes_servico <- tab_voos |> 
        dplyr::distinct(ds_servico_tipo_linha) |> 
        dplyr::pull(ds_servico_tipo_linha)

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "tipo_servico",
      choices = opcoes_servico,
      selected = opcoes_servico
    )

    dados_filtrados <- eventReactive(input$filtrar, {
      tab_voos |>
        dplyr::filter(
          dt_partida_real <= !!input$periodo[2] &
            dt_partida_real >= !!input$periodo[1],
          ds_natureza_tipo_linha %in% !!input$natureza_voo,
          ds_servico_tipo_linha %in% !!input$tipo_servico
        )
    })
    
    return(dados_filtrados)
  })
}
    
## To be copied in the UI
# mod_filtros_ui("filtros_1")
    
## To be copied in the server
# mod_filtros_server("filtros_1")
