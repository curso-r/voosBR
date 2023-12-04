#' filtro_periodo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filtro_periodo_ui <- function(id) {
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
    )
  )
}

#' filtro_periodo Server Functions
#'
#' @noRd
mod_filtro_periodo_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tab_voos <- dplyr::tbl(con, "tab_voos")

    datas <- tab_voos |>
      dplyr::summarise(
        data_min = min(dt_partida_real, na.rm = TRUE),
        data_max = max(dt_partida_real, na.rm = TRUE)
      )

    updateDateRangeInput(
      session = session,
      inputId = "periodo",
      start = data_min,
      end = data_max,
      min = data_min,
      max = data_max
    )

    dados_filtrados <- reactive({
      req(input$periodo[1] != input$periodo[2])
      tab_voos |>
        dplyr::filter(
          planned_departure_date <= !!input$periodo[2] &
            planned_departure_date >= !!input$periodo[1]
        )
    })

    return(dados_filtrados)
  })
}

## To be copied in the UI
# mod_filtro_periodo_ui("filtro_periodo_1")

## To be copied in the server
# mod_filtro_periodo_server("filtro_periodo_1")
