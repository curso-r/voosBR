#' visao_geral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visao_geral_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
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
      ),
      bslib::card(
        bslib::card_header(
          "Série histórica do número de vôos"
        ),
        bslib::card_body(
          echarts4r::echarts4rOutput(ns("serie_historica"))
        )
      ),
      bslib::layout_columns(
        bslib::card(
          bslib::card_header("Aeroportos com mais vôos")
        ),
        bslib::card(
          bslib::card_header("Empresas aéreas com mais vôos")
        )
      )
    )
  )
}

#' visao_geral Server Functions
#'
#' @noRd
mod_visao_geral_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tab_voos <- dplyr::tbl(con, "tab_voos")

    data_min <- tab_voos |>
      dplyr::summarise(
        data_min = min(planned_departure_date, na.rm = TRUE)
      ) |>
      dplyr::pull(data_min)

    data_max <- tab_voos |>
      dplyr::summarise(
        data_max = max(planned_departure_date, na.rm = TRUE)
      ) |>
      dplyr::pull(data_max)

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

    output$serie_historica <- echarts4r::renderEcharts4r({
      dados_filtrados() |>
        dplyr::group_by(planned_departure_date) |>
        dplyr::summarise(n = n()) |>
        dplyr::collect() |> 
        dplyr::mutate(
          planned_departure_date = lubridate::as_date(planned_departure_date)
        ) |>
        echarts4r::e_charts(x = planned_departure_date) |>
        echarts4r::e_bar(serie = n) |> 
        echarts4r::e_tooltip(trigger = "axis") |> 
        echarts4r::e_datazoom(
          type = "slider",
          start = 0,
          end = 100
        ) |> 
        echarts4r::e_grid(
          containLabel = TRUE
        )  |> 
        echarts4r::e_toolbox(
          show = TRUE,
          feature = list(
            dataZoom = list(
              yAxisIndex = "none"
            ),
            restore = list(
              title = "Restaurar"
            ),
            saveAsImage = list(
              title = "Salvar como imagem"
            )
          )
        ) |> 
        echarts4r::e_legend(show = FALSE)
    })
  })
}

## To be copied in the UI
# mod_visao_geral_ui("visao_geral_1")

## To be copied in the server
# mod_visao_geral_server("visao_geral_1")
