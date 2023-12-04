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
      fillable = FALSE,
      sidebar = bslib::sidebar(
        mod_filtro_periodo_ui(ns("filtro_periodo_1"))
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
        bslib::navset_card_tab(
          title = "Aeroportos com mais vôos",
          bslib::nav_panel(
            title = "Partidas",
            bslib::card_body(
              reactable::reactableOutput(ns("tabela_aeroportos_part"))
            )
          ),
          bslib::nav_panel(
            title = "Chegadas",
            bslib::card_body(
              reactable::reactableOutput(ns("tabela_aeroportos_cheg"))
            )
          )
        ),
        bslib::card(
          bslib::card_header("Empresas aéreas com mais vôos"),
          bslib::card_body(
            reactable::reactableOutput(ns("tabela_empresas"))
          )
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

    dados_filtrados <- mod_filtro_periodo_server(
      "filtro_periodo_1",
      con
    )

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
        ) |>
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

    output$tabela_aeroportos_part <- reactable::renderReactable({
      dados_filtrados() |>
        dplyr::group_by(origin_airport) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::arrange(desc(n)) |>
        head(5) |>
        dplyr::left_join(
          tab_aeroportos,
          by = c("origin_airport" = "airport_cod")
        ) |>
        dplyr::collect() |>
        dplyr::select(
          airport_name,
          city,
          n
        ) |>
        dplyr::mutate(
          n = formatar_numero(n)
        ) |>
        reactable_padrao(
          columns = list(
            airport_name = reactable::colDef(
              name = "Aeroporto",
              minWidth = 250
            ),
            n = reactable::colDef(
              name = "Número de vôos",
              align = "right"
            ),
            city = reactable::colDef(
              name = "Cidade"
            )
          )
        )
    })

    output$tabela_aeroportos_cheg <- reactable::renderReactable({
      dados_filtrados() |>
        dplyr::group_by(destination_airport) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::arrange(desc(n)) |>
        head(5) |>
        dplyr::left_join(
          tab_aeroportos,
          by = c("destination_airport" = "airport_cod")
        ) |>
        dplyr::collect() |>
        dplyr::select(
          airport_name,
          city,
          n
        ) |>
        dplyr::mutate(
          n = formatar_numero(n)
        ) |>
        reactable_padrao(
          columns = list(
            airport_name = reactable::colDef(
              name = "Aeroporto",
              minWidth = 250
            ),
            n = reactable::colDef(
              name = "Número de vôos",
              align = "right"
            ),
            city = reactable::colDef(
              name = "Cidade"
            )
          )
        )
    })

    output$tabela_empresas <- reactable::renderReactable({
      dados_filtrados() |>
        dplyr::group_by(airline) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::arrange(desc(n)) |>
        head(5) |>
        dplyr::left_join(
          tab_empresas,
          by = c("airline" = "airline_cod")
        ) |>
        dplyr::select(
          airline_name,
          n
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          n = formatar_numero(n)
        ) |>
        reactable_padrao(
          columns = list(
            airline_name = reactable::colDef(
              name = "Empresa aérea"
            ),
            n = reactable::colDef(
              name = "Número de vôos",
              align = "right"
            )
          )
        )
    })
  })
}

## To be copied in the UI
# mod_visao_geral_ui("visao_geral_1")

## To be copied in the server
# mod_visao_geral_server("visao_geral_1")
