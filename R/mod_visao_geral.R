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
        dplyr::group_by(dt_partida_real) |>
        dplyr::summarise(n = n()) |>
        dplyr::collect() |>
        dplyr::mutate(
          dt_partida_real = lubridate::as_date(dt_partida_real)
        ) |>
        echarts4r::e_charts(x = dt_partida_real) |>
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
        dplyr::group_by(nm_aerodromo_origem, nm_municipio_origem) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::arrange(desc(n)) |>
        head(5) |>
        dplyr::collect() |>
        dplyr::select(
          nm_aerodromo_origem,
          nm_municipio_origem,
          n
        ) |>
        dplyr::mutate(
          n = formatar_numero(n)
        ) |>
        reactable_padrao(
          columns = list(
            nm_aerodromo_origem = reactable::colDef(
              name = "Aeroporto",
              minWidth = 250
            ),
            n = reactable::colDef(
              name = "Número de vôos",
              align = "right"
            ),
            nm_municipio_origem = reactable::colDef(
              name = "Cidade"
            )
          )
        )
    })

    output$tabela_aeroportos_cheg <- reactable::renderReactable({
      dados_filtrados() |>
        dplyr::group_by(nm_aerodromo_destino, nm_municipio_destino) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::arrange(desc(n)) |>
        head(5) |>
        dplyr::collect() |>
        dplyr::select(
          nm_aerodromo_destino,
          nm_municipio_destino,
          n
        ) |>
        dplyr::mutate(
          n = formatar_numero(n)
        ) |>
        reactable_padrao(
          columns = list(
            nm_aerodromo_destino = reactable::colDef(
              name = "Aeroporto",
              minWidth = 250
            ),
            n = reactable::colDef(
              name = "Número de vôos",
              align = "right"
            ),
            nm_municipio_destino = reactable::colDef(
              name = "Cidade"
            )
          )
        )
    })

    output$tabela_empresas <- reactable::renderReactable({
      dados_filtrados() |>
        dplyr::group_by(nm_empresa) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::arrange(desc(n)) |>
        head(5) |>
        dplyr::collect() |>
        dplyr::mutate(
          n = formatar_numero(n)
        ) |>
        reactable_padrao(
          columns = list(
            nm_empresa = reactable::colDef(
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
