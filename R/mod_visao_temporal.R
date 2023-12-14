#' visao_temporal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visao_temporal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        mod_filtros_ui(ns("filtros_1"))
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = "Sazonalidade anual",
          bslib::card_body(
            echarts4r::echarts4rOutput(ns("ec_sazo_anual"))
          )
        ),
        bslib::nav_panel(
          title = "Sazonalidade mensal",
          bslib::card_body(
            echarts4r::echarts4rOutput(ns("ec_sazo_mensal"))
          )
        ),
        bslib::nav_panel(
          title = "Sazonalidade semanal",
          bslib::card_body(
            echarts4r::echarts4rOutput(ns("ec_sazo_semanal"))
          )
        ),
        bslib::nav_panel(
          title = "Sazonalidade diária",
          bslib::card_body(
            echarts4r::echarts4rOutput(ns("ec_sazo_diaria"))
          )
        )
      )
    )
  )
}

#' visao_temporal Server Functions
#'
#' @noRd
mod_visao_temporal_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dados_filtrados <- mod_filtros_server("filtros_1", con)

    output$ec_sazo_anual <- echarts4r::renderEcharts4r({
      dados_filtrados() |>
        dplyr::filter(nr_mes_partida_real != "") |>
        dplyr::count(nr_ano_partida_real, nr_mes_partida_real) |>
        dplyr::group_by(nr_mes_partida_real) |>
        dplyr::summarise(
          media_voos = mean(n, na.rm = TRUE)
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          nr_mes_partida_real = as.numeric(nr_mes_partida_real)
        ) |>
        echarts4r::e_charts(x = nr_mes_partida_real) |>
        echarts4r::e_bar(serie = media_voos)
    })

    output$ec_sazo_mensal <- echarts4r::renderEcharts4r({
      dados_filtrados() |>
        dplyr::filter(nr_dia_partida_real != "") |>
        dplyr::count(nr_dia_partida_real, nr_mes_partida_real, nr_ano_partida_real) |>
        dplyr::group_by(nr_dia_partida_real) |>
        dplyr::summarise(
          media_voos = mean(n, na.rm = TRUE)
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          nr_dia_partida_real = as.numeric(nr_dia_partida_real)
        ) |>
        echarts4r::e_charts(x = nr_dia_partida_real) |>
        echarts4r::e_bar(serie = media_voos)
    })

    output$ec_sazo_semanal <- echarts4r::renderEcharts4r({
      dados_filtrados() |>
        dplyr::filter(nm_dia_semana_partida_real != "") |>
        dplyr::count(nm_dia_semana_partida_real, nr_semana_partida_real, nr_ano_partida_real) |>
        dplyr::group_by(nm_dia_semana_partida_real) |>
        dplyr::summarise(
          media_voos = mean(n, na.rm = TRUE)
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          nm_dia_semana_partida_real = forcats::lvls_reorder(
            nm_dia_semana_partida_real,
            c(1, 5, 7, 2, 3, 6, 4)
          )
        ) |>
        dplyr::arrange(nm_dia_semana_partida_real) |>
        echarts4r::e_charts(x = nm_dia_semana_partida_real) |>
        echarts4r::e_bar(serie = media_voos)
    })

    output$ec_sazo_diaria <- echarts4r::renderEcharts4r({
      dados_filtrados() |>
        dplyr::filter(hr_partida_real != "") |>
        dplyr::mutate(
          hr_partida_real = stringr::str_sub(hr_partida_real, 1, 2)
        ) |>
        dplyr::count(
          hr_partida_real,
          nr_dia_partida_real,
          nr_mes_partida_real,
          nr_ano_partida_real
        ) |>
        dplyr::group_by(hr_partida_real) |>
        dplyr::summarise(
          media_voos = mean(n, na.rm = TRUE)
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          nr_mes_partida_real = as.numeric(hr_partida_real)
        ) |>
        echarts4r::e_charts(x = hr_partida_real) |>
        echarts4r::e_bar(serie = media_voos)
    })
  })
}

## To be copied in the UI
# mod_visao_temporal_ui("visao_temporal_1")

## To be copied in the server
# mod_visao_temporal_server("visao_temporal_1")
