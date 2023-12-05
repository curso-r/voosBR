#' partidas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_partidas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        mod_filtro_periodo_ui(ns("filtro_periodo_1"))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(
            bslib::card_title("Aeroportos com mais vôos")
          ),
          bslib::card_body(
            reactable::reactableOutput(ns("tab_aeroportos_voos"))
          )
        ),
        tagList(
          bslib::card(
            bslib::card_header(
              bslib::card_title("Menores atrasos")
            ),
            bslib::card_body(
              # reactable::reactableOutput(ns("tab_menores_atrasos"))
            )
          ),
          bslib::card(
            bslib::card_header(
              bslib::card_title("Maiores atrasos")
            ),
            bslib::card_body(
              # reactable::reactableOutput(ns("tab_maiores_atrasos"))
            )
          )
        )
      )
    )
  )
}

#' partidas Server Functions
#'
#' @noRd
mod_partidas_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dados_filtrados <- mod_filtro_periodo_server(
      "filtro_periodo_1",
      con
    )

    output$tab_aeroportos_voos <- reactable::renderReactable({
      dados_filtrados() |>
        dplyr::group_by(nm_aerodromo_origem, nm_municipio_origem) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::filter(!is.na(nm_aerodromo_origem)) |>
        dplyr::select(
          nm_aerodromo_origem,
          nm_municipio_origem,
          n
        ) |>
        dplyr::arrange(desc(n)) |>
        dplyr::collect() |>
        dplyr::mutate(
          rank = dplyr::row_number(),
          n = formatar_numero(n),
          .before = 1
        ) |>
        reactable_padrao(
          searchable = TRUE,
          columns = list(
            rank = reactable::colDef(
              name = "",
              align = "right",
              maxWidth = 60,
            ),
            nm_aerodromo_origem = reactable::colDef(
              name = "Aeroporto",
              minWidth = 250
            ),
            n = reactable::colDef(
              name = "Número de vôos",
              align = "right"
            ),
            nm_municipio_origem = reactable::colDef(
              name = "Cidade",
              minWidth = 120
            )
          )
        )
    })

  })
}

## To be copied in the UI
# mod_partidas_ui("partidas_1")

## To be copied in the server
# mod_partidas_server("partidas_1")
