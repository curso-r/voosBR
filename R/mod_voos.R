#' voos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_voos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        mod_filtros_ui(ns("filtros_1"))
      ),
      bslib::card(
        bslib::card_header(
          bslib::card_title("Aeroportos com mais vôos")
        ),
        bslib::card_body(
          reactable::reactableOutput(ns("tab_aeroportos_voos"))
        )
      )
    )
  )
}
    
#' voos Server Functions
#'
#' @noRd 
mod_voos_server <- function(id, con, tipo_aeroporto) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dados_filtrados <- mod_filtros_server("filtros_1", con)

    output$tab_aeroportos_voos <- reactable::renderReactable({
      dados_filtrados() |>
        dplyr::rename(
          nm_aerodromo = glue::glue("nm_aerodromo_{tipo_aeroporto}"),
          nm_municipio = glue::glue("nm_municipio_{tipo_aeroporto}")
        ) |> 
        dplyr::group_by(nm_aerodromo, nm_municipio) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::filter(!is.na(nm_aerodromo)) |>
        dplyr::select(
          nm_aerodromo,
          nm_municipio,
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
          defaultPageSize = 15,
          columns = list(
            rank = reactable::colDef(
              name = "",
              align = "right",
              maxWidth = 60,
            ),
            nm_aerodromo = reactable::colDef(
              name = "Aeroporto",
              minWidth = 250
            ),
            n = reactable::colDef(
              name = "Número de vôos",
              align = "right"
            ),
            nm_municipio = reactable::colDef(
              name = "Cidade",
              minWidth = 120
            )
          )
        )
    })
 
  })
}
    
## To be copied in the UI
# mod_voos_ui("voos_1")
    
## To be copied in the server
# mod_voos_server("voos_1")
