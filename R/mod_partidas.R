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
              reactable::reactableOutput(ns("tab_menores_atrasos"))
            )
          ),
          bslib::card(
            bslib::card_header(
              bslib::card_title("Maiores atrasos")
            ),
            bslib::card_body(
              reactable::reactableOutput(ns("tab_maiores_atrasos"))
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

    tab_aeroportos <- dplyr::tbl(con, "tab_aeroportos")

    dados_filtrados <- mod_filtro_periodo_server(
      "filtro_periodo_1",
      con
    )

    output$tab_aeroportos_voos <- reactable::renderReactable({
      dados_filtrados() |>
        dplyr::group_by(origin_airport) |>
        dplyr::summarise(n = dplyr::n()) |>
        # dplyr::arrange(desc(n)) |>
        dplyr::left_join(
          tab_aeroportos,
          by = c("origin_airport" = "airport_cod")
        ) |>
        dplyr::filter(!is.na(airport_name)) |>
        dplyr::select(
          airport_name,
          city,
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
            airport_name = reactable::colDef(
              name = "Aeroporto",
              minWidth = 250
            ),
            n = reactable::colDef(
              name = "Número de vôos",
              align = "right"
            ),
            city = reactable::colDef(
              name = "Cidade",
              minWidth = 120
            )
          )
        )
    })

    dados_atraso <- reactive({
      dados_filtrados() |>
        dplyr::mutate(
          data_hora_plan = paste(planned_departure_date, planned_departure_date_hm),
          data_hora_real = paste(actual_departure_date, actual_departure_date_hm)
        ) |>
        dplyr::select(
          origin_airport,
          data_hora_plan,
          data_hora_real
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          data_hora_plan = lubridate::ymd_hm(data_hora_plan),
          data_hora_real = lubridate::ymd_hm(data_hora_real),
          atraso = data_hora_real - data_hora_plan,
          atraso = as.numeric(atraso) / 60
        ) |>
        dplyr::group_by(origin_airport) |>
        dplyr::summarise(
          atraso_medio = mean(atraso, na.rm = TRUE)
        ) |>
        dplyr::left_join(
          dplyr::collect(tab_aeroportos),
          by = c("origin_airport" = "airport_cod")
        ) |>
        dplyr::mutate(
          atraso_medio = formatar_numero(atraso_medio)
        ) |> 
        tidyr::drop_na(airport_name)
    })

    output$tab_menores_atrasos <- reactable::renderReactable({
      dados_atraso() |>
        dplyr::arrange(atraso_medio) |>
        dplyr::mutate(
          rank = dplyr::row_number()
        ) |>
        dplyr::select(
          rank,
          airport_name,
          city,
          atraso_medio
        ) |> 
        reactable_padrao(
          searchable = TRUE,
          columns = list(
            rank = reactable::colDef(
              name = "",
              align = "right",
              maxWidth = 60,
            ),
            airport_name = reactable::colDef(
              name = "Aeroporto",
              minWidth = 200
            ),
            atraso_medio = reactable::colDef(
              name = "Atraso médio (min)",
              align = "right",
              minWidth = 120
            ),
            city = reactable::colDef(
              name = "Cidade",
              minWidth = 120
            )
          )
        )
    })

    output$tab_maiores_atrasos <- reactable::renderReactable({
      dados_atraso() |>
        dplyr::arrange(desc(atraso_medio)) |>
        dplyr::mutate(
          rank = dplyr::row_number()
        ) |>
        dplyr::select(
          rank,
          airport_name,
          city,
          atraso_medio
        ) |> 
        reactable_padrao(
          searchable = TRUE,
          columns = list(
            rank = reactable::colDef(
              name = "",
              align = "right",
              maxWidth = 60,
            ),
            airport_name = reactable::colDef(
              name = "Aeroporto",
              minWidth = 200
            ),
            atraso_medio = reactable::colDef(
              name = "Atraso médio (min)",
              align = "right",
              minWidth = 120
            ),
            city = reactable::colDef(
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
