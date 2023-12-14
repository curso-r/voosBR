#' aeroportos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_aeroportos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        class = "overflow-visible",
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
          inputId = ns("uf"),
          label = "Selecione um estado",
          choices = c("Carregando..." = "")
        ),
        picker_input(
          inputId = ns("aeroporto"),
          label = "Selecione um aeroporto",
          choices = c("Carregando..." = "")
        ),
        actionButton(
          inputId = ns("filtrar"),
          label = "Aplicar filtros"
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        uiOutput(ns("media_passageiros")),
        uiOutput(ns("media_lotacao")),
        uiOutput(ns("media_peso_bagagem"))
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(
            bslib::card_title("Destinos mais frequêntes"),
            reactable::reactableOutput(ns("tabela_destinos"))
          )
        ),
        bslib::card(
          bslib::card_header(
            bslib::card_title("Origens mais frequêntes"),
            reactable::reactableOutput(ns("tabela_origens"))
          )
        )
      )
    )
  )
}

#' aeroportos Server Functions
#'
#' @noRd
mod_aeroportos_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
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

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "uf",
      choices = tab_voos |>
        dplyr::filter(nm_pais_origem == "BRASIL") |>
        dplyr::distinct(sg_uf_origem) |>
        dplyr::filter(sg_uf_origem != "") |>
        dplyr::pull(sg_uf_origem) |>
        sort()
    )

    observe({
      aeroportos <- tab_voos |>
        dplyr::filter(nm_pais_origem == "BRASIL", sg_uf_origem == !!input$uf) |>
        dplyr::distinct(nm_aerodromo_origem) |>
        dplyr::pull(nm_aerodromo_origem) |>
        sort()

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "aeroporto",
        choices = aeroportos
      )
    })


    dados_filtrados <- reactive({
      req(input$aeroporto)
      tab_voos |>
        dplyr::filter(
          dt_partida_real <= !!input$periodo[2],
          dt_partida_real >= !!input$periodo[1],
          sg_uf_origem == !!input$uf,
          nm_aerodromo_origem == !!input$aeroporto,
           ds_servico_tipo_linha  == "PASSAGEIRO"
        )
    })

    output$media_passageiros <- renderUI({
      dados_filtrados() |>
        dplyr::summarise(
          media_passageiros = mean(nr_passag_pagos + nr_passag_gratis, na.rm = TRUE)
        ) |>
        dplyr::pull(media_passageiros) |>
        round() |>
        bslib::value_box(
          title = "Média de passageiros",
          showcase = bsicons::bs_icon("people-fill")
        )
    })
  })
}

## To be copied in the UI
# mod_aeroportos_ui("aeroportos_1")

## To be copied in the server
# mod_aeroportos_server("aeroportos_1")
