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
      sidebar = sidebar_custom(
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

    datas <- dplyr::tbl(con, "tab_datas") |>
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
      choices = dplyr::tbl(con, "tab_estados") |>
        dplyr::pull(sg_uf_origem) |>
        sort()
    )

    observe({
      aeroportos <- dplyr::tbl(con, "tab_aeroportos") |>
        dplyr::filter(sg_uf_origem == !!input$uf) |>
        dplyr::pull(nm_aerodromo_origem) |>
        sort()

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "aeroporto",
        choices = aeroportos
      )
    })


    dados_filtrados <- eventReactive(input$filtrar, {
      req(input$aeroporto)
      tab_voos |>
        dplyr::filter(
          dt_partida_real <= !!input$periodo[2],
          dt_partida_real >= !!input$periodo[1],
          sg_uf_origem == !!input$uf,
          nm_aerodromo_origem == !!input$aeroporto,
          ds_servico_tipo_linha == "PASSAGEIRO"
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
          showcase = bsicons::bs_icon("people-fill"),
          theme = bslib::value_box_theme(
            bg = "#071e41",
            fg = "#fff"
          )
        )
    })

    output$media_lotacao <- renderUI({
      dados_filtrados() |>
        dplyr::mutate(
          num_passageiros = nr_passag_pagos + nr_passag_gratis,
          nr_assentos_ofertados = as.numeric(nr_assentos_ofertados)
        ) |>
        dplyr::mutate(
          lotacao = num_passageiros / nr_assentos_ofertados
        ) |>
        dplyr::select(num_passageiros, nr_assentos_ofertados, lotacao) |>
        dplyr::summarise(
          locacao_media = mean(num_passageiros / nr_assentos_ofertados, na.rm = TRUE)
        ) |>
        dplyr::pull(locacao_media) |>
        scales::percent(accuracy = 0.1) |>
        bslib::value_box(
          title = "Lotação média",
          showcase = bsicons::bs_icon("airplane-fill"),
          theme = bslib::value_box_theme(
            bg = "#071e41",
            fg = "#fff"
          )
        )
    })

    output$media_peso_bagagem <- renderUI({
      dados_filtrados() |>
        dplyr::summarise(
          peso_medio = mean(kg_bagagem_livre + kg_bagagem_excesso, na.rm = TRUE)
        ) |>
        dplyr::pull(peso_medio) |>
        round() |>
        bslib::value_box(
          title = "Peso médio de bagagem",
          showcase = bsicons::bs_icon("bag-fill"),
          theme = bslib::value_box_theme(
            bg = "#071e41",
            fg = "#fff"
          )
        )
    })

    output$tabela_destinos <- reactable::renderReactable({
      dados_filtrados() |>
        dplyr::group_by(nm_aerodromo_destino, sg_uf_destino) |>
        dplyr::summarise(
          voos = dplyr::n()
        ) |>
        dplyr::arrange(desc(voos)) |>
        head(10) |>
        dplyr::collect() |>
        reactable::reactable(
          columns = list(
            nm_aerodromo_destino = reactable::colDef(
              name = "Aeroporto",
              minWidth = 200
            ),
            sg_uf_destino = reactable::colDef(
              name = "Estado",
              minWidth = 100
            ),
            voos = reactable::colDef(
              name = "Número de voos",
              minWidth = 100
            )
          ),
          defaultColDef = reactable::colDef(
            minWidth = 100
          ),
          striped = TRUE
        )
    })

    dados_filtrados_destino <- eventReactive(input$filtrar, {
      req(input$aeroporto)
      tab_voos |>
        dplyr::filter(
          dt_partida_real <= !!input$periodo[2],
          dt_partida_real >= !!input$periodo[1],
          sg_uf_destino == !!input$uf,
          nm_aerodromo_destino == !!input$aeroporto,
          ds_servico_tipo_linha == "PASSAGEIRO"
        ) 
    })

    output$tabela_origens <- reactable::renderReactable({
      dados_filtrados_destino() |>
        dplyr::group_by(nm_aerodromo_origem, sg_uf_origem) |>
        dplyr::summarise(
          voos = dplyr::n()
        ) |>
        dplyr::arrange(desc(voos)) |>
        head(10) |>
        dplyr::collect() |>
        reactable::reactable(
          columns = list(
            nm_aerodromo_origem = reactable::colDef(
              name = "Aeroporto",
              minWidth = 200
            ),
            sg_uf_origem = reactable::colDef(
              name = "Estado",
              minWidth = 100
            ),
            voos = reactable::colDef(
              name = "Número de voos",
              minWidth = 100
            )
          ),
          defaultColDef = reactable::colDef(
            minWidth = 100
          ),
          striped = TRUE
        )
    })
  })
}

## To be copied in the UI
# mod_aeroportos_ui("aeroportos_1")

## To be copied in the server
# mod_aeroportos_server("aeroportos_1")
