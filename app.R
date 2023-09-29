library(shiny)
library(ggplot2)

ui <- navbarPage(
  title = "vôosBR",
  tabPanel(
    title = "Visão geral",
    titlePanel("Visão geral"),
    hr(),
    dateRangeInput(
      inputId = "vg_periodo",
      label = "Selecione um período",
      start = "2019-01-01",
      end = "2023-07-31",
      min = "2019-01-01",
      max = "2023-07-31",
      language = "pt-BR",
      format = "dd/mm/yyyy",
      separator = "a"
    ),
    fluidRow(
      column(
        width = 4,
        # num voos total
        # num partidas
        # num chegadas
        # num empresas aereas que atuaram no periodo
        # num aeroportos com voos no periodo
      ),
      column(
        width = 8,
        plotOutput("vg_serie_historica_partidas"),
        plotOutput("vg_serie_historica_chegadas")
      )
    )
  ),
  tabPanel(
    title = "Partidas"
  ),
  tabPanel(
    title = "Chegadas"
  ),
  tabPanel(
    title = "Aeroportos"
  ),
  tabPanel(
    title = "Companhias aéreas"
  ),
  tabPanel(
    title = "Localidade"
  ),
  tabPanel(
    title = "Sobre"
  )
)

# ui <- fluidPage(
#   titlePanel("Painel de vôos do Brasil"),
#   hr(),
#   selectInput(
#     inputId = "ano",
#     label = "Selecione um ano",
#     # choices = unique(c(
#     #   dados$planned_departure_date_year,
#     #   dados$actual_arrival_date_year
#     # ))
#     choices = 2019:2023
#   ),
#   plotOutput("numero_voos_por_mes"),
#   hr(),
#   dateRangeInput(
#     inputId = "periodo",
#     label = "Selecione um período",
#     start = "2023-01-01",
#     end = "2023-01-01",
#     min = "2019-01-01",
#     max = "2023-07-31",
#     language = "pt-BR",
#     format = "dd/mm/yyyy",
#     separator = "a"
#   ),
#   tableOutput("tabela_voos")
# )

server <- function(input, output, session) {

  con <- RSQLite::dbConnect(
    RSQLite::SQLite(),
    "brflights.sqlite"
  )

  tab_voos <- dplyr::tbl(con, "tab_voos")

  output$vg_serie_historica_partidas <- renderPlot({
    tab_voos |>
      dplyr::filter(
        actual_departure_date >= !!input$vg_periodo[1],
        actual_departure_date <= !!input$vg_periodo[2]
      ) |>
      dplyr::count(planned_departure_date_ym) |>
      dplyr::collect() |>
      dplyr::mutate(
        planned_departure_date_ym = as.Date(planned_departure_date_ym)
      ) |>
      ggplot(aes(x = planned_departure_date_ym, y = n)) +
      geom_line(color = "black", fill = "royalblue") +
      theme_minimal()
  })

  # output$tabela_voos <- renderTable({
  #
  #   tab_voos |>
  #     dplyr::filter(
  #       actual_departure_date >= !!input$periodo[1],
  #       actual_departure_date <= !!input$periodo[2],
  #     ) |>
  #     head(20) |>
  #     dplyr::select(
  #       airline,
  #       flight_number,
  #       origin_airport,
  #       destination_airport,
  #       actual_departure_date,
  #       actual_arrival_date
  #     ) |>
  #     dplyr::collect()
  #
  # })


}

shinyApp(ui, server, options = list(launch.browser = FALSE, port = 4242))
