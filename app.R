library(shiny)
library(ggplot2)

# dados <- readRDS("data/brflights.rds")
# dplyr::glimpse(dados)

ui <- fluidPage(
  titlePanel("Painel de vôos do Brasil"),
  hr(),
  selectInput(
    inputId = "ano",
    label = "Selecione um ano",
    # choices = unique(c(
    #   dados$planned_departure_date_year,
    #   dados$actual_arrival_date_year
    # ))
    choices = 2019:2023
  ),
  plotOutput("numero_voos_saem_dia"),
  hr(),
  dateRangeInput(
    inputId = "periodo",
    label = "Selecione um período",
    start = "2023-01-01",
    end = "2023-01-01",
    min = "2019-01-01",
    max = "2023-07-31",
    language = "pt-BR",
    format = "dd/mm/yyyy",
    separator = "a"
  ),
  tableOutput("tabela_voos")
)

server <- function(input, output, session) {

  output$numero_voos_saem_dia <- renderPlot({
    dados |>
      dplyr::filter(
        actual_departure_date_year == input$ano
      ) |>
      dplyr::mutate(
        actual_departure_date = as.Date(actual_departure_date)
      ) |>
      dplyr::count(actual_departure_date) |>
      ggplot(aes(x = actual_departure_date, y = n)) +
      geom_col(color = "black", fill = "royalblue") +
      theme_minimal()
  })


  output$tabela_voos <- renderTable({
    dados |>
      dplyr::mutate(
        actual_departure_date = as.Date(actual_departure_date)
      ) |>
      dplyr::filter(
        actual_departure_date >= input$periodo[1],
        actual_departure_date <= input$periodo[2]
      ) |>
      dplyr::slice(1:20) |>
      dplyr::mutate(
        actual_departure_date = format(actual_departure_date, "%d/%m/%Y"),
        actual_arrival_date = format(actual_arrival_date, "%d/%m/%Y")
      ) |>
      dplyr::select(
        airline,
        flight_number,
        origin_airport,
        destination_airport,
        actual_departure_date,
        actual_arrival_date
      )
  })


}

shinyApp(ui, server, options = list(launch.browser = FALSE, port = 4242))
