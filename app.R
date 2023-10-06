library(shiny)
library(bs4Dash)
library(ggplot2)

ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  header = bs4DashNavbar(
    title = "voosBR"
  ),
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        text = "Visão geral",
        tabName = "visao_geral",
        icon = icon("eye")
      )
    )
  ),
  body = bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css")
    ),
    bs4TabItems(
      bs4TabItem(
        tabName = "visao_geral",
        titlePanel("Visão geral"),
        hr(),
        fluidRow(
          bs4ValueBoxOutput(
            "vg_num_voos",
            width = 4
          ),
          bs4ValueBoxOutput(
            "vg_num_voos_domesticos",
            width = 4
          ),
          bs4ValueBoxOutput(
            "vg_num_voos_inter"
          )
        ),
        fluidRow(
          bs4Card(
            width = 12,
            title = "Número de voos domésticos e internacionais",
            plotOutput("vg_serie_voos") |>
              shinycssloaders::withSpinner(),
            collapsible = FALSE
          )
        ),
        fluidRow(
          bs4TabCard(
            width = 6,
            height = 642,
            title = "Aeroportos com mais voos",
            side = "right",
            collapsible = FALSE,
            tabPanel(
              title = "Partidas",
              tableOutput("vg_tab_aero_partidas")
            ),
            tabPanel(
              title = "Chegadas",
              tableOutput("vg_tab_aero_chegadas")
            )
          ),
          bs4Card(
            width = 6,
            height = 660,
            title = "Empresas com mais voos",
            side = "right",
            collapsible = FALSE,
            tableOutput("vg_tab_emp")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  con <- RSQLite::dbConnect(
    RSQLite::SQLite(),
    "brflights.sqlite"
  )

  tab_voos <- dplyr::tbl(con, "tab_voos")
  tab_aeroportos <- dplyr::tbl(con, "tab_aeroportos")
  tab_empresas <- dplyr::tbl(con, "tab_empresas")

  output$vg_num_voos <- renderbs4ValueBox({
    valor <- tab_voos |>
      contar_linhas() |>
      formatar_numero()
    bs4ValueBox(
      value = valor,
      subtitle = "Total de voos",
      icon = icon("plane"),
      color = "lightblue"
    )
  })

  output$vg_num_voos_domesticos <- renderbs4ValueBox({
    valor <- tab_voos |>
      dplyr::filter(flight_type == "N") |>
      contar_linhas() |>
      formatar_numero()


    bs4ValueBox(
      value = valor,
      subtitle = "Número de voos domésticos",
      icon = icon("home"),
      color = "lightblue"
    )
  })

  output$vg_num_voos_inter <- renderbs4ValueBox({
    valor <- tab_voos |>
      dplyr::filter(flight_type == "I") |>
      contar_linhas() |>
      formatar_numero()


    bs4ValueBox(
      value = valor,
      subtitle = "Número de voos internacionais",
      icon = icon("globe"),
      color = "lightblue"
    )
  })

  output$vg_serie_voos <- renderPlot({
    tab_voos |>
      dplyr::count(flight_type, planned_departure_date_ym) |>
      dplyr::filter(
        !is.na(planned_departure_date_ym)
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        data = as.Date(planned_departure_date_ym)
      ) |>
      ggplot(aes(x = data, y = n, color = flight_type)) +
      geom_line() +
      theme_minimal()

  })

  output$vg_tab_aero_partidas <- renderTable({
    tab_voos |>
      dplyr::count(origin_airport, sort = TRUE) |>
      head(10) |>
      dplyr::left_join(
        tab_aeroportos,
        by = c("origin_airport" = "airport_cod")
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        n = formatar_numero(n)
      ) |>
      dplyr::select(
       Aeroporto = airport_name,
       cidade = city,
       `Número de voos` = n
      )

  })

  output$vg_tab_aero_chegadas <- renderTable({
    tab_voos |>
      dplyr::count(destination_airport, sort = TRUE) |>
      head(10) |>
      dplyr::left_join(
        tab_aeroportos,
        by = c("destination_airport" = "airport_cod")
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        n = formatar_numero(n)
      ) |>
      dplyr::select(
        Aeroporto = airport_name,
        cidade = city,
        `Número de voos` = n
      )
  })

  output$vg_tab_emp <- renderTable({
    tab_voos |>
      dplyr::count(airline, sort = TRUE) |>
      head(10) |>
      dplyr::left_join(
        tab_empresas,
        by = c("airline" = "airline_cod")
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        n = formatar_numero(n)
      ) |>
      dplyr::select(
        `Empresa aérea` = airline_name,
        `Número de voos` = n
      )
  })


}

shinyApp(ui, server, options = list(launch.browser = FALSE, port = 4242))
