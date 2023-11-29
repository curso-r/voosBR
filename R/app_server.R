#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  con <- RSQLite::dbConnect(RSQLite::SQLite(), "brflights.sqlite")

  mod_visao_geral_server("visao_geral_1", con)
  mod_partidas_server("partidas_1", con)

}
