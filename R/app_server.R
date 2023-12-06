#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  con <- RSQLite::dbConnect(RSQLite::SQLite(), "flightsbr.sqlite")

  mod_visao_geral_server("visao_geral_1", con)
  mod_voos_server("voos_partida", con, tipo_aeroporto = "origem")
  mod_voos_server("voos_chegada", con, tipo_aeroporto = "destino")
  mod_visao_temporal_server("visao_temporal_1", con)
}
