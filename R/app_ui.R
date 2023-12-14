#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "voosBR",
      bslib::nav_panel(
        title = "Visão geral",
        mod_visao_geral_ui("visao_geral_1")
      ),
      bslib::nav_menu(
        title = "Voos",
        bslib::nav_panel(
          title = "Partidas",
          mod_voos_ui("voos_partida")
        ),
        bslib::nav_panel(
          title = "Chegadas",
          mod_voos_ui("voos_chegada")
        )
      ),
      bslib::nav_panel(
        title = "Visão temporal",
        mod_visao_temporal_ui("visao_temporal_1")
      ),
      bslib::nav_panel(
        title = "Aeroportos",
        mod_aeroportos_ui("aeroportos_1")
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "voosBR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
