formatar_numero <- function(x, acc = 1) {
  scales::number(
    x,
    accuracy = acc,
    big.mark = ".",
    decimal.mark = ","
  )
}

picker_input <- function(inputId, ...) {
  shinyWidgets::pickerInput(
    inputId = inputId,
    ...,
    options = list(
      `actions-box` = TRUE
    )
  )
}

reactable_padrao <- function(tab, ...) {
  tab |>
    reactable::reactable(
      ...,
      defaultColDef = reactable::colDef(
        style = list(
          fontSize = "14px"
        )
      )
    )
}

ver_a_base <- function() {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "flightsbr.sqlite")
  dplyr::tbl(con, "tab_voos") |>
    dplyr::glimpse()
}

ver_coluna <- function(...) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "flightsbr.sqlite")
  dplyr::tbl(con, "tab_voos") |>
    dplyr::distinct(...)
}


contar_linhas <- function(tab) {
  tab |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull(n)
}

sidebar_custom <- function(...) {
  bslib::sidebar(
    ...,
    bg = "#071e41"
  )
}
