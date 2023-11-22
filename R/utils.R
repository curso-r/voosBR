formatar_numero <- function(x, acc = 1) {
  scales::number(
    x,
    accuracy = acc,
    big.mark = ".",
    decimal.mark = ","
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
