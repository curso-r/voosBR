formatar_numero <- function(x, acc = 1) {
  scales::number(
    x,
    accuracy = acc,
    big.mark = ".",
    decimal.mark = ","
  )
}


contar_linhas <- function(tab) {
  tab |>
    dplyr::summarise(
      n = dplyr::n()
    ) |>
    dplyr::pull(n)
}

