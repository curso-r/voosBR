dados <- readRDS("data-raw/brflights.rds")

# Manipulacao

dados <- dados |>
  dplyr::mutate(
    dplyr::across(
      dplyr::ends_with("date"),
      ~ .x |> lubridate::dmy_hm()
    ),
    dplyr::across(
      dplyr::ends_with("date"),
      ~ .x |> lubridate::year(),
      .names = "{.col}_year"
    )
  )

readr::write_rds(dados, "data/brflights.rds")
