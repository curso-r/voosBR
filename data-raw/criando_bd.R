dados <- readRDS("data-raw/brflights.rds")

tab_voos <- dados |>
  dplyr::mutate(
    dplyr::across(
      dplyr::ends_with("date"),
      ~ .x |> lubridate::dmy_hm()
    ),
    year = lubridate::year(planned_departure_date),
    dplyr::across(
      dplyr::ends_with("date"),
      ~ .x |> lubridate::floor_date(unit = "months"),
      .names = "{.col}_ym"
    )
  ) |>
  dplyr::filter(
    lubridate::year(actual_departure_date) %in% 2019:2023,
    lubridate::year(actual_arrival_date) %in% 2019:2023
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::contains("date"),
      as.character
    )
  )


tab_aeroportos <- readxl::read_excel(
  "data-raw/glossario_de_aerodromo.xls",
  skip = 3
) |>
  janitor::clean_names() |>
  dplyr::rename(
    airport_cod = sigla_oaci,
    airport_name = descricao,
    city = cidade,
    state = uf,
    country = pais,
    continent = continente
  )

tab_empresas <- readxl::read_excel(
  "data-raw/glossario_de_empresas_aereas.xls",
  skip = 3
) |>
  janitor::clean_names() |>
  dplyr::select(
    airline_cod = sigla_oaci,
    airline_name = nome_empresas,
    flag_brazilian = nacional_ou_estrangeira
  ) |>
  dplyr::mutate(
    flag_brazilian = dplyr::case_when(
      is.na(flag_brazilian) |  flag_brazilian == "NÃO INFORMADO" ~ NA,
      flag_brazilian == "BRASILEIRA" ~ TRUE,
      TRUE ~ FALSE
    )
  )

con <- RSQLite::dbConnect(RSQLite::SQLite(), "brflights.sqlite")

RSQLite::dbWriteTable(con, "tab_voos", value = tab_voos)
RSQLite::dbWriteTable(con, "tab_aeroportos", value = tab_aeroportos)
RSQLite::dbWriteTable(con, "tab_empresas", value = tab_empresas)

RSQLite::dbDisconnect(con)





