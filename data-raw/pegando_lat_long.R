tab_aeroportos <- readxl::read_excel(
  "data-raw/glossario_de_aerodromo_raw.xls",
  skip = 3
) |>
  janitor::clean_names()

ggmap::register_google()

# ggmap::register_google(
#   key = "" # https://console.cloud.google.com/google/maps-apis/,
#   write = TRUE
# )

safe_geocode <- purrr::possibly(ggmap::geocode, otherwise = NA)

tab <- tab_aeroportos |>
  dplyr::filter(pais == "BRASIL") |>
  dplyr::mutate(
    end = paste(paste("AEROPORTO", descricao), cidade, uf, pais, sep = ", "),
    latlong = purrr::map(
      end,
      safe_geocode
    )
  )

sorriso <- safe_geocode("AEROPORTO SORRISO, SORRISO, MT, BRASIL")

tab$latlong[[3]] <- sorriso

tab |>
  dplyr::select(-end) |>
  tidyr::unnest(latlong) |>
  writexl::write_xlsx("data-raw/glossario_de_aerodromo.xlsx")







