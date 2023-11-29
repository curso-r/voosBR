con <- RSQLite::dbConnect(RSQLite::SQLite(), "brflights.sqlite")

tab_voos <- dplyr::tbl(con, "tab_voos")
tab_aeroportos <- dplyr::tbl(con, "tab_aeroportos")

tab_voos |> head(1) |> dplyr::collect() |> names()

tab_voos |> 
  head(100) |> 
  dplyr::mutate(
    data_hora_plan = paste(planned_departure_date, planned_departure_date_hm),
    data_hora_real = paste(actual_departure_date, actual_departure_date_hm)
  ) |> 
  dplyr::select(
    origin_airport,
    data_hora_plan,
    data_hora_real
  ) |> 
  dplyr::collect() |> 
  dplyr::mutate(
    data_hora_plan = lubridate::ymd_hm(data_hora_plan),
    data_hora_real = lubridate::ymd_hm(data_hora_real),
    atraso = data_hora_real - data_hora_plan,
    atraso = as.numeric(atraso) / 60
  )  |> 
  dplyr::group_by(origin_airport) |> 
  dplyr::summarise(
    atraso_medio = mean(atraso, na.rm = TRUE)
  ) |> 
  dplyr::left_join(
    dplyr::collect(tab_aeroportos),
    by = c("origin_airport" = "airport_cod")
  ) |> 
  tidyr::drop_na(airport_name) |>
  dplyr::arrange(desc(atraso_medio)) |> 
  dplyr::mutate(
    rank = dplyr::row_number()
  ) |> 
  dplyr::select(
    rank,
    airport_name,
    city,
    atraso_medio
  )
   


