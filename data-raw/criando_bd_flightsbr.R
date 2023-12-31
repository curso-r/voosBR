# Baixando base de dados -----------------------------------------------

anos <- 2019:2023

tabelas <- purrr::map(
  anos,
  flightsbr::read_flights
)

tab_voos <- tabelas |>
  dplyr::bind_rows() |>
  dplyr::select(
    id_empresa,
    sg_empresa_iata,
    nm_empresa,
    ds_tipo_empresa,
    nm_pais,
    dt_referencia,
    nr_ano_referencia,
    nr_mes_referencia,
    nr_semana_referencia,
    nm_dia_semana_referencia,
    nr_dia_referencia,
    nr_ano_mes_referencia,
    cd_tipo_linha,
    ds_tipo_linha,
    ds_natureza_tipo_linha,
    ds_servico_tipo_linha,
    ds_natureza_etapa,
    hr_partida_real,
    dt_partida_real,
    nr_ano_partida_real,
    nr_mes_partida_real,
    nr_semana_partida_real,
    nm_dia_semana_partida_real,
    nr_dia_partida_real,
    nr_ano_mes_partida_real,
    id_aerodromo_origem,
    sg_iata_origem,
    nm_aerodromo_origem,
    nm_municipio_origem,
    sg_uf_origem,
    nm_regiao_origem,
    nm_pais_origem,
    nm_continente_origem,
    nr_etapa,
    hr_chegada_real,
    dt_chegada_real,
    nr_ano_chegada_real,
    nr_mes_chegada_real,
    nr_semana_chegada_real,
    nm_dia_semana_chegada_real,
    nr_dia_chegada_real,
    nr_ano_mes_chegada_real,
    id_equipamento,
    sg_equipamento_icao,
    ds_modelo,
    id_aerodromo_destino,
    sg_iata_destino,
    nm_aerodromo_destino,
    nm_municipio_destino,
    sg_uf_destino,
    nm_regiao_destino,
    nm_pais_destino,
    nm_continente_destino,
    nr_escala_destino,
    lt_combustivel,
    nr_assentos_ofertados,
    kg_payload,
    km_distancia,
    nr_passag_pagos,
    nr_passag_gratis,
    kg_bagagem_livre,
    kg_bagagem_excesso,
    nr_decolagem,
    nr_horas_voadas,
    nr_velocidade_media
  )

con <- RSQLite::dbConnect(
  RSQLite::SQLite(),
  "flightsbr.sqlite"
)

RSQLite::dbWriteTable(
  con,
  "tab_voos",
  tab_voos,
  overwrite = TRUE
)

# Ajustando dados -----------------------------------------------------

con <- RSQLite::dbConnect(
  RSQLite::SQLite(),
  "flightsbr.sqlite"
)

tab_voos <- dplyr::tbl(con, "tab_voos") |>
  dplyr::collect()

mudar_encoding <- function(x) {
  Encoding(x) <- "latin1"
  x
}

tab <- tab_voos |>
  dplyr::filter(
    dt_partida_real >= "2019-01-01",
    dt_partida_real <= "2023-12-31"
  ) |>
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      mudar_encoding
    )
  )

RSQLite::dbWriteTable(
  con,
  "tab_voos",
  dplyr::collect(tab),
  overwrite = TRUE
)

RSQLite::dbDisconnect(con)

# Criando tabelas resumo ----------------------------------------------

con <- RSQLite::dbConnect(
  RSQLite::SQLite(),
  "flightsbr.sqlite"
)

tab_voos <- dplyr::tbl(con, "tab_voos")

tab_datas <- tab_voos |>
  dplyr::summarise(
    min = min(dt_partida_real, na.rm = TRUE),
    max = max(dt_partida_real, na.rm = TRUE)
  ) |>
  dplyr::collect()

tab_natureza <- tab_voos |>
  dplyr::distinct(ds_natureza_tipo_linha) |>
  dplyr::collect()

tab_servico <- tab_voos |>
  dplyr::distinct(ds_servico_tipo_linha) |>
  dplyr::collect()

tab_estados <- tab_voos |>
  dplyr::filter(nm_pais_origem == "BRASIL") |>
  dplyr::distinct(sg_uf_origem) |>
  dplyr::filter(sg_uf_origem != "") |> 
  dplyr::collect()

tab_aeroportos <- tab_voos |> 
  dplyr::filter(nm_pais_origem == "BRASIL") |>
  dplyr::distinct(nm_aerodromo_origem, sg_uf_origem) |>
  dplyr::collect()

RSQLite::dbWriteTable(
  con,
  "tab_datas",
  datas,
  overwrite = TRUE
)

RSQLite::dbWriteTable(
  con,
  "tab_natureza",
  tab_natureza,
  overwrite = TRUE
)

RSQLite::dbWriteTable(
  con,
  "tab_servico",
  tab_servico,
  overwrite = TRUE
)

RSQLite::dbWriteTable(
  con,
  "tab_estados",
  tab_estados,
  overwrite = TRUE
)

RSQLite::dbWriteTable(
  con,
  "tab_aeroportos",
  tab_aeroportos,
  overwrite = TRUE
)
