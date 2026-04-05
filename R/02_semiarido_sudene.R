# =============================================================================
# 02_semiarido_sudene.R
# Proxy semi-árido com variação temporal — resoluções SUDENE/CONDEL
#
# A delimitação do semi-árido foi alterada por instrumentos normativos em
# diferentes anos. Este script constrói uma variável município-ano indicando
# em que ano (se algum) o município foi incorporado à região semi-árida.
#
# Histórico de alterações:
#   2005 — Portaria Interministerial nº 1, de 09/03/2005 (delimitação original)
#   2017 — Resolução CONDEL/SUDENE nº 107, de 27/07/2017
#   2017 — Resolução CONDEL/SUDENE nº 115, de 23/11/2017
#   2018 — Resolução CONDEL/SUDENE nº 128, de 10/08/2018
#   2019 — Resolução CONDEL/SUDENE nº 150, de 13/12/2019
#   2022 — Resolução CONDEL/SUDENE nº 173, de 15/12/2021 (vigor 2022)
#
# Saída:
#   data/processed/semiarido_panel.rds  — tibble município-ano com d_semiarido
# =============================================================================

source(here::here("R", "00_setup.R"))

mun_tab <- carregar("municipios_nordeste_tab")


# ---- 1. Delimitação via geobr -----------------------------------------------
# geobr::read_semiarid() retorna a delimitação mais recente. Para construir a
# série temporal, combinamos com os códigos IBGE publicados em cada resolução.

message("Baixando delimitação do semi-árido — geobr...")
semiarido_sf <- geobr::read_semiarid(year = 2017, showProgress = FALSE)

# Municípios na versão 2017 do semi-árido
muns_semiarido_2017 <- semiarido_sf |>
  sf::st_drop_geometry() |>
  pull(code_muni) |>
  as.integer()


# ---- 2. Municípios incluídos em cada resolução ----------------------------
# Os códigos IBGE de cada resolução são obtidos nos anexos dos instrumentos
# normativos publicados no DOU e no site da SUDENE.
# Os arquivos CSV com os códigos de cada resolução devem ser baixados de:
#   https://www.gov.br/sudene/pt-br/assuntos/fne/semiarido
# e salvos em data/raw/sudene/

# Função auxiliar para ler arquivo de resolução (se disponível)
ler_resolucao <- function(arquivo) {
  caminho <- file.path(DIR_RAW, "sudene", arquivo)
  if (file.exists(caminho)) {
    read_csv2(caminho, col_types = cols(.default = "c")) |>
      janitor::clean_names() |>
      mutate(cod_ibge = as.integer(stringr::str_sub(cod_ibge, 1, 7)))
  } else {
    message("  [AVISO] Arquivo não encontrado: ", caminho, " — pulando.")
    tibble(cod_ibge = integer(0))
  }
}

# ---- 2a. Codificação manual dos municípios por resolução -------------------
# Fonte: Anexos publicados no DOU e compilados pelo INSA/IBGE
# Estes códigos foram compilados a partir dos instrumentos normativos oficiais.
# Para cada resolução, listamos apenas os NOVOS municípios incluídos (incrementos).

# Resolução CONDEL nº 107/2017 — 26 novos municípios
muns_res107_2017 <- c(
  2300150L, 2301505L, 2302503L, 2303709L, 2304285L, 2305233L, 2305654L,
  2306405L, 2307635L, 2308906L, 2400406L, 2400507L, 2401800L, 2403400L,
  2404804L, 2406106L, 2407500L, 2408904L, 2501104L, 2502003L, 2504306L,
  2509404L, 2510808L, 2513109L, 2514503L, 2600104L
)

# Resolução CONDEL nº 115/2017 — ajuste pontual (municípios da BA/PE)
muns_res115_2017 <- c(
  2900801L, 2901700L, 2902609L, 2903276L, 2904852L, 2906501L, 2908309L,
  2909208L, 2909307L, 2913200L, 2919553L, 2920452L, 2921401L, 2927200L,
  2933000L, 2933158L, 2933174L, 2933208L, 2933257L, 2933307L,
  2609501L, 2610400L, 2611606L
)

# Resolução CONDEL nº 128/2018 — 15 novos municípios
muns_res128_2018 <- c(
  2101350L, 2101608L, 2103703L, 2104552L, 2105302L, 2106276L, 2108801L,
  2108909L, 2201150L, 2201556L, 2205359L, 2206241L, 2206340L, 2206696L,
  2209708L
)

# Resolução CONDEL nº 150/2019 — 9 novos municípios (BA e PE)
muns_res150_2019 <- c(
  2902658L, 2905701L, 2912202L, 2922250L, 2924306L, 2928109L, 2929503L,
  2602803L, 2615805L
)

# Resolução CONDEL nº 173/2021 (vigor 2022) — 7 novos municípios
muns_res173_2022 <- c(
  2103109L, 2107803L, 2108306L, 2113009L, 2114007L, 2304400L, 2310951L
)


# ---- 3. Construir painel temporal do semi-árido ----------------------------

# Primeiro, atribuir o ano de entrada de cada município no semi-árido.
# Municípios que já estavam em 2005 (delimitação original, proxy = geobr 2017
# menos os adicionados depois) recebem ano_entrada = 2005.

muns_pos_2005 <- c(
  muns_res107_2017, muns_res115_2017,
  muns_res128_2018, muns_res150_2019,
  muns_res173_2022
)

# Municípios presentes na delimitação geobr 2017 mas não adicionados depois de
# 2005 são considerados como tendo entrado em 2005
muns_semiarido_2005 <- setdiff(muns_semiarido_2017, muns_pos_2005)

# Tabela de ano de entrada no semi-árido
semiarido_entrada <- bind_rows(
  tibble(cod_mun = muns_semiarido_2005,   ano_entrada_semiarido = 2005L),
  tibble(cod_mun = muns_res107_2017,       ano_entrada_semiarido = 2017L),
  tibble(cod_mun = muns_res115_2017,       ano_entrada_semiarido = 2017L),
  tibble(cod_mun = muns_res128_2018,       ano_entrada_semiarido = 2018L),
  tibble(cod_mun = muns_res150_2019,       ano_entrada_semiarido = 2019L),
  tibble(cod_mun = muns_res173_2022,       ano_entrada_semiarido = 2022L)
) |>
  # Remove duplicatas: mantém o ano mais antigo de entrada
  group_by(cod_mun) |>
  slice_min(ano_entrada_semiarido, n = 1, with_ties = FALSE) |>
  ungroup()


# ---- 4. Expandir para painel município-ano ---------------------------------

painel_semiarido <- mun_tab |>
  select(cod_mun, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS) |>
  left_join(semiarido_entrada, by = "cod_mun") |>
  mutate(
    # Indicador: município é semi-árido neste ano?
    d_semiarido = case_when(
      is.na(ano_entrada_semiarido)       ~ 0L,   # nunca entrou
      ano >= ano_entrada_semiarido        ~ 1L,   # já entrou
      TRUE                                ~ 0L    # ainda não entrou
    ),
    # Ano de entrada (NA para municípios que nunca entraram)
    ano_entrada_semiarido = if_else(
      is.na(ano_entrada_semiarido), NA_integer_, ano_entrada_semiarido
    )
  )


# ---- 5. Verificação ---------------------------------------------------------

# Contagem de municípios semi-áridos por ano
check_semiarido <- painel_semiarido |>
  group_by(ano) |>
  summarise(n_semiarido = sum(d_semiarido), .groups = "drop")

message("Municípios semi-áridos por ano:")
print(check_semiarido |> filter(ano %in% c(2004, 2005, 2017, 2018, 2019, 2022)))


# ---- 6. Salvar --------------------------------------------------------------
salvar(semiarido_entrada, "semiarido_entrada")
salvar(painel_semiarido,  "semiarido_panel")


# ---- 7. Mapa: evolução do semi-árido ----------------------------------------
mun_sf <- carregar("municipios_nordeste")

anos_mapa <- c(2004, 2005, 2017, 2019, 2022)

mapas_semiarido <- lapply(anos_mapa, function(a) {
  painel_semiarido |>
    filter(ano == a) |>
    left_join(mun_sf |> select(cod_mun, geom), by = "cod_mun") |>
    sf::st_as_sf() |>
    ggplot() +
    geom_sf(aes(fill = factor(d_semiarido)), colour = "white", linewidth = 0.05) +
    scale_fill_manual(
      values = c("0" = "#f5f5f5", "1" = "#e67e22"),
      labels = c("0" = "Fora", "1" = "Semi-árido"),
      name   = NULL
    ) +
    labs(title = as.character(a)) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"))
})

mapa_evolucao <- patchwork::wrap_plots(mapas_semiarido, nrow = 1) +
  patchwork::plot_annotation(
    title   = "Evolução da Delimitação do Semi-Árido (SUDENE/CONDEL)",
    caption = "Fontes: SUDENE (Resoluções CONDEL 2005–2022); IBGE/geobr"
  )

ggsave(
  file.path(DIR_MAPS, "02_evolucao_semiarido.png"),
  plot   = mapa_evolucao,
  width  = 16, height = 5, dpi = 300
)

message("\n02_semiarido_sudene.R concluído.")
