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
#   2017 — Resoluções CONDEL/SUDENE nº 107 (jul) e nº 115 (nov)
#   2018 — Resolução CONDEL/SUDENE nº 128, de 10/08/2018
#   2019 — Resolução CONDEL/SUDENE nº 150, de 13/12/2019
#   2022 — Resolução CONDEL/SUDENE nº 173, de 15/12/2021 (vigor 2022)
#
# Estratégia:
#   - geobr::read_semiarid() disponibiliza snapshots para 2005, 2017, 2022.
#   - Municípios adicionados entre snapshots são deduzidos por diferença de conjuntos.
#   - Para resoluções 128/2018 e 150/2019 (sem snapshot geobr disponível),
#     os códigos são extraídos das tabelas incrementais dos anexos normativos.
#
# Saída:
#   data/processed/semiarido_entrada.rds — tabela de ano de entrada por município
#   data/processed/semiarido_panel.rds   — painel município-ano com d_semiarido
# =============================================================================

source(here::here("R", "00_setup.R"))

mun_tab <- carregar("municipios_nordeste_tab")


# ---- 1. Snapshots via geobr -------------------------------------------------

message("Baixando delimitação do semi-árido (geobr) — anos 2005, 2017, 2022...")

# geobr::read_semiarid() suporta years: 2005, 2017, 2021, 2022
extrair_codigos <- function(ano_geobr) {
  geobr::read_semiarid(year = ano_geobr, showProgress = FALSE) |>
    sf::st_drop_geometry() |>
    dplyr::pull(code_muni) |>
    ibge7()   # padroniza para 7 dígitos inteiros
}

codigos_2005 <- extrair_codigos(2005)
codigos_2017 <- extrair_codigos(2017)
codigos_2022 <- extrair_codigos(2022)

message("  Semi-árido 2005: ", length(codigos_2005), " municípios")
message("  Semi-árido 2017: ", length(codigos_2017), " municípios")
message("  Semi-árido 2022: ", length(codigos_2022), " municípios")


# ---- 2. Códigos incrementais das resoluções intermediárias ------------------
# Resoluções 128/2018 e 150/2019 adicionaram municípios não capturados pela
# diferença 2017 → 2022 do geobr. Códigos extraídos dos anexos publicados
# no DOU e compilados no site da SUDENE.

# Resolução CONDEL nº 128/2018 — municípios do MA e PI
muns_res128_2018 <- ibge7(c(
  2101350L, 2101608L, 2103703L, 2104552L, 2105302L, 2106276L, 2108801L,
  2108909L, 2201150L, 2201556L, 2205359L, 2206241L, 2206340L, 2206696L,
  2209708L
))

# Resolução CONDEL nº 150/2019 — municípios da BA e PE
muns_res150_2019 <- ibge7(c(
  2902658L, 2905701L, 2912202L, 2922250L, 2924306L, 2928109L, 2929503L,
  2602803L, 2615805L
))


# ---- 3. Atribuir ano de entrada por conjunto de diferença ------------------

# Adicionados em 2017 (Res. 107 + 115): estavam em 2017 mas não em 2005
muns_add_2017 <- setdiff(codigos_2017, codigos_2005)

# Adicionados em 2022 (Res. 173): estavam em 2022 mas não em 2017, 2018 ou 2019
muns_add_2022 <- setdiff(
  setdiff(codigos_2022, codigos_2017),
  c(muns_res128_2018, muns_res150_2019)
)

# Tabela de ano de entrada
semiarido_entrada <- dplyr::bind_rows(
  tibble::tibble(cod_mun = codigos_2005,     ano_entrada_semiarido = 2005L),
  tibble::tibble(cod_mun = muns_add_2017,    ano_entrada_semiarido = 2017L),
  tibble::tibble(cod_mun = muns_res128_2018, ano_entrada_semiarido = 2018L),
  tibble::tibble(cod_mun = muns_res150_2019, ano_entrada_semiarido = 2019L),
  tibble::tibble(cod_mun = muns_add_2022,    ano_entrada_semiarido = 2022L)
) |>
  # Mantém apenas municípios do Nordeste que estão no painel
  dplyr::filter(cod_mun %in% mun_tab$cod_mun) |>
  # Remove duplicatas: mantém o ano mais antigo
  dplyr::group_by(cod_mun) |>
  dplyr::slice_min(ano_entrada_semiarido, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

message("Municípios nordestinos semi-áridos mapeados: ", nrow(semiarido_entrada))


# ---- 4. Expandir para painel município-ano ---------------------------------

painel_semiarido <- mun_tab |>
  dplyr::select(cod_mun, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS) |>
  dplyr::left_join(semiarido_entrada, by = "cod_mun") |>
  dplyr::mutate(
    d_semiarido = dplyr::case_when(
      is.na(ano_entrada_semiarido)    ~ 0L,   # nunca entrou no semi-árido
      ano >= ano_entrada_semiarido    ~ 1L,   # já entrou
      TRUE                            ~ 0L    # ainda não entrou
    )
  )


# ---- 5. Verificação ---------------------------------------------------------

check_semiarido <- painel_semiarido |>
  dplyr::group_by(ano) |>
  dplyr::summarise(n_semiarido = sum(d_semiarido), .groups = "drop")

message("\nMunicípios semi-áridos (NE) por ano de referência:")
print(
  check_semiarido |>
    dplyr::filter(ano %in% c(2004, 2005, 2017, 2018, 2019, 2022))
)


# ---- 6. Salvar --------------------------------------------------------------

salvar(semiarido_entrada, "semiarido_entrada")
salvar(painel_semiarido,  "semiarido_panel")


# ---- 7. Mapa: evolução do semi-árido ----------------------------------------

mun_sf <- carregar("municipios_nordeste")

anos_mapa <- c(2004, 2005, 2017, 2019, 2022)

mapas_semiarido <- lapply(anos_mapa, function(a) {
  painel_semiarido |>
    dplyr::filter(ano == a) |>
    dplyr::left_join(mun_sf |> dplyr::select(cod_mun, geom), by = "cod_mun") |>
    sf::st_as_sf() |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(fill = factor(d_semiarido)),
      colour = "white", linewidth = 0.05
    ) +
    ggplot2::scale_fill_manual(
      values = c("0" = "#f5f5f5", "1" = "#e67e22"),
      labels = c("0" = "Fora", "1" = "Semi-árido"),
      name   = NULL
    ) +
    ggplot2::labs(title = as.character(a)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position  = "none",
      plot.title       = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
})

mapa_evolucao <- patchwork::wrap_plots(mapas_semiarido, nrow = 1) +
  patchwork::plot_annotation(
    title   = "Evolução da Delimitação do Semi-Árido no Nordeste (SUDENE/CONDEL)",
    caption = "Fontes: SUDENE (Resoluções CONDEL 2005–2022); IBGE/geobr"
  )

ggplot2::ggsave(
  file.path(DIR_MAPS, "02_evolucao_semiarido.png"),
  plot   = mapa_evolucao,
  width  = 16, height = 5, dpi = 300
)

message("\n02_semiarido_sudene.R concluído.")
