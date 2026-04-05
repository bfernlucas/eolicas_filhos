# =============================================================================
# 02_semiarido_sudene.R
# Proxy semi-árido com variação temporal — resoluções SUDENE/CONDEL
#
# A delimitação do semi-árido foi alterada por instrumentos normativos em
# diferentes anos. Este script constrói uma variável município-ano indicando
# em que ano o município foi incorporado à região semi-árida.
#
# Histórico de alterações (com contagens verificadas):
#   2005 — Portaria Interministerial nº 1, de 09/03/2005
#           → delimitação original: 1.135 municípios
#   2017 — Resolução CONDEL/SUDENE nº 107 (54 mun.) + nº 115 (73 mun.)
#           → total: 1.262 municípios
#   2021 — Resolução CONDEL/SUDENE nº 150, de 13/12/2021
#           → +215 municípios, -50 municípios (inclui MG e ES pela 1ª vez)
#           → total: 1.477 municípios
#
# Estratégia:
#   Usa geobr::read_semiarid() nos anos 2005, 2017 e 2021 (snapshots
#   oficiais verificáveis). A diferença de conjuntos entre snapshots
#   determina o ano de entrada. Nenhum código IBGE é hardcoded.
#
# Nota sobre escopo:
#   Este projeto foca no Nordeste. As adições de 2021 incluem municípios
#   de MG e ES que não integram o painel — apenas mun. NE são mantidos.
#
# Saída:
#   data/processed/semiarido_entrada.rds — tabela município × ano_entrada
#   data/processed/semiarido_panel.rds   — painel município-ano com d_semiarido
# =============================================================================

source(here::here("R", "00_setup.R"))

mun_tab <- carregar("municipios_nordeste_tab")


# ---- 1. Snapshots via geobr -------------------------------------------------

message("Baixando delimitações do semi-árido (geobr)...")

extrair_codigos <- function(ano_geobr) {
  geobr::read_semiarid(year = ano_geobr, showProgress = FALSE) |>
    sf::st_drop_geometry() |>
    dplyr::pull(code_muni) |>
    ibge7()
}

codigos_2005 <- extrair_codigos(2005)
codigos_2017 <- extrair_codigos(2017)
codigos_2021 <- extrair_codigos(2021)

message(sprintf("  Snapshot 2005: %d municípios (Brasil)", length(codigos_2005)))
message(sprintf("  Snapshot 2017: %d municípios (Brasil)", length(codigos_2017)))
message(sprintf("  Snapshot 2021: %d municípios (Brasil)", length(codigos_2021)))


# ---- 2. Municípios adicionados em cada rodada (diferença de conjuntos) -----

# Adicionados em 2017 (Res. 107 + Res. 115)
muns_add_2017 <- setdiff(codigos_2017, codigos_2005)
# Removidos em 2017 (raro, mas possível)
muns_rem_2017 <- setdiff(codigos_2005, codigos_2017)

# Adicionados em 2021 (Res. 150)
muns_add_2021 <- setdiff(codigos_2021, codigos_2017)
# Removidos em 2021 (Res. 150 removeu 50 municípios)
muns_rem_2021 <- setdiff(codigos_2017, codigos_2021)

message(sprintf("\n  Adicionados em 2017: %d | Removidos: %d",
                length(muns_add_2017), length(muns_rem_2017)))
message(sprintf("  Adicionados em 2021: %d | Removidos: %d",
                length(muns_add_2021), length(muns_rem_2021)))


# ---- 3. Tabela de ano de entrada (escopo: todos os municípios do NE) --------

semiarido_entrada <- dplyr::bind_rows(
  # Baseline 2005
  tibble::tibble(cod_mun = codigos_2005, ano_entrada_semiarido = 2005L,
                 status  = "base_2005"),
  # Adicionados em 2017
  tibble::tibble(cod_mun = muns_add_2017, ano_entrada_semiarido = 2017L,
                 status  = "add_2017"),
  # Adicionados em 2021
  tibble::tibble(cod_mun = muns_add_2021, ano_entrada_semiarido = 2021L,
                 status  = "add_2021")
) |>
  # Restringe ao Nordeste
  dplyr::filter(cod_mun %in% mun_tab$cod_mun) |>
  # Remove possíveis duplicatas (mantém entrada mais antiga)
  dplyr::group_by(cod_mun) |>
  dplyr::slice_min(ano_entrada_semiarido, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

# Municípios removidos (tratados como nunca-tratados a partir da remoção)
# Nota: removidos em 2021 recebem d_semiarido = 0 a partir de 2021.
saidas_2021_ne <- intersect(muns_rem_2021, mun_tab$cod_mun)
message(sprintf("\n  Municípios NE removidos em 2021: %d", length(saidas_2021_ne)))

message(sprintf("  Municípios NE com entrada no semi-árido: %d / %d total NE",
                nrow(semiarido_entrada), nrow(mun_tab)))


# ---- 4. Painel município-ano ------------------------------------------------

painel_semiarido <- mun_tab |>
  dplyr::select(cod_mun, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS) |>
  dplyr::left_join(semiarido_entrada |> dplyr::select(cod_mun, ano_entrada_semiarido),
                   by = "cod_mun") |>
  dplyr::mutate(
    d_semiarido = dplyr::case_when(
      # Nunca entrou
      is.na(ano_entrada_semiarido)         ~ 0L,
      # Entrou mas foi removido em 2021 → 0 a partir de 2021
      cod_mun %in% saidas_2021_ne & ano >= 2021L ~ 0L,
      # Entrou e está dentro do período
      ano >= ano_entrada_semiarido          ~ 1L,
      # Ainda não entrou
      TRUE                                  ~ 0L
    )
  )


# ---- 5. Verificação ---------------------------------------------------------

check_sa <- painel_semiarido |>
  dplyr::group_by(ano) |>
  dplyr::summarise(n_semiarido = sum(d_semiarido), .groups = "drop")

message("\nMunicípios semi-áridos (NE) por ano:")
print(
  check_sa |> dplyr::filter(ano %in% c(2004, 2005, 2016, 2017, 2020, 2021, 2022))
)


# ---- 6. Salvar --------------------------------------------------------------

salvar(semiarido_entrada, "semiarido_entrada")
salvar(painel_semiarido,  "semiarido_panel")


# ---- 7. Mapa: evolução do semi-árido ----------------------------------------

mun_sf <- carregar("municipios_nordeste")

anos_mapa <- c(2004, 2005, 2017, 2021, 2022)

mapas_sa <- lapply(anos_mapa, function(a) {
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
      legend.position = "none",
      plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
})

mapa_evolucao <- patchwork::wrap_plots(mapas_sa, nrow = 1) +
  patchwork::plot_annotation(
    title   = "Evolução da Delimitação do Semi-Árido no Nordeste (SUDENE/CONDEL)",
    caption = paste(
      "Fontes: geobr::read_semiarid() (snapshots 2005, 2017, 2021);",
      "Portaria Intermin. 1/2005; Res. CONDEL 107+115/2017; Res. CONDEL 150/2021."
    )
  )

ggplot2::ggsave(
  file.path(DIR_MAPS, "02_evolucao_semiarido.png"),
  plot   = mapa_evolucao,
  width  = 16, height = 5, dpi = 300
)

message("\n02_semiarido_sudene.R concluído.")
