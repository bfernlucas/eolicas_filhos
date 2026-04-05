# =============================================================================
# 01_geo_nordeste.R
# Municípios do Nordeste — shapefile IBGE via pacote geobr
#
# Saída:
#   data/processed/municipios_nordeste.rds  — sf com geometria
#   data/processed/municipios_nordeste_tab.rds — tibble sem geometria (para joins)
# =============================================================================

source(here::here("R", "00_setup.R"))

# ---- 1. Baixar shapefile de todos os municípios do Brasil ------------------
# geobr usa o ano mais próximo disponível; para o painel usamos o limite de 2020
# (última versão estável). Municípios criados/extintos entre 2000-2022 são
# tratados na etapa de montagem do painel (08_build_panel.R).

message("Baixando shapefile de municípios — IBGE (geobr)...")

mun_br <- geobr::read_municipality(
  code_muni = "all",
  year       = 2020,
  simplified = FALSE,   # geometria completa para mapas precisos
  showProgress = TRUE
)

# ---- 2. Filtrar apenas o Nordeste ------------------------------------------
mun_ne <- mun_br |>
  filter(code_state %in% as.integer(ESTADOS_NE)) |>
  mutate(
    cod_mun    = as.integer(code_muni),
    cod_uf     = as.integer(code_state),
    nome_mun   = name_muni,
    nome_uf    = name_state,
    sigla_uf   = abbrev_state,
    regiao     = "Nordeste"
  ) |>
  select(cod_mun, cod_uf, nome_mun, nome_uf, sigla_uf, regiao, geom)

message(glue::glue("Municípios do Nordeste: {nrow(mun_ne)}"))

# ---- 3. Tabela sem geometria (para joins eficientes) -----------------------
mun_ne_tab <- mun_ne |>
  sf::st_drop_geometry() |>
  as_tibble()

# ---- 4. Verificação de integridade -----------------------------------------
stopifnot(
  "Número inesperado de municípios do NE" = nrow(mun_ne) >= 1794,  # ≥ 1794 mun
  "Código de 7 dígitos esperado"          = all(nchar(mun_ne_tab$cod_mun) == 7 |
                                                  mun_ne_tab$cod_mun >= 1000000)
)

# Tabela-resumo por UF
resumo_uf <- mun_ne_tab |>
  count(sigla_uf, nome_uf, name = "n_municipios") |>
  arrange(sigla_uf)

print(resumo_uf)

# ---- 5. Salvar --------------------------------------------------------------
salvar(mun_ne,     "municipios_nordeste")
salvar(mun_ne_tab, "municipios_nordeste_tab")

# Exportar também como GeoPackage (formato aberto, sem limite de nome de campo)
gpkg_path <- file.path(DIR_RAW, "ibge", "municipios_nordeste_2020.gpkg")
sf::st_write(mun_ne, gpkg_path, delete_dsn = TRUE)
message("GeoPackage salvo em: ", gpkg_path)


# ---- 6. Mapa de conferência ------------------------------------------------
mapa_ne <- ggplot(mun_ne) +
  geom_sf(fill = "#d4e6f1", colour = "white", linewidth = 0.1) +
  labs(
    title    = "Municípios do Nordeste Brasileiro",
    subtitle = glue::glue("{nrow(mun_ne)} municípios — Fonte: IBGE/geobr (2020)"),
    caption  = "Projeção: SIRGAS 2000 (EPSG:4674)"
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(
  file.path(DIR_MAPS, "01_municipios_nordeste.png"),
  plot   = mapa_ne,
  width  = 10, height = 8, dpi = 300
)

message("\n01_geo_nordeste.R concluído.")
