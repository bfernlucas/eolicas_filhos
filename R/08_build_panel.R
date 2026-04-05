# =============================================================================
# 08_build_panel.R
# Montagem do painel analítico final — município × ano
#
# Combina todas as fontes processadas em um único data frame balanceado,
# pronto para estimação no scripts 10 e 11.
#
# Lógica de join:
#   backbone (todos mun × todos anos)
#   → tratamento (ANEEL)
#   → semi-árido (SUDENE)
#   → saúde (DataSUS)
#   → registro civil (paternidade)
#   → emprego (RAIS)
#   → controles (IPEA/IBGE)
#
# Variáveis derivadas finais (taxas, logs, indicadores DiD).
#
# Saída:
#   data/final/painel_final.rds  — tibble balanceado, pronto para estimação
#   data/final/painel_final.csv  — versão CSV legível
# =============================================================================

source(here::here("R", "00_setup.R"))

# ---- Carrega todos os módulos processados ----------------------------------
mun_tab        <- carregar("municipios_nordeste_tab")
eolicas        <- carregar("eolicas_panel")
semiarido      <- carregar("semiarido_panel")
saude          <- carregar("saude_panel")
registro_civil <- carregar("registro_civil_panel")
rais           <- carregar("rais_panel")
controles      <- carregar("controles_panel")


# ============================================================================
# 1. BACKBONE: produto cartesiano município × ano
# ============================================================================

message("Construindo backbone do painel...")

backbone <- mun_tab |>
  select(cod_mun, cod_uf, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS)

message(glue::glue(
  "Backbone: {nrow(backbone):,} observações ",
  "({n_distinct(backbone$cod_mun)} municípios × {length(ANOS)} anos)"
))


# ============================================================================
# 2. JOINS DAS FONTES
# ============================================================================

message("Fazendo joins das fontes...")

# Selecionar apenas colunas necessárias de cada fonte (evitar duplicatas)
eolicas_join <- eolicas |>
  select(cod_mun, ano,
         d_eolica_dum, ano_primeiro_eolica, g_eolica,
         n_usinas_operando, cap_instalada_mw, log_cap_mw)

semiarido_join <- semiarido |>
  select(cod_mun, ano, d_semiarido, ano_entrada_semiarido)

saude_join <- saude |>
  select(cod_mun, ano,
         y_obito_aborto, y_obito_rn, y_obito_materno,
         y_nascidos_vivos,
         y_nasc_adol, y_nasc_adol_10_14, y_nasc_adol_15_19,
         y_intern_aborto, y_intern_o03, y_intern_o04, y_intern_o07,
         x_prenatal_media, x_prenatal_nenhum,
         tx_obito_aborto, tx_obito_rn, tx_obito_materno,
         tx_intern_aborto, tx_nasc_adol, pct_nasc_adol)

rc_join <- registro_civil |>
  select(cod_mun, ano,
         y_nasc_rc, y_sem_pai, y_reconhecimento,
         tx_sem_pai, tx_reconhecimento)

rais_join <- rais |>
  select(cod_mun, ano,
         n_vinculos, n_mulheres, n_vinculos_energia, n_vinculos_construcao,
         salario_medio, pct_mulheres, log_vinculos, log_vinculos_energia)

controles_join <- controles |>
  select(cod_mun, ano,
         x_pop, x_pib_pc, x_idhm, x_gini,
         lon, lat, x_dist_capital_km, x_vento_ms,
         log_pop, log_pib_pc, x_pib_pc_mil)

# Join sequencial
painel <- backbone |>
  left_join(eolicas_join,   by = c("cod_mun", "ano")) |>
  left_join(semiarido_join, by = c("cod_mun", "ano")) |>
  left_join(saude_join,     by = c("cod_mun", "ano")) |>
  left_join(rc_join,        by = c("cod_mun", "ano")) |>
  left_join(rais_join,      by = c("cod_mun", "ano")) |>
  left_join(controles_join, by = c("cod_mun", "ano"))

message(glue::glue("Painel após joins: {nrow(painel):,} obs × {ncol(painel)} variáveis"))


# ============================================================================
# 3. VARIÁVEIS DERIVADAS PARA DiD
# ============================================================================

message("Criando variáveis derivadas para DiD...")

painel <- painel |>
  mutate(

    # ── Grupo de coorte (Callaway & Sant'Anna)
    # Convenção: 0 = nunca tratado (requerido pelo pacote `did`)
    g_cs = if_else(is.na(ano_primeiro_eolica), 0L, as.integer(ano_primeiro_eolica)),

    # ── Tempo relativo ao tratamento
    rel_time = if_else(
      !is.na(ano_primeiro_eolica),
      as.integer(ano - ano_primeiro_eolica),
      NA_integer_
    ),

    # ── Post-tratamento (para TWFE simples)
    d_post = as.integer(d_eolica_dum == 1),

    # ── Indicador COVID (controle em todas as especificações)
    d_covid = as.integer(ano %in% c(2020L, 2021L)),

    # ── Densidade demográfica
    x_dens_dem = x_pop / 1,  # área será adicionada após join com sf (abaixo)

    # ── Taxas per capita para RAIS
    x_vinculos_pc = n_vinculos / pmax(x_pop, 1) * 1000,
    x_energia_pc  = n_vinculos_energia / pmax(x_pop, 1) * 1000,

    # ── PIB per capita em log (para controle)
    log_pib_pc = log1p(x_pib_pc),

    # ── Variáveis de resultado em log (adicionando 0.5 para zeros)
    log_y_obito_aborto  = log(y_obito_aborto + 0.5),
    log_y_obito_rn      = log(y_obito_rn + 0.5),
    log_y_intern_aborto = log(y_intern_aborto + 0.5),
    log_y_nasc_adol     = log(y_nasc_adol + 0.5),
    log_y_sem_pai       = log(y_sem_pai + 0.5),

    # ── Winsorize taxas para evitar outliers extremos
    tx_nasc_adol_w   = winsorize(tx_nasc_adol),
    tx_intern_aborto_w = winsorize(tx_intern_aborto),
    tx_sem_pai_w     = winsorize(tx_sem_pai),

    # ── Nunca tratado (para filtros)
    never_treated = as.integer(is.na(ano_primeiro_eolica))
  )


# ── Área dos municípios (para densidade) ------------------------------------
mun_sf <- carregar("municipios_nordeste")

areas_km2 <- mun_sf |>
  mutate(area_km2 = as.numeric(sf::st_area(geom)) / 1e6) |>
  sf::st_drop_geometry() |>
  select(cod_mun, area_km2) |>
  as_tibble()

painel <- painel |>
  left_join(areas_km2, by = "cod_mun") |>
  mutate(x_dens_dem = x_pop / pmax(area_km2, 1))


# ============================================================================
# 4. VERIFICAÇÕES DE QUALIDADE
# ============================================================================

message("\n=== Verificações de qualidade do painel ===")

# 4a. Balanceamento
n_esperado <- n_distinct(painel$cod_mun) * length(ANOS)
stopifnot(
  "Painel desbalanceado" = nrow(painel) == n_esperado,
  "Duplicatas encontradas" = !anyDuplicated(painel[c("cod_mun", "ano")])
)
message(glue::glue("✓ Painel balanceado: {nrow(painel):,} obs"))

# 4b. Distribuição de coortes
coortes <- painel |>
  filter(ano == ANO_INICIO) |>
  count(g_cs, name = "n_mun") |>
  arrange(g_cs)

message("\nDistribuição de coortes de tratamento (g_cs):")
print(coortes)

# 4c. Missing em variáveis-chave
missing_check <- painel |>
  summarise(
    across(
      c(y_nascidos_vivos, tx_nasc_adol, tx_intern_aborto,
        y_sem_pai, n_vinculos, x_pop, x_pib_pc),
      ~ mean(is.na(.)) * 100,
      .names = "pct_miss_{.col}"
    )
  ) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "pct_missing") |>
  filter(pct_missing > 0)

if (nrow(missing_check) > 0) {
  message("\nVariáveis com missing (%):")
  print(missing_check)
} else {
  message("✓ Sem missing em variáveis-chave")
}

# 4d. Resumo por grupo de tratamento
resumo_grupos <- painel |>
  group_by(never_treated) |>
  summarise(
    n_mun         = n_distinct(cod_mun),
    n_obs         = n(),
    media_pct_adol = mean(pct_nasc_adol, na.rm = TRUE),
    media_vinculos = mean(n_vinculos,    na.rm = TRUE),
    .groups = "drop"
  )

message("\nResumo por grupo de tratamento:")
print(resumo_grupos)


# ============================================================================
# 5. SALVAR
# ============================================================================

# Formato .rds (para R)
saveRDS(painel, file.path(DIR_FINAL, "painel_final.rds"), compress = "xz")
message(glue::glue("\nPainel final salvo: {file.path(DIR_FINAL, 'painel_final.rds')}"))

# Formato .csv (para verificação externa / Stata / Python)
readr::write_csv(
  painel |> select(-geom) |> sf::st_drop_geometry(),
  file.path(DIR_FINAL, "painel_final.csv")
)
message("CSV salvo: ", file.path(DIR_FINAL, "painel_final.csv"))

# Dicionário resumido das variáveis
variaveis_info <- tibble(
  variavel    = names(painel),
  classe      = sapply(painel, class) |> sapply(paste, collapse = "/"),
  n_missing   = sapply(painel, function(x) sum(is.na(x))),
  pct_missing = round(100 * n_missing / nrow(painel), 2)
)
readr::write_csv(variaveis_info, file.path(DIR_FINAL, "variaveis_painel.csv"))

message(glue::glue(
  "\n=== 08_build_panel.R concluído ===",
  "\nDimensões finais: {nrow(painel):,} obs × {ncol(painel)} variáveis",
  "\nMunicípios: {n_distinct(painel$cod_mun)}",
  "\nAnos: {min(painel$ano)}–{max(painel$ano)}",
  "\nTratados: {sum(painel$never_treated == 0 & painel$ano == ANO_INICIO)} municípios",
  "\nControles: {sum(painel$never_treated == 1 & painel$ano == ANO_INICIO)} municípios"
))
