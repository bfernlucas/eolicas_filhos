# =============================================================================
# 04_saude_datasus.R
# Desfechos de saúde — DataSUS
#
# Sistemas utilizados:
#   SIM  (Sistema de Informações sobre Mortalidade)
#     → óbitos por CID O00-O08 (aborto/desfechos gestacionais)
#     → óbitos neonatais (0-27 dias) — RN
#   SINASC (Sistema de Informações sobre Nascidos Vivos)
#     → total de nascidos vivos por município-ano
#     → gravidez na adolescência (mãe 10-19 anos)
#   SIH (Sistema de Informações Hospitalares)
#     → internações por CID O00-O08 (proxy para abortos)
#
# Pacote: microdatasus (Saldanha et al., 2019)
#   GitHub: https://github.com/rfsaldanha/microdatasus
#
# Saída:
#   data/processed/saude_panel.rds  — tibble município-ano com variáveis y_
# =============================================================================

source(here::here("R", "00_setup.R"))

mun_tab <- carregar("municipios_nordeste_tab")


# ---- Função auxiliar: agrega microdados → município-ano ------------------

agg_mun_ano <- function(df, cod_col, ano_col, ...) {
  df |>
    rename(cod_mun_raw = all_of(cod_col),
           ano         = all_of(ano_col)) |>
    mutate(
      cod_mun = as.integer(stringr::str_sub(as.character(cod_mun_raw), 1, 7)),
      ano     = as.integer(ano)
    ) |>
    filter(cod_mun %in% mun_tab$cod_mun,
           ano >= ANO_INICIO, ano <= ANO_FIM) |>
    group_by(cod_mun, ano) |>
    summarise(..., .groups = "drop")
}


# ============================================================================
# BLOCO A — SIM: Sistema de Informações sobre Mortalidade
# ============================================================================

message("\n=== SIM — Óbitos ===")

# Estratégia: baixar microdados por UF e ano, filtrar CIDs relevantes.
# microdatasus::fetch_datasus() permite filtrar por UF e período.

# Para eficiência, usamos período 2000-2022 e UFs do Nordeste.
# O download pode ser demorado (~1-2h); o resultado é cacheado em disco.

sim_cache <- file.path(DIR_RAW, "datasus", "sim", "sim_nordeste.rds")

if (!file.exists(sim_cache)) {
  message("Baixando microdados SIM (óbitos) — NE, 2000-2022...")
  message("  Isso pode levar 30-90 minutos na primeira execução.")

  sim_raw <- purrr::map_dfr(SIGLAS_NE, function(uf) {
    message("  SIM — UF: ", uf)
    tryCatch(
      microdatasus::fetch_datasus(
        year_start  = ANO_INICIO,
        year_end    = ANO_FIM,
        uf          = uf,
        information_system = "SIM-DO"
      ),
      error = function(e) {
        message("  ERRO em ", uf, ": ", conditionMessage(e))
        NULL
      }
    )
  }) |>
    janitor::clean_names()

  saveRDS(sim_raw, sim_cache, compress = "xz")
  message("SIM salvo em: ", sim_cache)
} else {
  message("Carregando SIM do cache...")
  sim_raw <- readRDS(sim_cache)
}


# ---- A1. Óbitos por CID O00-O08 (gestação com desfecho abortivo) ----------

# CIDs da faixa: O00 a O08 e subcategorias (ex: O03.0, O03.9, etc.)
pattern_aborto <- paste0("^O0[0-8]")

sim_aborto <- sim_raw |>
  filter(stringr::str_detect(causabas, pattern_aborto)) |>
  agg_mun_ano(
    cod_col = "codmunres",    # código do município de residência
    ano_col = "ano",
    y_obito_aborto = n()
  )


# ---- A2. Óbitos neonatais (0-27 dias) — recém-nascidos --------------------

sim_rn <- sim_raw |>
  mutate(
    # idade em dias: códigos 1xxx = horas, 2xxx = dias, 3xxx = meses, 4xxx = anos
    idade_dias = case_when(
      stringr::str_starts(as.character(idade), "2") ~
        as.integer(stringr::str_sub(as.character(idade), 2)),
      stringr::str_starts(as.character(idade), "1") ~ 0L,  # horas → 0 dias
      TRUE                                            ~ NA_integer_
    )
  ) |>
  filter(!is.na(idade_dias), idade_dias <= IDADE_RN_MAX_DIAS) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano",
    y_obito_rn = n()
  )


# ---- A3. Mortalidade materna (CID O00-O99) ---------------------------------

pattern_materna <- "^O[0-9]"

sim_materna <- sim_raw |>
  filter(stringr::str_detect(causabas, pattern_materna)) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano",
    y_obito_materno = n()
  )


# ============================================================================
# BLOCO B — SINASC: Nascidos Vivos
# ============================================================================

message("\n=== SINASC — Nascidos Vivos ===")

sinasc_cache <- file.path(DIR_RAW, "datasus", "sinasc", "sinasc_nordeste.rds")

if (!file.exists(sinasc_cache)) {
  message("Baixando microdados SINASC — NE, 2000-2022...")
  message("  Isso pode levar 20-60 minutos na primeira execução.")

  sinasc_raw <- purrr::map_dfr(SIGLAS_NE, function(uf) {
    message("  SINASC — UF: ", uf)
    tryCatch(
      microdatasus::fetch_datasus(
        year_start  = ANO_INICIO,
        year_end    = ANO_FIM,
        uf          = uf,
        information_system = "SINASC"
      ),
      error = function(e) {
        message("  ERRO em ", uf, ": ", conditionMessage(e))
        NULL
      }
    )
  }) |>
    janitor::clean_names()

  saveRDS(sinasc_raw, sinasc_cache, compress = "xz")
  message("SINASC salvo em: ", sinasc_cache)
} else {
  message("Carregando SINASC do cache...")
  sinasc_raw <- readRDS(sinasc_cache)
}


# ---- B1. Total de nascidos vivos -------------------------------------------

sinasc_total <- sinasc_raw |>
  mutate(ano = as.integer(stringr::str_sub(as.character(dtnasc), nchar(as.character(dtnasc)) - 3, nchar(as.character(dtnasc))))) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano",
    y_nascidos_vivos = n()
  )


# ---- B2. Gravidez na adolescência (mãe 10-19 anos) ------------------------

sinasc_adol <- sinasc_raw |>
  mutate(
    idade_mae = as.integer(idademae),
    ano       = as.integer(stringr::str_sub(
      as.character(dtnasc),
      nchar(as.character(dtnasc)) - 3,
      nchar(as.character(dtnasc))
    ))
  ) |>
  filter(idade_mae >= IDADE_ADOL_MIN, idade_mae <= IDADE_ADOL_MAX) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano",
    y_nasc_adol      = n(),
    y_nasc_adol_10_14 = sum(idade_mae <= 14L, na.rm = TRUE),
    y_nasc_adol_15_19 = sum(idade_mae >= 15L, na.rm = TRUE)
  )


# ---- B3. Consultas de pré-natal (proxy acesso à saúde) --------------------

sinasc_prenatal <- sinasc_raw |>
  mutate(
    ano         = as.integer(stringr::str_sub(as.character(dtnasc), -4)),
    consultas_n = as.integer(consultas)
  ) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano",
    x_prenatal_media   = mean(consultas_n, na.rm = TRUE),
    x_prenatal_nenhum  = sum(consultas_n == 0L, na.rm = TRUE)
  )


# ============================================================================
# BLOCO C — SIH: Internações por CID O00-O08
# ============================================================================

message("\n=== SIH — Internações (AIH) ===")

sih_cache <- file.path(DIR_RAW, "datasus", "sih", "sih_nordeste_aborto.rds")

if (!file.exists(sih_cache)) {
  message("Baixando microdados SIH (internações) — NE, 2000-2022...")
  message("  Isso pode levar 30-90 minutos na primeira execução.")

  sih_raw <- purrr::map_dfr(SIGLAS_NE, function(uf) {
    message("  SIH — UF: ", uf)
    tryCatch(
      microdatasus::fetch_datasus(
        year_start  = ANO_INICIO,
        year_end    = ANO_FIM,
        uf          = uf,
        information_system = "SIH-RD"
      ) |>
        # Filtra no ato do download para reduzir memória
        filter(stringr::str_detect(diag_princ, "^O0[0-8]")),
      error = function(e) {
        message("  ERRO em ", uf, ": ", conditionMessage(e))
        NULL
      }
    )
  }) |>
    janitor::clean_names()

  saveRDS(sih_raw, sih_cache, compress = "xz")
  message("SIH salvo em: ", sih_cache)
} else {
  message("Carregando SIH do cache...")
  sih_raw <- readRDS(sih_cache)
}

# Internações por CID O00-O08 (aborto/complicações)
sih_aborto <- sih_raw |>
  mutate(ano = as.integer(stringr::str_sub(as.character(dt_inter), -4))) |>
  agg_mun_ano(
    cod_col = "munic_res",
    ano_col = "ano",
    y_intern_aborto = n(),
    y_intern_o03    = sum(stringr::str_starts(diag_princ, "O03"), na.rm = TRUE),
    y_intern_o04    = sum(stringr::str_starts(diag_princ, "O04"), na.rm = TRUE),
    y_intern_o07    = sum(stringr::str_starts(diag_princ, "O07"), na.rm = TRUE)
  )


# ============================================================================
# BLOCO D — Montagem do painel de saúde município-ano
# ============================================================================

message("\n=== Montando painel de saúde ===")

painel_saude <- mun_tab |>
  select(cod_mun, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS) |>
  left_join(sim_aborto,    by = c("cod_mun", "ano")) |>
  left_join(sim_rn,        by = c("cod_mun", "ano")) |>
  left_join(sim_materna,   by = c("cod_mun", "ano")) |>
  left_join(sinasc_total,  by = c("cod_mun", "ano")) |>
  left_join(sinasc_adol,   by = c("cod_mun", "ano")) |>
  left_join(sinasc_prenatal, by = c("cod_mun", "ano")) |>
  left_join(sih_aborto,    by = c("cod_mun", "ano")) |>
  # Substituir NA por 0 em contagens (ausência de registro = 0 eventos)
  mutate(across(starts_with("y_"), ~ replace_na(.x, 0L))) |>
  # Calcular taxas por 1.000 nascidos vivos
  mutate(
    tx_obito_aborto   = taxa_mil(y_obito_aborto,   pmax(y_nascidos_vivos, 1)),
    tx_obito_rn       = taxa_mil(y_obito_rn,       pmax(y_nascidos_vivos, 1)),
    tx_obito_materno  = taxa_mil(y_obito_materno,  pmax(y_nascidos_vivos, 1)),
    tx_intern_aborto  = taxa_mil(y_intern_aborto,  pmax(y_nascidos_vivos, 1)),
    tx_nasc_adol      = taxa_mil(y_nasc_adol,      pmax(y_nascidos_vivos, 1)),
    # Taxa por 100 nascidos vivos para leitura mais intuitiva
    pct_nasc_adol     = 100 * y_nasc_adol / pmax(y_nascidos_vivos, 1)
  )


# ---- Verificação rápida ----------------------------------------------------

check_saude <- painel_saude |>
  group_by(ano) |>
  summarise(
    total_nasc    = sum(y_nascidos_vivos, na.rm = TRUE),
    total_adol    = sum(y_nasc_adol,      na.rm = TRUE),
    total_obito_rn = sum(y_obito_rn,      na.rm = TRUE),
    pct_adol_media = mean(pct_nasc_adol,  na.rm = TRUE),
    .groups        = "drop"
  )

message("\nResumo anual — saúde NE:")
print(tail(check_saude, 10))


# ---- Salvar ----------------------------------------------------------------

salvar(painel_saude, "saude_panel")

message("\n04_saude_datasus.R concluído.")
