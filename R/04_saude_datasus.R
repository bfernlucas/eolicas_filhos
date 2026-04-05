# =============================================================================
# 04_saude_datasus.R
# Desfechos de saúde — DataSUS via pacote healthbR
#
# Pacote: healthbR (CRAN, v0.2.0+, Sidney Bissoli)
#   install.packages("healthbR")
#
# Sistemas utilizados:
#   SIM    → sim_data()    — óbitos por CID O00-O08 e neonatais (0-27 dias)
#   SINASC → sinasc_data() — nascidos vivos e gravidez na adolescência
#   SIH    → sih_data()    — internações por CID O00-O08
#
# Vantagens do healthbR sobre microdatasus:
#   ✓ Filtro nativo por CID-10 (parâmetro `cause` / `anomaly`)
#   ✓ Cache automático em Parquet (arrow)
#   ✓ API uniforme: *_years(), *_variables(), *_dictionary(), *_data()
#   ✓ Retorna data.frame tidy (tidyverse-ready)
#   ✓ No CRAN — sem dependência de instalação via GitHub
#
# Saída:
#   data/processed/saude_panel.rds  — tibble município-ano com variáveis y_/tx_
# =============================================================================

source(here::here("R", "00_setup.R"))

mun_tab <- carregar("municipios_nordeste_tab")


# ---- Explorar o pacote (rode uma vez para conhecer os dados) ---------------
# sim_years()           # anos disponíveis no SIM
# sim_variables()       # todas as variáveis
# sim_variables(search = "causa")  # busca por nome
# sim_dictionary("SEXO")           # categorias de uma variável
# sih_data() organiza por mês — precisa de month = 1:12


# ---- Função auxiliar: colapsa em município × ano ---------------------------

agg_mun_ano <- function(df, cod_col, ano_col, ...) {
  df |>
    rename(cod_mun_raw = all_of(cod_col),
           ano         = all_of(ano_col)) |>
    mutate(
      cod_mun = as.integer(stringr::str_sub(as.character(cod_mun_raw), 1, 7)),
      ano     = as.integer(ano)
    ) |>
    filter(
      cod_mun %in% mun_tab$cod_mun,
      ano >= ANO_INICIO,
      ano <= ANO_FIM
    ) |>
    group_by(cod_mun, ano) |>
    summarise(..., .groups = "drop")
}


# ============================================================================
# BLOCO A — SIM: Sistema de Informações sobre Mortalidade
#
# healthbR::sim_data():
#   year  : ano (inteiro)
#   uf    : sigla da UF ("CE", "RN", etc.)
#   cause : prefixo CID-10 — "O0" captura O00 a O09
#   vars  : vetor de colunas (NULL = todas)
# ============================================================================

message("\n=== SIM — Óbitos (healthbR) ===")
message("Anos disponíveis: ", paste(healthbR::sim_years(), collapse = ", "))

sim_cache <- file.path(DIR_RAW, "datasus", "sim", "sim_nordeste_healthbr.rds")

if (!file.exists(sim_cache)) {
  message("Baixando SIM via healthbR — NE, ", ANO_INICIO, "-", ANO_FIM, "...")
  message("  Isso pode levar 20-60 min (cache automático do healthbR ativado).")

  # Variáveis necessárias — usar subset para economizar memória
  vars_sim <- c("CAUSABAS", "DTOBITO", "CODMUNRES", "SEXO", "IDADE",
                "IDADEMAE", "OBITOPARTO")

  sim_raw <- purrr::map_dfr(SIGLAS_NE, function(uf) {
    message("  SIM — ", uf)

    # Download de TODOS os óbitos (filtra depois por CID)
    # Nota: healthbR suporta cause = "O" para capturar capítulo inteiro
    # Mas baixamos tudo e filtramos para ter óbitos neonatais (CID variado)
    tryCatch(
      healthbR::sim_data(
        year = ANO_INICIO:ANO_FIM,
        uf   = uf,
        vars = vars_sim
      ),
      error = function(e) {
        message("  ERRO SIM [", uf, "]: ", conditionMessage(e))
        NULL
      }
    )
  }) |>
    janitor::clean_names()

  saveRDS(sim_raw, sim_cache, compress = "xz")
  message("SIM salvo: ", sim_cache)
} else {
  message("Carregando SIM do cache...")
  sim_raw <- readRDS(sim_cache)
}


# ---- A1. Óbitos CID O00-O08 ------------------------------------------------
# healthbR já pode filtrar via cause = "O0", mas como baixamos tudo,
# filtramos aqui para ter também os óbitos neonatais

sim_aborto <- sim_raw |>
  filter(stringr::str_detect(causabas, "^O0[0-8]")) |>
  mutate(
    ano_obito = as.integer(stringr::str_sub(as.character(dtobito), 5, 8))
  ) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano_obito",
    y_obito_aborto   = n(),
    y_obito_o03      = sum(stringr::str_starts(causabas, "O03"), na.rm = TRUE),
    y_obito_o04      = sum(stringr::str_starts(causabas, "O04"), na.rm = TRUE),
    y_obito_ectopico = sum(stringr::str_starts(causabas, "O00"), na.rm = TRUE)
  )

# ---- A2. Óbitos neonatais (0-27 dias) -------------------------------------
# Encoding da variável IDADE no SIM:
#   Primeiro dígito = unidade: 1=horas, 2=dias, 3=meses, 4=anos
#   Ex: "215" = 15 dias; "112" = 12 horas (→ 0 dias)

sim_rn <- sim_raw |>
  mutate(
    idade_raw   = as.character(idade),
    unidade     = stringr::str_sub(idade_raw, 1, 1),
    valor_idade = suppressWarnings(as.integer(stringr::str_sub(idade_raw, 2))),
    idade_dias  = case_when(
      unidade == "2" ~ valor_idade,          # dias diretamente
      unidade == "1" ~ 0L,                   # horas → 0 dias
      TRUE           ~ NA_integer_
    ),
    ano_obito = as.integer(stringr::str_sub(as.character(dtobito), 5, 8))
  ) |>
  filter(!is.na(idade_dias), idade_dias <= IDADE_RN_MAX_DIAS) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano_obito",
    y_obito_rn = n()
  )

# ---- A3. Mortalidade materna (CID O00-O99) --------------------------------

sim_materna <- sim_raw |>
  filter(stringr::str_detect(causabas, "^O")) |>
  mutate(ano_obito = as.integer(stringr::str_sub(as.character(dtobito), 5, 8))) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano_obito",
    y_obito_materno = n()
  )

message("✓ SIM processado: ",
        sum(sim_aborto$y_obito_aborto, na.rm = TRUE), " óbitos CID O00-O08")


# ============================================================================
# BLOCO B — SINASC: Nascidos Vivos
#
# healthbR::sinasc_data():
#   year    : ano(s)
#   uf      : sigla da UF
#   anomaly : prefixo CID-10 para anomalia congênita (CODANOMAL)
#   vars    : colunas específicas
# ============================================================================

message("\n=== SINASC — Nascidos Vivos (healthbR) ===")

sinasc_cache <- file.path(DIR_RAW, "datasus", "sinasc", "sinasc_nordeste_healthbr.rds")

if (!file.exists(sinasc_cache)) {
  message("Baixando SINASC via healthbR — NE, ", ANO_INICIO, "-", ANO_FIM, "...")

  vars_sinasc <- c("DTNASC", "CODMUNRES", "IDADEMAE", "CONSULTAS",
                   "GESTACAO", "PARTO", "PESO", "SEXO", "CODANOMAL",
                   "ESCMAE", "CODESTAB")

  sinasc_raw <- purrr::map_dfr(SIGLAS_NE, function(uf) {
    message("  SINASC — ", uf)
    tryCatch(
      healthbR::sinasc_data(
        year = ANO_INICIO:ANO_FIM,
        uf   = uf,
        vars = vars_sinasc
      ),
      error = function(e) {
        message("  ERRO SINASC [", uf, "]: ", conditionMessage(e))
        NULL
      }
    )
  }) |>
    janitor::clean_names()

  saveRDS(sinasc_raw, sinasc_cache, compress = "xz")
  message("SINASC salvo: ", sinasc_cache)
} else {
  message("Carregando SINASC do cache...")
  sinasc_raw <- readRDS(sinasc_cache)
}

# Extrair ano a partir de DTNASC (formato DDMMAAAA)
sinasc_raw <- sinasc_raw |>
  mutate(
    ano_nasc  = as.integer(stringr::str_sub(as.character(dtnasc), 5, 8)),
    idade_mae = suppressWarnings(as.integer(idademae))
  )

# ---- B1. Total de nascidos vivos -------------------------------------------

sinasc_total <- sinasc_raw |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano_nasc",
    y_nascidos_vivos = n()
  )

# ---- B2. Gravidez na adolescência (mãe 10-19 anos) ------------------------

sinasc_adol <- sinasc_raw |>
  filter(idade_mae >= IDADE_ADOL_MIN, idade_mae <= IDADE_ADOL_MAX) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano_nasc",
    y_nasc_adol        = n(),
    y_nasc_adol_10_14  = sum(idade_mae <= 14L, na.rm = TRUE),
    y_nasc_adol_15_19  = sum(idade_mae >= 15L, na.rm = TRUE)
  )

# ---- B3. Pré-natal (proxy acesso à saúde) ----------------------------------

sinasc_prenatal <- sinasc_raw |>
  mutate(consultas_n = suppressWarnings(as.integer(consultas))) |>
  agg_mun_ano(
    cod_col = "codmunres",
    ano_col = "ano_nasc",
    x_prenatal_media  = mean(consultas_n, na.rm = TRUE),
    x_prenatal_nenhum = sum(consultas_n == 0L, na.rm = TRUE)
  )

message("✓ SINASC processado: ",
        sum(sinasc_total$y_nascidos_vivos, na.rm = TRUE), " nascimentos")


# ============================================================================
# BLOCO C — SIH: Internações Hospitalares
#
# healthbR::sih_data():
#   year  : ano(s)
#   month : mês(es) — SIH é organizado MENSALMENTE (1:12)
#   uf    : sigla da UF
#   vars  : colunas específicas
#
# ATENÇÃO: Diferentemente do SIM e SINASC (arquivos anuais por UF),
# o SIH tem 1 arquivo por UF por MÊS. healthbR itera automaticamente.
# Filtrar por DIAG_PRINC após o download (sem parâmetro `cause` nativo).
# ============================================================================

message("\n=== SIH — Internações (healthbR) ===")

# Verificar variáveis disponíveis
# healthbR::sih_variables() — descomentar para explorar
# healthbR::sih_dictionary("DIAG_PRINC")

sih_cache <- file.path(DIR_RAW, "datasus", "sih", "sih_nordeste_aborto_healthbr.rds")

if (!file.exists(sih_cache)) {
  message("Baixando SIH via healthbR — NE, ", ANO_INICIO, "-", ANO_FIM, "...")
  message("  SIH é organizado por mês — healthbR itera automaticamente.")
  message("  Isso pode levar 60-120 min na primeira execução.")

  vars_sih <- c("DIAG_PRINC", "DIAG_SEC", "MUNIC_RES", "ANO_CMPT", "MES_CMPT",
                "MORTE", "DIAS_PERM", "IDADE", "SEXO", "PROC_REA")

  sih_raw <- purrr::map_dfr(SIGLAS_NE, function(uf) {
    message("  SIH — ", uf)
    tryCatch(
      healthbR::sih_data(
        year  = ANO_INICIO:ANO_FIM,
        month = 1:12,
        uf    = uf,
        vars  = vars_sih
      ) |>
        # Filtrar CID O00-O08 logo após download para economizar RAM
        filter(stringr::str_detect(DIAG_PRINC, "^O0[0-8]")),
      error = function(e) {
        message("  ERRO SIH [", uf, "]: ", conditionMessage(e))
        NULL
      }
    )
  }) |>
    janitor::clean_names()

  saveRDS(sih_raw, sih_cache, compress = "xz")
  message("SIH salvo: ", sih_cache)
} else {
  message("Carregando SIH do cache...")
  sih_raw <- readRDS(sih_cache)
}

# ---- C1. Internações por CID O00-O08 ---------------------------------------

sih_aborto <- sih_raw |>
  mutate(ano = suppressWarnings(as.integer(ano_cmpt))) |>
  agg_mun_ano(
    cod_col = "munic_res",
    ano_col = "ano",
    y_intern_aborto  = n(),
    y_intern_o03     = sum(stringr::str_starts(diag_princ, "O03"), na.rm = TRUE),
    y_intern_o04     = sum(stringr::str_starts(diag_princ, "O04"), na.rm = TRUE),
    y_intern_o05     = sum(stringr::str_starts(diag_princ, "O05"), na.rm = TRUE),
    y_intern_o06     = sum(stringr::str_starts(diag_princ, "O06"), na.rm = TRUE),
    y_intern_o07     = sum(stringr::str_starts(diag_princ, "O07"), na.rm = TRUE),
    # Mortalidade hospitalar por aborto
    y_morte_intern_aborto = sum(as.integer(morte) == 1L, na.rm = TRUE),
    # Dias de permanência média (proxy gravidade)
    x_dias_perm_aborto    = mean(suppressWarnings(as.numeric(dias_perm)), na.rm = TRUE)
  )

message("✓ SIH processado: ",
        sum(sih_aborto$y_intern_aborto, na.rm = TRUE), " internações CID O00-O08")


# ============================================================================
# BLOCO D — Montagem do painel de saúde município-ano
# ============================================================================

message("\n=== Montando painel de saúde ===")

painel_saude <- mun_tab |>
  select(cod_mun, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS) |>
  left_join(sim_aborto,     by = c("cod_mun", "ano")) |>
  left_join(sim_rn,         by = c("cod_mun", "ano")) |>
  left_join(sim_materna,    by = c("cod_mun", "ano")) |>
  left_join(sinasc_total,   by = c("cod_mun", "ano")) |>
  left_join(sinasc_adol,    by = c("cod_mun", "ano")) |>
  left_join(sinasc_prenatal, by = c("cod_mun", "ano")) |>
  left_join(sih_aborto,     by = c("cod_mun", "ano")) |>
  # Zeros em contagens (ausência de evento = 0, não NA)
  mutate(across(starts_with("y_"), ~ replace_na(.x, 0L))) |>
  # Taxas por nascidos vivos (denominador: máximo 1 para evitar divisão por zero)
  mutate(
    tx_obito_aborto   = taxa_mil(y_obito_aborto,   pmax(y_nascidos_vivos, 1)),
    tx_obito_rn       = taxa_mil(y_obito_rn,       pmax(y_nascidos_vivos, 1)),
    tx_obito_materno  = taxa_mil(y_obito_materno,  pmax(y_nascidos_vivos, 1)),
    tx_intern_aborto  = taxa_mil(y_intern_aborto,  pmax(y_nascidos_vivos, 1)),
    tx_nasc_adol      = taxa_mil(y_nasc_adol,      pmax(y_nascidos_vivos, 1)),
    pct_nasc_adol     = 100 * y_nasc_adol / pmax(y_nascidos_vivos, 1),
    pct_adol_10_14    = 100 * y_nasc_adol_10_14 / pmax(y_nascidos_vivos, 1),
    # Taxa de mortalidade hospitalar por aborto
    tx_morte_intern   = 100 * y_morte_intern_aborto / pmax(y_intern_aborto, 1)
  )


# ---- Verificação -----------------------------------------------------------

message("\n--- Verificação do painel de saúde ---")
message("Observações: ", nrow(painel_saude))
message("Municípios:  ", n_distinct(painel_saude$cod_mun))
message("Anos:        ", min(painel_saude$ano), "-", max(painel_saude$ano))

check_saude <- painel_saude |>
  group_by(ano) |>
  summarise(
    total_nasc        = sum(y_nascidos_vivos, na.rm = TRUE),
    total_adol        = sum(y_nasc_adol,      na.rm = TRUE),
    total_obito_rn    = sum(y_obito_rn,       na.rm = TRUE),
    total_obito_abor  = sum(y_obito_aborto,   na.rm = TRUE),
    total_intern_abor = sum(y_intern_aborto,  na.rm = TRUE),
    pct_adol_media    = round(mean(pct_nasc_adol, na.rm = TRUE), 2),
    .groups = "drop"
  )

message("\nSérie temporal — resumo NE:")
print(check_saude |> filter(ano %in% c(2000, 2005, 2010, 2015, 2020, 2022)))


# ---- Salvar ----------------------------------------------------------------

salvar(painel_saude, "saude_panel")

# Exportar resumo para conferência
readr::write_csv(check_saude, file.path(DIR_PROCESSED, "saude_serie_anual.csv"))

message("\n04_saude_datasus.R concluído.")
message("healthbR version: ", as.character(packageVersion("healthbR")))


# ============================================================================
# APÊNDICE: Como usar healthbR para exploração interativa
# ============================================================================
#
# # Explorar variáveis disponíveis:
# healthbR::sim_years()                   # anos disponíveis
# healthbR::sim_variables()               # todas as variáveis
# healthbR::sim_variables(search = "cau") # busca por nome
# healthbR::sim_dictionary("CAUSABAS")    # categorias de CAUSABAS
# healthbR::sim_dictionary("SEXO")        # categorias de SEXO
#
# # Exemplo de download direto filtrado (modo alternativo):
# obitos_o03_ce <- healthbR::sim_data(
#   year  = 2022,
#   uf    = "CE",
#   cause = "O03"   # apenas aborto espontâneo
# )
#
# # SINASC: explorar pré-natal
# healthbR::sinasc_variables(search = "consul")
# healthbR::sinasc_dictionary("CONSULTAS")
#
# # SIH: variáveis e cache
# healthbR::sih_years()
# healthbR::sih_variables()
# healthbR::sih_cache_status()   # ver o que está em cache
# healthbR::sih_clear_cache()    # limpar cache se necessário
