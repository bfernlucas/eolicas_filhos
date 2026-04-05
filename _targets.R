# =============================================================================
# _targets.R
# Pipeline principal — targets (refatorado: funções em vez de source())
#
# Uso:
#   library(targets)
#   tar_make()                 # Executa pipeline completo
#   tar_make(painel_final)     # Apenas até um alvo específico
#   tar_visnetwork()           # Visualiza o grafo de dependências
#   tar_outdated()             # Lista alvos desatualizados
#   tar_read(resultados_cs)    # Lê resultado de um alvo
# =============================================================================

library(targets)
library(tarchetypes)

# ---- Opções globais ---------------------------------------------------------
tar_option_set(
  packages = c(
    "tidyverse", "data.table", "janitor", "lubridate", "glue", "arrow",
    "sf", "geobr",
    "healthbR", "ipeadatar", "sidrar",
    "fixest", "did", "didimputation", "HonestDiD", "bacondecomp", "ggdid",
    "modelsummary", "gt", "kableExtra",
    "ggplot2", "patchwork", "scales", "viridis",
    "here", "fs", "progressr", "furrr"
  ),
  format            = "rds",
  memory            = "transient",
  garbage_collection = TRUE,
  seed              = 42L
)

# Carregar helpers e constantes (executado uma vez ao carregar o pipeline)
source(here::here("R", "00_setup.R"))
source(here::here("R", "functions.R"))


# ============================================================================
# FUNÇÕES DO PIPELINE (definidas aqui para serem rastreadas por targets)
# ============================================================================

# --- Geo ---------------------------------------------------------------------

build_mun_sf <- function() {
  geobr::read_municipality("all", year = 2020, simplified = FALSE,
                            showProgress = FALSE) |>
    dplyr::filter(code_state %in% as.integer(ESTADOS_NE)) |>
    dplyr::mutate(
      cod_mun  = ibge7(code_muni),
      cod_uf   = as.integer(code_state),
      nome_mun = name_muni,
      sigla_uf = abbrev_state
    ) |>
    dplyr::select(cod_mun, cod_uf, nome_mun, sigla_uf, geom)
}

build_mun_tab <- function(mun_sf) {
  mun_sf |> sf::st_drop_geometry() |> tibble::as_tibble()
}

# --- Semi-árido --------------------------------------------------------------

build_semiarido <- function(mun_tab) {
  # Snapshots via geobr (2005, 2017, 2022)
  extrair_codigos <- function(ano_geobr) {
    geobr::read_semiarid(year = ano_geobr, showProgress = FALSE) |>
      sf::st_drop_geometry() |>
      dplyr::pull(code_muni) |>
      ibge7()
  }

  codigos_2005 <- extrair_codigos(2005)
  codigos_2017 <- extrair_codigos(2017)
  codigos_2022 <- extrair_codigos(2022)

  # Incrementos manuais para resoluções sem snapshot geobr
  muns_res128_2018 <- ibge7(c(
    2101350L, 2101608L, 2103703L, 2104552L, 2105302L, 2106276L, 2108801L,
    2108909L, 2201150L, 2201556L, 2205359L, 2206241L, 2206340L, 2206696L,
    2209708L
  ))
  muns_res150_2019 <- ibge7(c(
    2902658L, 2905701L, 2912202L, 2922250L, 2924306L, 2928109L, 2929503L,
    2602803L, 2615805L
  ))

  muns_add_2017 <- setdiff(codigos_2017, codigos_2005)
  muns_add_2022 <- setdiff(
    setdiff(codigos_2022, codigos_2017),
    c(muns_res128_2018, muns_res150_2019)
  )

  semiarido_entrada <- dplyr::bind_rows(
    tibble::tibble(cod_mun = codigos_2005,     ano_entrada_semiarido = 2005L),
    tibble::tibble(cod_mun = muns_add_2017,    ano_entrada_semiarido = 2017L),
    tibble::tibble(cod_mun = muns_res128_2018, ano_entrada_semiarido = 2018L),
    tibble::tibble(cod_mun = muns_res150_2019, ano_entrada_semiarido = 2019L),
    tibble::tibble(cod_mun = muns_add_2022,    ano_entrada_semiarido = 2022L)
  ) |>
    dplyr::filter(cod_mun %in% mun_tab$cod_mun) |>
    dplyr::group_by(cod_mun) |>
    dplyr::slice_min(ano_entrada_semiarido, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  mun_tab |>
    dplyr::select(cod_mun, sigla_uf, nome_mun) |>
    tidyr::crossing(ano = ANOS) |>
    dplyr::left_join(semiarido_entrada, by = "cod_mun") |>
    dplyr::mutate(
      d_semiarido = dplyr::case_when(
        is.na(ano_entrada_semiarido)    ~ 0L,
        ano >= ano_entrada_semiarido    ~ 1L,
        TRUE                            ~ 0L
      )
    )
}

# --- ANEEL -------------------------------------------------------------------

build_eolicas <- function(mun_tab, siga_file) {
  siga <- readr::read_csv2(siga_file, show_col_types = FALSE) |>
    janitor::clean_names()

  eolicas_mun <- siga |>
    dplyr::filter(
      tip_geracao == "EOL",
      dsc_fase_usina == "Operação",
      !is.na(dat_ini_ope)
    ) |>
    dplyr::mutate(
      cod_mun = ibge7(cod_municipio_ibge),
      ano_ini = lubridate::year(lubridate::dmy(dat_ini_ope))
    ) |>
    dplyr::filter(cod_mun %in% mun_tab$cod_mun) |>
    dplyr::group_by(cod_mun) |>
    dplyr::summarise(
      ano_primeiro_eolica = min(ano_ini, na.rm = TRUE),
      n_usinas            = dplyr::n(),
      cap_total_mw        = sum(mdc_pot_instalada_kw, na.rm = TRUE) / 1000,
      .groups = "drop"
    )

  criar_backbone(mun_tab) |>
    dplyr::left_join(eolicas_mun, by = "cod_mun") |>
    dplyr::mutate(
      d_eolica  = as.integer(!is.na(ano_primeiro_eolica) & ano >= ano_primeiro_eolica),
      cap_mw_t  = dplyr::if_else(d_eolica == 1L, cap_total_mw, 0),
      log_cap_mw = log_shift(cap_mw_t, shift = 1),
      g_eolica  = dplyr::if_else(is.na(ano_primeiro_eolica), 0L,
                                  as.integer(ano_primeiro_eolica))
    )
}

# --- Painel final ------------------------------------------------------------

build_painel_final <- function(mun_tab, mun_sf,
                                eolicas_panel, semiarido_panel,
                                saude_panel, registro_civil_panel,
                                rais_panel, controles_panel) {
  backbone <- criar_backbone(mun_tab)

  painel <- backbone |>
    # Tratamento eólica
    dplyr::left_join(
      eolicas_panel |> dplyr::select(cod_mun, ano, d_eolica, cap_mw_t,
                                      log_cap_mw, g_eolica, ano_primeiro_eolica),
      by = c("cod_mun", "ano")
    ) |>
    # Semi-árido temporal
    dplyr::left_join(
      semiarido_panel |> dplyr::select(cod_mun, ano, d_semiarido,
                                        ano_entrada_semiarido),
      by = c("cod_mun", "ano")
    ) |>
    # Saúde
    dplyr::left_join(saude_panel,         by = c("cod_mun", "ano")) |>
    # Registro Civil
    dplyr::left_join(registro_civil_panel, by = c("cod_mun", "ano")) |>
    # RAIS
    dplyr::left_join(rais_panel,           by = c("cod_mun", "ano")) |>
    # Controles
    dplyr::left_join(controles_panel,      by = c("cod_mun", "ano")) |>
    # Variáveis DiD padrão
    gerar_vars_did(col_primeiro_trat = "ano_primeiro_eolica") |>
    # Zeros em contagens
    zeros_contagem() |>
    # Variáveis auxiliares
    dplyr::mutate(
      d_covid    = as.integer(ano %in% 2020:2021),
      d_semiarido = dplyr::coalesce(d_semiarido, 0L)
    ) |>
    dplyr::arrange(cod_mun, ano)

  # Verificação de balanceamento
  checar_balanceamento(painel)

  painel
}

# --- Estimação CS2021 --------------------------------------------------------

estimar_cs2021 <- function(painel, outcomes) {
  purrr::map(
    purrr::set_names(outcomes),
    ~ rodar_cs2021(.x, dados = painel)
  )
}

# --- Estimação SA2021 --------------------------------------------------------

estimar_sa2021 <- function(painel, outcomes) {
  purrr::map(
    purrr::set_names(outcomes),
    ~ rodar_sa2021(.x, dados = painel)
  )
}

# --- Estimação TWFE ----------------------------------------------------------

estimar_twfe <- function(painel, outcomes) {
  purrr::map(
    purrr::set_names(outcomes),
    ~ rodar_twfe(.x, dados = painel)
  )
}

# --- Tabela comparação -------------------------------------------------------

build_tabela_comparacao <- function(twfe_list, cs_list, outcomes, labels) {
  tabela_comparacao(twfe_list, cs_list, sa_list = NULL, outcomes, labels)
}


# ============================================================================
# DEFINIÇÃO DOS ALVOS
# ============================================================================

OUTCOMES <- c(
  "pct_nasc_adol", "tx_intern_aborto", "tx_obito_rn",
  "tx_sem_pai", "log_vinculos_energia"
)

LABELS <- c(
  pct_nasc_adol        = "Gravidez adolescente (%)",
  tx_intern_aborto     = "Internações CID O00-O08 (por 1.000 NV)",
  tx_obito_rn          = "Óbitos neonatais (por 1.000 NV)",
  tx_sem_pai           = "Nascimentos sem pai (%)",
  log_vinculos_energia = "ln(Vínculos energia + 1)"
)

list(

  # --------------------------------------------------------------------------
  # RASTREAMENTO DE ARQUIVOS FONTE
  # Os scripts são rastreados como alvos de arquivo: se um script mudar,
  # todos os alvos que dele dependem são re-executados.
  # --------------------------------------------------------------------------

  tar_target(scripts_r,
    c("R/00_setup.R", "R/functions.R",
      "R/03_eolicas_aneel.R", "R/04_saude_datasus.R",
      "R/05_registro_civil.R", "R/06_rais.R",
      "R/07_controles_ipea.R", "R/08_build_panel.R",
      "R/09_descritivas.R",    "R/10_staggered_did.R",
      "R/11_robustez.R"),
    format = "file"
  ),

  # Arquivo ANEEL (rastreado como arquivo externo)
  tar_target(
    siga_file,
    {
      dest <- file.path(DIR_RAW, "aneel", "siga_empreendimentos.csv")
      if (!file.exists(dest))
        stop("ANEEL SIGA não encontrado em: ", dest,
             "\nExecute: make download-aneel")
      dest
    },
    format = "file",
    cue    = tarchetypes::tar_cue_age(
      name = siga_file,
      age  = as.difftime(30, units = "days")  # re-verifica após 30 dias
    )
  ),

  # --------------------------------------------------------------------------
  # DADOS GEOGRÁFICOS
  # --------------------------------------------------------------------------

  tar_target(mun_sf,  build_mun_sf()),
  tar_target(mun_tab, build_mun_tab(mun_sf)),

  # --------------------------------------------------------------------------
  # SEMI-ÁRIDO (variação temporal via geobr)
  # --------------------------------------------------------------------------

  tar_target(semiarido_panel, build_semiarido(mun_tab)),

  # --------------------------------------------------------------------------
  # TRATAMENTO: EÓLICAS (ANEEL SIGA)
  # --------------------------------------------------------------------------

  tar_target(eolicas_panel, build_eolicas(mun_tab, siga_file)),

  # --------------------------------------------------------------------------
  # SAÚDE: DataSUS (healthbR — executa script por ser muito complexo para inline)
  # --------------------------------------------------------------------------

  tar_target(
    saude_panel,
    {
      source(here::here("R", "04_saude_datasus.R"))
      carregar("saude_panel")
    },
    deps = list(mun_tab, scripts_r)
  ),

  # --------------------------------------------------------------------------
  # REGISTRO CIVIL: paternidade
  # --------------------------------------------------------------------------

  tar_target(
    registro_civil_panel,
    {
      source(here::here("R", "05_registro_civil.R"))
      carregar("registro_civil_panel")
    },
    deps = list(mun_tab, scripts_r)
  ),

  # --------------------------------------------------------------------------
  # EMPREGO: RAIS (via basedosdados/BigQuery)
  # --------------------------------------------------------------------------

  tar_target(
    rais_panel,
    {
      source(here::here("R", "06_rais.R"))
      carregar("rais_panel")
    },
    deps = list(mun_tab, scripts_r)
  ),

  # --------------------------------------------------------------------------
  # CONTROLES: IPEA / IBGE
  # --------------------------------------------------------------------------

  tar_target(
    controles_panel,
    {
      source(here::here("R", "07_controles_ipea.R"))
      carregar("controles_panel")
    },
    deps = list(mun_tab, mun_sf, scripts_r)
  ),

  # --------------------------------------------------------------------------
  # PAINEL FINAL
  # --------------------------------------------------------------------------

  tar_target(
    painel_final,
    build_painel_final(
      mun_tab, mun_sf,
      eolicas_panel, semiarido_panel,
      saude_panel, registro_civil_panel,
      rais_panel, controles_panel
    )
  ),

  # Salva CSV para referência externa (planilhas, Stata, etc.)
  tar_target(
    painel_csv,
    {
      readr::write_csv(painel_final,
                       file.path(DIR_FINAL, "painel_final.csv"))
      file.path(DIR_FINAL, "painel_final.csv")
    },
    format = "file",
    deps   = painel_final
  ),

  # Verificação de qualidade
  tar_target(
    painel_ok,
    {
      n_ids   <- dplyr::n_distinct(painel_final$cod_mun)
      n_anos  <- dplyr::n_distinct(painel_final$ano)
      n_obs   <- nrow(painel_final)
      stopifnot(
        "Painel desbalanceado" = (n_obs == n_ids * n_anos),
        "Duplicatas"           = !anyDuplicated(painel_final[c("cod_mun", "ano")]),
        "Anos faltando"        = all(ANOS %in% unique(painel_final$ano))
      )
      message("Painel OK: ", n_obs, " obs | ", n_ids, " mun | ", n_anos, " anos")
      TRUE
    }
  ),

  # --------------------------------------------------------------------------
  # DESCRITIVAS
  # --------------------------------------------------------------------------

  tar_target(
    descritivas,
    {
      source(here::here("R", "09_descritivas.R"))
      TRUE
    },
    deps = list(painel_final, mun_sf, painel_ok, scripts_r)
  ),

  # --------------------------------------------------------------------------
  # ESTIMAÇÃO: STAGGERED DiD
  # --------------------------------------------------------------------------

  tar_target(
    resultados_twfe,
    estimar_twfe(painel_final, OUTCOMES),
    deps = painel_ok
  ),

  tar_target(
    resultados_cs,
    estimar_cs2021(painel_final, OUTCOMES),
    deps = painel_ok
  ),

  tar_target(
    resultados_sa,
    estimar_sa2021(painel_final, OUTCOMES),
    deps = painel_ok
  ),

  # ATT simples de CS2021 (tabela principal)
  tar_target(
    att_simples,
    purrr::map_dfr(
      OUTCOMES,
      ~ extrair_att_simples(resultados_cs[[.x]], outcome = .x)
    )
  ),

  # Tabela comparação de estimadores
  tar_target(
    tabela_comparacao_estimadores,
    tabela_comparacao(
      twfe_list = resultados_twfe,
      cs_list   = resultados_cs,
      sa_list   = resultados_sa,
      outcomes  = OUTCOMES,
      labels    = LABELS
    )
  ),

  # Estudos de evento (gráficos via ggdid)
  tar_target(
    event_study_plots,
    {
      source(here::here("R", "10_staggered_did.R"))
      TRUE
    },
    deps = list(resultados_cs, resultados_sa, resultados_twfe, scripts_r)
  ),

  # --------------------------------------------------------------------------
  # ROBUSTEZ
  # --------------------------------------------------------------------------

  tar_target(
    robustez,
    {
      source(here::here("R", "11_robustez.R"))
      TRUE
    },
    deps = list(painel_final, resultados_cs, tabela_comparacao_estimadores,
                scripts_r)
  ),

  # --------------------------------------------------------------------------
  # SAÍDA FINAL
  # --------------------------------------------------------------------------

  # Salva tabela de comparação de estimadores em CSV e LaTeX
  tar_target(
    tabelas_finais,
    {
      readr::write_csv(
        tabela_comparacao_estimadores,
        file.path(DIR_TABLES, "tab_comparacao_estimadores.csv")
      )
      readr::write_csv(
        att_simples,
        file.path(DIR_TABLES, "tab_att_simples_cs2021.csv")
      )
      message("Tabelas finais salvas em ", DIR_TABLES)
      TRUE
    },
    deps = list(tabela_comparacao_estimadores, att_simples,
                event_study_plots, robustez)
  )
)
