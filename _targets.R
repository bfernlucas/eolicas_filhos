# =============================================================================
# _targets.R
# Pipeline principal — targets
#
# Uso:
#   library(targets)
#   tar_make()          # Executa pipeline completo
#   tar_make(y_principal)  # Executa apenas até um alvo específico
#   tar_visnetwork()    # Visualiza o grafo de dependências
#   tar_outdated()      # Lista alvos desatualizados
#
# Cada tar_target() é re-executado somente se suas dependências mudarem.
# =============================================================================

library(targets)
library(tarchetypes)

# ---- Opções globais ---------------------------------------------------------
tar_option_set(
  packages = c(
    "tidyverse", "data.table", "janitor", "lubridate", "glue",
    "sf", "geobr",
    "microdatasus", "basedosdados", "ipeadatar",
    "fixest", "did", "didimputation", "HonestDiD", "bacondecomp",
    "modelsummary", "gt", "ggplot2", "patchwork", "viridis",
    "here", "fs", "progressr"
  ),
  format   = "rds",          # formato padrão de serialização
  memory   = "transient",    # libera memória após uso do alvo
  garbage_collection = TRUE
)

# Carregar helpers e constantes
source(here::here("R", "00_setup.R"))

# ============================================================================
# DEFINIÇÃO DOS ALVOS
# ============================================================================

list(

  # --------------------------------------------------------------------------
  # DADOS GEOGRÁFICOS
  # --------------------------------------------------------------------------

  tar_target(
    mun_sf,
    {
      mun_br <- geobr::read_municipality("all", year = 2020, simplified = FALSE)
      mun_br |>
        filter(code_state %in% as.integer(ESTADOS_NE)) |>
        mutate(
          cod_mun  = as.integer(code_muni),
          cod_uf   = as.integer(code_state),
          nome_mun = name_muni,
          sigla_uf = abbrev_state
        ) |>
        select(cod_mun, cod_uf, nome_mun, sigla_uf, geom)
    },
    format = "rds"
  ),

  tar_target(
    mun_tab,
    mun_sf |> sf::st_drop_geometry() |> as_tibble()
  ),

  # --------------------------------------------------------------------------
  # SEMI-ÁRIDO SUDENE (variação temporal)
  # --------------------------------------------------------------------------

  tar_target(
    semiarido_entrada,
    {
      source(here::here("R", "02_semiarido_sudene.R"))
      carregar("semiarido_entrada")
    }
  ),

  tar_target(
    semiarido_panel,
    {
      source(here::here("R", "02_semiarido_sudene.R"))
      carregar("semiarido_panel")
    },
    deps = semiarido_entrada
  ),

  # --------------------------------------------------------------------------
  # TRATAMENTO: EÓLICAS (ANEEL SIGA)
  # --------------------------------------------------------------------------

  tar_target(
    aneel_siga_file,
    {
      dest <- file.path(DIR_RAW, "aneel", "siga_empreendimentos.csv")
      if (!file.exists(dest)) {
        download.file(
          paste0("https://dadosabertos.aneel.gov.br/dataset/",
                 "1213b4c3-8a02-4dc5-9b0e-25dff7b6d63e/resource/",
                 "b1bd71e7-d0ad-4214-9053-cbd58e9564a7/download/",
                 "siga-empreendimentos-geracao.csv"),
          destfile = dest, mode = "wb"
        )
      }
      dest
    },
    format = "file"
  ),

  tar_target(
    eolicas_panel,
    {
      source(here::here("R", "03_eolicas_aneel.R"))
      carregar("eolicas_panel")
    },
    deps = list(aneel_siga_file, mun_tab)
  ),

  # --------------------------------------------------------------------------
  # SAÚDE: DataSUS (SIM, SINASC, SIH)
  # --------------------------------------------------------------------------

  tar_target(
    saude_panel,
    {
      source(here::here("R", "04_saude_datasus.R"))
      carregar("saude_panel")
    },
    deps = mun_tab
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
    deps = mun_tab
  ),

  # --------------------------------------------------------------------------
  # EMPREGO: RAIS
  # --------------------------------------------------------------------------

  tar_target(
    rais_panel,
    {
      source(here::here("R", "06_rais.R"))
      carregar("rais_panel")
    },
    deps = mun_tab
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
    deps = list(mun_tab, mun_sf)
  ),

  # --------------------------------------------------------------------------
  # PAINEL FINAL
  # --------------------------------------------------------------------------

  tar_target(
    painel_final,
    {
      source(here::here("R", "08_build_panel.R"))
      readRDS(file.path(DIR_FINAL, "painel_final.rds"))
    },
    deps = list(
      mun_tab, mun_sf,
      eolicas_panel, semiarido_panel,
      saude_panel, registro_civil_panel,
      rais_panel, controles_panel
    )
  ),

  # Verificação de qualidade do painel
  tar_target(
    painel_checks,
    {
      stopifnot(
        "Painel desbalanceado" = nrow(painel_final) ==
          n_distinct(painel_final$cod_mun) * length(ANOS),
        "Duplicatas" = !anyDuplicated(painel_final[c("cod_mun", "ano")])
      )
      message("✓ Painel balanceado: ", nrow(painel_final), " obs")
      TRUE
    },
    deps = painel_final
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
    deps = list(painel_final, mun_sf)
  ),

  # --------------------------------------------------------------------------
  # ESTIMAÇÃO: STAGGERED DiD
  # --------------------------------------------------------------------------

  tar_target(
    resultados_twfe,
    {
      OUTCOMES <- c("pct_nasc_adol", "tx_intern_aborto", "tx_obito_rn",
                    "tx_sem_pai", "log_vinculos_energia")
      lapply(OUTCOMES, function(y) {
        fixest::feols(
          fml     = as.formula(paste0(
            y, " ~ d_post + d_semiarido + d_covid | cod_mun + ano"
          )),
          data    = painel_final,
          cluster = ~cod_mun
        )
      }) |> setNames(OUTCOMES)
    },
    deps = list(painel_final, painel_checks)
  ),

  tar_target(
    resultados_cs2021,
    {
      source(here::here("R", "10_staggered_did.R"))
      readRDS(file.path(DIR_TABLES, "cs2021_att_gt.rds"))
    },
    deps = list(painel_final, painel_checks)
  ),

  tar_target(
    agregados_cs2021,
    {
      readRDS(file.path(DIR_TABLES, "cs2021_agregado.rds"))
    },
    deps = resultados_cs2021
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
    deps = list(painel_final, resultados_cs2021, agregados_cs2021)
  ),

  # --------------------------------------------------------------------------
  # TABELAS FINAIS
  # --------------------------------------------------------------------------

  tar_target(
    tabelas,
    {
      # Tabela principal CS2021
      OUTCOMES <- c("pct_nasc_adol", "tx_intern_aborto", "tx_obito_rn",
                    "tx_sem_pai", "log_vinculos_energia")
      LABELS   <- c(
        pct_nasc_adol        = "Gravidez adolescente (%)",
        tx_intern_aborto     = "Internações CID O00-O08 (por 1.000 NV)",
        tx_obito_rn          = "Óbitos neonatais (por 1.000 NV)",
        tx_sem_pai           = "Nascimentos sem pai (%)",
        log_vinculos_energia = "ln(Vínculos em energia + 1)"
      )

      tab_principal <- purrr::map_dfr(OUTCOMES, function(y) {
        ag <- agregados_cs2021[[y]]$simple
        if (is.null(ag)) return(tibble(variavel = LABELS[y], att = NA, se = NA, n = NA))
        tibble(
          variavel = LABELS[y],
          att      = ag$overall.att,
          se       = ag$overall.se,
          n        = ag$DID.se  # número de observações
        )
      }) |>
        mutate(
          ci95_low  = att - 1.96 * se,
          ci95_high = att + 1.96 * se,
          p_value   = 2 * pnorm(-abs(att / se)),
          stars     = case_when(
            p_value < 0.01 ~ "***", p_value < 0.05 ~ "**",
            p_value < 0.10 ~ "*",   TRUE ~ ""
          )
        )

      readr::write_csv(tab_principal,
                       file.path(DIR_TABLES, "tab_principal_cs2021.csv"))
      message("✓ Tabela principal CS2021 salva")
      TRUE
    },
    deps = list(resultados_twfe, agregados_cs2021, robustez)
  )
)
