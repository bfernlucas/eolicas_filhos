# =============================================================================
# 11_robustez.R
# Verificações de robustez, testes de falsificação e análises adicionais
#
# Verificações implementadas:
#   1. Goodman-Bacon decomposition (bacondecomp)
#   2. CS2021 com grupo controle alternativo: "notyettreated"
#   3. Placebo: atribuição aleatória do ano de tratamento
#   4. Excluindo adotantes precoces (antes de 2008)
#   5. Excluindo anos COVID (2020-2021)
#   6. CS2021 com antecipação = 1 (obras pré-operação)
#   7. Honest DiD (Rambachan & Roth 2023) — análise de sensibilidade
#   8. Heterogeneidade: semi-árido vs fora do semi-árido
#   9. Análise de intensidade: capacidade instalada em MW (log)
#  10. Teste de especificação: resultados placebo (não afetáveis por eólicas)
#
# Saídas em output/tables/ e output/figures/
# =============================================================================

source(here::here("R", "00_setup.R"))

painel <- readRDS(file.path(DIR_FINAL, "painel_final.rds"))

# Carrega resultados da estimação principal
cs_resultados <- readRDS(file.path(DIR_TABLES, "cs2021_att_gt.rds"))
cs_agregado   <- readRDS(file.path(DIR_TABLES, "cs2021_agregado.rds"))

OUTCOMES <- c(
  "pct_nasc_adol",
  "tx_intern_aborto",
  "tx_obito_rn",
  "tx_sem_pai",
  "log_vinculos_energia"
)

OUTCOME_LABELS <- c(
  pct_nasc_adol        = "Gravidez adolescente (%)",
  tx_intern_aborto     = "Internações CID O00-O08 (por 1.000 NV)",
  tx_obito_rn          = "Óbitos neonatais (por 1.000 NV)",
  tx_sem_pai           = "Nascimentos sem pai (%)",
  log_vinculos_energia = "ln(Vínculos em energia + 1)"
)

# Outcome principal para robustez detalhada
Y_PRINCIPAL <- "pct_nasc_adol"

painel_cs <- painel |>
  mutate(g_cs = if_else(is.na(ano_primeiro_eolica), 0L, as.integer(ano_primeiro_eolica)))


# ============================================================================
# 1. GOODMAN-BACON DECOMPOSITION
# ============================================================================

message("\n=== Goodman-Bacon Decomposition ===")

bacon_res <- lapply(OUTCOMES[1:3], function(y) {
  message("  Bacon — ", y)
  tryCatch({
    df_bacon <- painel |>
      filter(!is.na(get(y))) |>
      mutate(d_treat = as.integer(!is.na(ano_primeiro_eolica)))
    bacondecomp::bacon(
      formula  = as.formula(paste0(y, " ~ d_post")),
      data     = df_bacon,
      id_var   = "cod_mun",
      time_var = "ano"
    )
  }, error = function(e) {
    message("  ERRO Bacon [", y, "]: ", conditionMessage(e))
    NULL
  })
})
names(bacon_res) <- OUTCOMES[1:3]

# Figura Bacon para outcome principal
if (!is.null(bacon_res[[Y_PRINCIPAL]])) {
  bacon_df <- bacon_res[[Y_PRINCIPAL]]
  fig_bacon <- bacon_df |>
    ggplot(aes(x = weight, y = estimate, colour = type, shape = type)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_point(size = 3, alpha = 0.8) +
    scale_colour_brewer(palette = "Set1", name = "Comparação") +
    scale_shape_discrete(name = "Comparação") +
    labs(
      title    = "Goodman-Bacon Decomposition",
      subtitle = paste0("Variável: ", OUTCOME_LABELS[Y_PRINCIPAL]),
      x        = "Peso na estimativa TWFE",
      y        = "Estimativa 2×2",
      caption  = "Fonte: bacondecomp"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(
    file.path(DIR_FIGURES, "11_bacon_decomp.png"),
    plot = fig_bacon, width = 9, height = 6, dpi = 300
  )
  message("✓ Bacon decomposition salvo")
}


# ============================================================================
# 2. CS2021 — GRUPO CONTROLE ALTERNATIVO: "notyettreated"
# ============================================================================

message("\n=== CS2021 com notyettreated ===")

cs_nyt <- lapply(OUTCOMES, function(y) {
  message("  CS nyt — ", y)
  tryCatch(
    did::att_gt(
      yname         = y,
      tname         = "ano",
      idname        = "cod_mun",
      gname         = "g_cs",
      xformla       = ~ d_semiarido + d_covid,
      control_group = "notyettreated",
      anticipation  = 0L,
      base_period   = "universal",
      est_method    = "reg",
      data          = painel_cs,
      panel         = TRUE
    ),
    error = function(e) {
      message("  ERRO [", y, "]: ", conditionMessage(e))
      NULL
    }
  )
})
names(cs_nyt) <- OUTCOMES

cs_nyt_simple <- purrr::map_dfr(OUTCOMES, function(y) {
  res <- cs_nyt[[y]]
  if (is.null(res)) return(tibble(outcome = y, att = NA, se = NA))
  ag <- did::aggte(res, type = "simple", na.rm = TRUE)
  tibble(outcome = y, att = ag$overall.att, se = ag$overall.se)
}) |>
  mutate(controle = "notyettreated")

readr::write_csv(cs_nyt_simple, file.path(DIR_TABLES, "tab_cs_notyettreated.csv"))
message("✓ CS notyettreated salvo")


# ============================================================================
# 3. PLACEBO: ANO DE TRATAMENTO ALEATÓRIO
# ============================================================================

message("\n=== Teste Placebo — ano de tratamento aleatório ===")

set.seed(42)
N_PLACEBO <- 200

# Municípios tratados e seus anos reais
tratados <- painel_cs |>
  filter(g_cs > 0) |>
  distinct(cod_mun, g_cs)

# Para cada iteração: permuta aleatoriamente os anos de tratamento entre tratados
placebo_atts <- purrr::map_dfr(seq_len(N_PLACEBO), function(i) {
  tratados_perm <- tratados |>
    mutate(g_cs_perm = sample(g_cs))

  painel_perm <- painel_cs |>
    left_join(tratados_perm |> select(cod_mun, g_cs_perm), by = "cod_mun") |>
    mutate(g_cs = if_else(!is.na(g_cs_perm), as.integer(g_cs_perm), 0L))

  res <- tryCatch(
    did::att_gt(
      yname         = Y_PRINCIPAL,
      tname         = "ano",
      idname        = "cod_mun",
      gname         = "g_cs",
      control_group = "nevertreated",
      est_method    = "reg",
      data          = painel_perm,
      panel         = TRUE,
      print_details = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(res)) return(tibble(sim = i, att_placebo = NA))
  ag <- tryCatch(
    did::aggte(res, type = "simple", na.rm = TRUE),
    error = function(e) NULL
  )
  if (is.null(ag)) return(tibble(sim = i, att_placebo = NA))
  tibble(sim = i, att_placebo = ag$overall.att)
})

# ATT real
att_real <- cs_agregado[[Y_PRINCIPAL]]$simple$overall.att

fig_placebo <- placebo_atts |>
  filter(!is.na(att_placebo)) |>
  ggplot(aes(x = att_placebo)) +
  geom_histogram(bins = 40, fill = "#aec7e8", colour = "white") +
  geom_vline(xintercept = att_real, colour = "#d62728",
             linetype = "solid", linewidth = 1.2) +
  annotate("text", x = att_real, y = Inf, vjust = 1.5, hjust = -0.1,
           label = paste0("ATT real = ", round(att_real, 3)),
           colour = "#d62728", size = 3.5) +
  labs(
    title    = "Distribuição Placebo do ATT",
    subtitle = paste0("200 permutações aleatórias | Variável: ",
                      OUTCOME_LABELS[Y_PRINCIPAL]),
    x        = "ATT placebo",
    y        = "Frequência",
    caption  = "Linha vermelha: ATT estimado com tratamento real."
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(DIR_FIGURES, "11_placebo_permutacao.png"),
  plot = fig_placebo, width = 9, height = 6, dpi = 300
)

p_valor_placebo <- mean(abs(placebo_atts$att_placebo) >= abs(att_real), na.rm = TRUE)
message(glue::glue(
  "✓ Placebo: p-valor = {round(p_valor_placebo, 3)} ",
  "(fração de simulações com |ATT placebo| ≥ |ATT real|)"
))


# ============================================================================
# 4. EXCLUINDO ADOTANTES PRECOCES (antes de 2008)
# ============================================================================

message("\n=== CS2021 excluindo adotantes precoces (<2008) ===")

painel_sem_precoces <- painel_cs |>
  filter(g_cs == 0 | g_cs >= 2008)

cs_sem_precoces <- lapply(OUTCOMES, function(y) {
  tryCatch(
    did::att_gt(
      yname = y, tname = "ano", idname = "cod_mun", gname = "g_cs",
      xformla = ~ d_semiarido + d_covid,
      control_group = "nevertreated", est_method = "reg",
      data = painel_sem_precoces, panel = TRUE
    ),
    error = function(e) NULL
  )
})
names(cs_sem_precoces) <- OUTCOMES

cs_sem_precoces_simple <- purrr::map_dfr(OUTCOMES, function(y) {
  res <- cs_sem_precoces[[y]]
  if (is.null(res)) return(tibble(outcome = y, att = NA, se = NA))
  ag <- tryCatch(did::aggte(res, type = "simple", na.rm = TRUE),
                 error = function(e) NULL)
  if (is.null(ag)) return(tibble(outcome = y, att = NA, se = NA))
  tibble(outcome = y, att = ag$overall.att, se = ag$overall.se)
}) |> mutate(spec = "sem_adotantes_precoces")

readr::write_csv(cs_sem_precoces_simple, file.path(DIR_TABLES, "tab_sem_precoces.csv"))
message("✓ CS sem adotantes precoces salvo")


# ============================================================================
# 5. EXCLUINDO ANOS COVID
# ============================================================================

message("\n=== CS2021 excluindo 2020-2021 ===")

painel_sem_covid <- painel_cs |>
  filter(!ano %in% c(2020L, 2021L))

cs_sem_covid <- lapply(OUTCOMES, function(y) {
  tryCatch(
    did::att_gt(
      yname = y, tname = "ano", idname = "cod_mun", gname = "g_cs",
      xformla = ~ d_semiarido, control_group = "nevertreated",
      est_method = "reg", data = painel_sem_covid, panel = TRUE
    ),
    error = function(e) NULL
  )
})
names(cs_sem_covid) <- OUTCOMES

cs_sem_covid_simple <- purrr::map_dfr(OUTCOMES, function(y) {
  res <- cs_sem_covid[[y]]
  if (is.null(res)) return(tibble(outcome = y, att = NA, se = NA))
  ag <- tryCatch(did::aggte(res, type = "simple", na.rm = TRUE),
                 error = function(e) NULL)
  if (is.null(ag)) return(tibble(outcome = y, att = NA, se = NA))
  tibble(outcome = y, att = ag$overall.att, se = ag$overall.se)
}) |> mutate(spec = "sem_covid")

readr::write_csv(cs_sem_covid_simple, file.path(DIR_TABLES, "tab_sem_covid.csv"))
message("✓ CS sem COVID salvo")


# ============================================================================
# 6. ANTECIPAÇÃO = 1
# ============================================================================

message("\n=== CS2021 com anticipation = 1 ===")

cs_antec <- lapply(OUTCOMES, function(y) {
  tryCatch(
    did::att_gt(
      yname = y, tname = "ano", idname = "cod_mun", gname = "g_cs",
      xformla = ~ d_semiarido + d_covid,
      control_group = "nevertreated", anticipation = 1L,
      est_method = "reg", data = painel_cs, panel = TRUE
    ),
    error = function(e) NULL
  )
})
names(cs_antec) <- OUTCOMES

cs_antec_simple <- purrr::map_dfr(OUTCOMES, function(y) {
  res <- cs_antec[[y]]
  if (is.null(res)) return(tibble(outcome = y, att = NA, se = NA))
  ag <- tryCatch(did::aggte(res, type = "simple", na.rm = TRUE),
                 error = function(e) NULL)
  if (is.null(ag)) return(tibble(outcome = y, att = NA, se = NA))
  tibble(outcome = y, att = ag$overall.att, se = ag$overall.se)
}) |> mutate(spec = "antecipacao_1")

readr::write_csv(cs_antec_simple, file.path(DIR_TABLES, "tab_antecipacao.csv"))
message("✓ CS com antecipação=1 salvo")


# ============================================================================
# 7. HONEST DiD — ANÁLISE DE SENSIBILIDADE (Rambachan & Roth 2023)
# ============================================================================

message("\n=== Honest DiD (Rambachan & Roth 2023) ===")

# HonestDiD usa a estrutura de erros padrão do event study CS2021
# para construir intervalos robustos a desvios lineares da tendência paralela

ag_dyn_principal <- cs_agregado[[Y_PRINCIPAL]]$dynamic

if (!is.null(ag_dyn_principal)) {
  tryCatch({
    # Extrair betahat (ATT dinâmico) e sigma (matriz de variância)
    betahat <- ag_dyn_principal$att.egt
    sigma   <- ag_dyn_principal$V.analytical.egt

    # Períodos pré e pós
    l_vec <- rep(0, length(betahat))
    # Peso unitário no período pós (simplificado)
    pos_idx <- which(ag_dyn_principal$egt >= 0)
    l_vec[pos_idx] <- 1 / length(pos_idx)

    # Sequência de Mbars (grau de desvio permitido da tendência paralela)
    Mbars <- seq(0, 0.5, by = 0.05)

    honest_res <- HonestDiD::createSensitivityResults(
      betahat   = betahat,
      sigma     = sigma,
      numPrePeriods  = sum(ag_dyn_principal$egt < 0),
      numPostPeriods = sum(ag_dyn_principal$egt >= 0),
      l_vec     = l_vec,
      Mbar      = Mbars,
      method    = "FLCI"  # Fixed-Length Confidence Interval
    )

    fig_honest <- HonestDiD::createSensitivityPlot(
      robustResults    = honest_res,
      originalResults  = HonestDiD::constructOriginalCS(
        betahat       = betahat,
        sigma         = sigma,
        numPrePeriods  = sum(ag_dyn_principal$egt < 0),
        numPostPeriods = sum(ag_dyn_principal$egt >= 0),
        l_vec         = l_vec
      )
    ) +
      labs(
        title   = "Honest DiD — Análise de Sensibilidade",
        subtitle = paste0("Variável: ", OUTCOME_LABELS[Y_PRINCIPAL]),
        caption = "Rambachan & Roth (2023). Mbar = grau de desvio linear permitido."
      ) +
      theme_minimal(base_size = 12)

    ggsave(
      file.path(DIR_FIGURES, "11_honestdid.png"),
      plot = fig_honest, width = 9, height = 6, dpi = 300
    )
    message("✓ Honest DiD salvo")
  }, error = function(e) {
    message("ERRO Honest DiD: ", conditionMessage(e))
  })
}


# ============================================================================
# 8. HETEROGENEIDADE: SEMI-ÁRIDO vs FORA
# ============================================================================

message("\n=== Heterogeneidade: semi-árido vs fora ===")

painel_sa_sub  <- painel_cs |> filter(d_semiarido == 1)
painel_fsa_sub <- painel_cs |> filter(d_semiarido == 0)

het_resultados <- lapply(c("semiarido", "fora_semiarido"), function(subg) {
  df <- if (subg == "semiarido") painel_sa_sub else painel_fsa_sub
  lapply(OUTCOMES, function(y) {
    tryCatch(
      did::att_gt(
        yname = y, tname = "ano", idname = "cod_mun", gname = "g_cs",
        xformla = ~ d_covid, control_group = "nevertreated",
        est_method = "reg", data = df, panel = TRUE
      ),
      error = function(e) NULL
    )
  }) |> setNames(OUTCOMES)
}) |> setNames(c("semiarido", "fora_semiarido"))

het_simple <- purrr::map_dfr(c("semiarido", "fora_semiarido"), function(subg) {
  purrr::map_dfr(OUTCOMES, function(y) {
    res <- het_resultados[[subg]][[y]]
    if (is.null(res)) return(tibble(subgrupo = subg, outcome = y, att = NA, se = NA))
    ag <- tryCatch(did::aggte(res, type = "simple", na.rm = TRUE),
                   error = function(e) NULL)
    if (is.null(ag)) return(tibble(subgrupo = subg, outcome = y, att = NA, se = NA))
    tibble(subgrupo = subg, outcome = y, att = ag$overall.att, se = ag$overall.se)
  })
})

readr::write_csv(het_simple, file.path(DIR_TABLES, "tab_heterogeneidade_semiarido.csv"))
message("✓ Heterogeneidade semi-árido salva")


# ============================================================================
# 9. INTENSIDADE: CAPACIDADE INSTALADA (log MW)
# ============================================================================

message("\n=== Especificação por intensidade (log_cap_mw) ===")

# Substitui o indicador binário d_eolica_dum pela capacidade em log
intens_resultados <- lapply(OUTCOMES, function(y) {
  formula_int <- as.formula(
    paste0(y, " ~ log_cap_mw + d_semiarido + d_covid | cod_mun + ano")
  )
  tryCatch(
    fixest::feols(fml = formula_int, data = painel, cluster = ~cod_mun),
    error = function(e) NULL
  )
})
names(intens_resultados) <- OUTCOMES

fixest::etable(
  intens_resultados,
  headers = OUTCOME_LABELS[OUTCOMES],
  tex     = TRUE,
  file    = file.path(DIR_TABLES, "tab_intensidade.tex"),
  title   = "Especificação por Intensidade — Log(Capacidade Instalada MW + 1)",
  notes   = "Erros padrão clusterizados no município."
)
message("✓ Tabela intensidade salva")


# ============================================================================
# 10. RESULTADO PLACEBO (não afetável): PIB per capita
# ============================================================================

message("\n=== Placebo de resultado: PIB per capita ===")

# Se eólicas afetassem o PIB antes de serem instaladas → problema de seleção
painel_placebo <- painel_cs |> filter(!is.na(x_pib_pc))

cs_placebo_pib <- tryCatch(
  did::att_gt(
    yname = "log_pib_pc", tname = "ano", idname = "cod_mun", gname = "g_cs",
    xformla = ~ d_semiarido + d_covid,
    control_group = "nevertreated", est_method = "reg",
    data = painel_placebo, panel = TRUE
  ),
  error = function(e) {
    message("ERRO placebo PIB: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(cs_placebo_pib)) {
  ag_pib <- did::aggte(cs_placebo_pib, type = "dynamic", na.rm = TRUE)
  p_pib <- ggdid::ggdid(ag_pib,
    title    = "Teste Placebo: ln(PIB per capita)",
    subtitle = "Não deve mostrar efeito no pré-período",
    xlab     = "Anos desde a primeira eólica",
    ylab     = "ATT"
  ) +
    geom_vline(xintercept = -0.5, linetype = "dashed") +
    theme_minimal(base_size = 12)

  ggsave(
    file.path(DIR_FIGURES, "11_placebo_pib.png"),
    plot = p_pib, width = 9, height = 6, dpi = 300
  )
}
message("✓ Placebo PIB salvo")


# ============================================================================
# RESUMO CONSOLIDADO DAS ROBUSTEZAS
# ============================================================================

# Tabela comparativa de todas as especificações
cs_main_simple <- purrr::map_dfr(OUTCOMES, function(y) {
  ag <- cs_agregado[[y]]$simple
  if (is.null(ag)) return(tibble(outcome = y, att = NA, se = NA))
  tibble(outcome = y, att = ag$overall.att, se = ag$overall.se)
}) |> mutate(spec = "principal_nevertreated")

tab_robustez_consolidado <- bind_rows(
  cs_main_simple,
  cs_nyt_simple,
  cs_sem_precoces_simple,
  cs_sem_covid_simple,
  cs_antec_simple
) |>
  mutate(
    ci_low  = att - 1.96 * se,
    ci_high = att + 1.96 * se,
    label   = OUTCOME_LABELS[outcome]
  )

readr::write_csv(
  tab_robustez_consolidado,
  file.path(DIR_TABLES, "tab_robustez_consolidado.csv")
)

# Figura consolidada
fig_robustez <- tab_robustez_consolidado |>
  filter(!is.na(att)) |>
  mutate(spec = factor(spec, levels = c(
    "principal_nevertreated", "notyettreated", "sem_adotantes_precoces",
    "sem_covid", "antecipacao_1"
  ), labels = c(
    "Principal (never-treated)", "Not-yet-treated", "Sem adotantes precoces",
    "Sem 2020-2021 (COVID)", "Antecipação = 1"
  ))) |>
  ggplot(aes(x = att, y = spec, colour = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.7) +
  facet_wrap(~ label, scales = "free_x", ncol = 2) +
  scale_colour_brewer(palette = "Dark2", guide = "none") +
  labs(
    title   = "Análise de Robustez — ATT Médio por Especificação",
    x       = "ATT estimado (IC 95%)",
    y       = NULL,
    caption = "Estimador: Callaway & Sant'Anna (2021)."
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave(
  file.path(DIR_FIGURES, "11_robustez_consolidado.png"),
  plot = fig_robustez, width = 14, height = 9, dpi = 300
)

message("✓ Figura robustez consolidada salva")
message("\n11_robustez.R concluído.")
