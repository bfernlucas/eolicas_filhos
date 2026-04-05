# =============================================================================
# 10_staggered_did.R
# Estimação principal — DiD com adoção escalonada
#
# Estimadores implementados:
#   1. TWFE clássico (fixest::feols)           — baseline / comparação
#   2. Callaway & Sant'Anna 2021 (did::att_gt) — estimador preferido
#   3. Sun & Abraham 2021 (fixest::feols+sunab) — robustez
#   4. Borusyak, Jaravel & Spiess 2024 (didimputation) — robustez
#
# Para cada estimador, as variáveis de resultado estimadas são:
#   y1: pct_nasc_adol       — gravidez adolescente (%)
#   y2: tx_intern_aborto    — internações CID O00-O08 (por 1.000 NV)
#   y3: tx_obito_rn         — óbitos neonatais (por 1.000 NV)
#   y4: tx_sem_pai          — nascimentos sem pai declarado (%)
#   y5: log_vinculos_energia — ln(vínculos em energia elétrica + 1)
#
# Saídas:
#   output/tables/tab_twfe.tex
#   output/tables/tab_cs2021.tex
#   output/tables/tab_sa2021.tex
#   output/figures/fig_event_study_cs.png
#   output/figures/fig_event_study_sa.png
# =============================================================================

source(here::here("R", "00_setup.R"))

painel <- readRDS(file.path(DIR_FINAL, "painel_final.rds"))

# Resultados a estimar
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
  tx_sem_pai           = "Nascimentos sem pai (% do total)",
  log_vinculos_energia = "ln(Vínculos em energia + 1)"
)

# Fórmula de controles pré-determinados (características de 2000 × tendência)
# Usamos interação de controles fixos com tendência linear (como em CS2021)
XFORMLA <- ~ d_semiarido + d_covid


# ============================================================================
# 1. TWFE CLÁSSICO
# ============================================================================

message("\n=== TWFE Clássico ===")

twfe_resultados <- lapply(OUTCOMES, function(y) {
  formula_twfe <- as.formula(
    paste0(y, " ~ d_post + d_semiarido + d_covid | cod_mun + ano")
  )

  fixest::feols(
    fml     = formula_twfe,
    data    = painel,
    cluster = ~cod_mun,
    notes   = FALSE
  )
})
names(twfe_resultados) <- OUTCOMES

# Tabela TWFE
fixest::etable(
  twfe_resultados,
  headers  = OUTCOME_LABELS[OUTCOMES],
  tex      = TRUE,
  file     = file.path(DIR_TABLES, "tab_twfe.tex"),
  title    = "TWFE Clássico — Efeito de Parques Eólicos",
  notes    = "Erros padrão clusterizados no município. FE: município e ano. Controles: semi-árido, COVID.",
  style.df = fixest::style.df("markdown")
)
message("✓ Tabela TWFE salva")


# ============================================================================
# 2. CALLAWAY & SANT'ANNA (2021)
# ============================================================================

message("\n=== Callaway & Sant'Anna (2021) ===")

# Preparar dados: g_cs = 0 para nunca tratados (requerimento do pacote did)
painel_cs <- painel |>
  mutate(g_cs = if_else(is.na(ano_primeiro_eolica), 0L, as.integer(ano_primeiro_eolica)))

cs_resultados <- lapply(OUTCOMES, function(y) {
  message("  CS2021 — ", y)

  # Especificação principal: controle = nunca tratados
  res <- tryCatch(
    did::att_gt(
      yname          = y,
      tname          = "ano",
      idname         = "cod_mun",
      gname          = "g_cs",
      xformla        = XFORMLA,
      control_group  = "nevertreated",
      anticipation   = 0L,
      base_period    = "universal",
      est_method     = "reg",      # estimação por OLS (mais rápido); "ipw" para IPW
      data           = painel_cs,
      panel          = TRUE,
      allow_unbalanced_panel = FALSE
    ),
    error = function(e) {
      message("  ERRO CS2021 [", y, "]: ", conditionMessage(e))
      NULL
    }
  )
  res
})
names(cs_resultados) <- OUTCOMES

# Agregações para cada resultado
cs_agregado <- lapply(cs_resultados, function(res) {
  if (is.null(res)) return(list(simple = NULL, dynamic = NULL, group = NULL))
  list(
    simple  = did::aggte(res, type = "simple",  na.rm = TRUE),
    dynamic = did::aggte(res, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8),
    group   = did::aggte(res, type = "group",   na.rm = TRUE)
  )
})
names(cs_agregado) <- OUTCOMES

# Salvar objetos
saveRDS(cs_resultados, file.path(DIR_TABLES, "cs2021_att_gt.rds"))
saveRDS(cs_agregado,   file.path(DIR_TABLES, "cs2021_agregado.rds"))

# Tabela ATT simples (efeito médio)
cs_simple_tab <- purrr::map_dfr(OUTCOMES, function(y) {
  ag <- cs_agregado[[y]]$simple
  if (is.null(ag)) return(tibble(outcome = y, att = NA, se = NA, p = NA))
  tibble(
    outcome = y,
    att     = ag$overall.att,
    se      = ag$overall.se,
    p       = 2 * pnorm(-abs(ag$overall.att / ag$overall.se))
  )
}) |>
  mutate(
    ci_low  = att - 1.96 * se,
    ci_high = att + 1.96 * se,
    sig     = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""),
    label   = OUTCOME_LABELS[outcome]
  )

readr::write_csv(cs_simple_tab, file.path(DIR_TABLES, "tab_cs2021_simple.csv"))
message("✓ ATT simples CS2021 salvo")


# ============================================================================
# 3. SUN & ABRAHAM (2021) via fixest
# ============================================================================

message("\n=== Sun & Abraham (2021) ===")

# Para Sun-Abraham: nunca-tratados devem ter g_cs = NA (ou Inf)
painel_sa <- painel |>
  mutate(g_sa = if_else(is.na(ano_primeiro_eolica), NA_integer_,
                        as.integer(ano_primeiro_eolica)))

sa_resultados <- lapply(OUTCOMES, function(y) {
  formula_sa <- as.formula(paste0(
    y, " ~ sunab(g_sa, ano) + d_semiarido + d_covid | cod_mun + ano"
  ))
  message("  SA2021 — ", y)
  tryCatch(
    fixest::feols(
      fml     = formula_sa,
      data    = painel_sa,
      cluster = ~cod_mun,
      notes   = FALSE
    ),
    error = function(e) {
      message("  ERRO SA2021 [", y, "]: ", conditionMessage(e))
      NULL
    }
  )
})
names(sa_resultados) <- OUTCOMES

# Tabela SA2021 (ATT médio por período)
sa_tab <- purrr::map_dfr(OUTCOMES, function(y) {
  res <- sa_resultados[[y]]
  if (is.null(res)) return(NULL)
  # Agregar usando fixest (média ponderada sobre coortes e períodos)
  ag  <- fixest::aggregate(res, "(cohort|period)::(.*)")
  tibble(
    outcome = y,
    att     = coef(ag)["ATT"],
    se      = sqrt(diag(vcov(ag)))["ATT"]
  )
})

readr::write_csv(sa_tab, file.path(DIR_TABLES, "tab_sa2021.csv"))
fixest::etable(
  sa_resultados,
  headers  = OUTCOME_LABELS[OUTCOMES],
  tex      = TRUE,
  file     = file.path(DIR_TABLES, "tab_sa2021.tex"),
  title    = "Sun \\& Abraham (2021) — Efeito de Parques Eólicos",
  notes    = "Erros padrão clusterizados no município. FE: município e ano.",
  keep     = "ATT"
)
message("✓ Tabela SA2021 salva")


# ============================================================================
# 4. BORUSYAK, JARAVEL & SPIESS (2024) — imputation estimator
# ============================================================================

message("\n=== Borusyak, Jaravel & Spiess (2024) ===")

bjs_resultados <- lapply(OUTCOMES, function(y) {
  message("  BJS — ", y)
  tryCatch(
    didimputation::did_imputation(
      data         = painel,
      yname        = y,
      gname        = "ano_primeiro_eolica",
      tname        = "ano",
      idname       = "cod_mun",
      horizon      = TRUE,
      pretrends    = 4L,
      cluster_var  = "cod_mun"
    ),
    error = function(e) {
      message("  ERRO BJS [", y, "]: ", conditionMessage(e))
      NULL
    }
  )
})
names(bjs_resultados) <- OUTCOMES

saveRDS(bjs_resultados, file.path(DIR_TABLES, "bjs2024.rds"))
message("✓ BJS2024 salvo")


# ============================================================================
# 5. EVENT STUDY PLOTS
# ============================================================================

message("\n=== Gerando event study plots ===")

# ---- 5a. Callaway & Sant'Anna (ggdid) -------------------------------------

for (y in OUTCOMES) {
  ag_dyn <- cs_agregado[[y]]$dynamic
  if (is.null(ag_dyn)) next

  # Usando ggdid::ggdid para plotar o agregado dinâmico
  p <- ggdid::ggdid(
    ag_dyn,
    title    = paste0("Event Study — ", OUTCOME_LABELS[y]),
    xlab     = "Anos desde a primeira eólica",
    ylab     = "ATT estimado",
    grtitle  = "Coorte"
  ) +
    geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey50") +
    theme_minimal(base_size = 12)

  ggsave(
    file.path(DIR_FIGURES, paste0("10_event_cs_", y, ".png")),
    plot = p, width = 10, height = 6, dpi = 300
  )
}

# ---- 5b. Painel multi-resultados (CS2021) ----------------------------------

# Construir data frame de todos os event studies (CS)
es_cs_data <- purrr::map_dfr(OUTCOMES, function(y) {
  ag_dyn <- cs_agregado[[y]]$dynamic
  if (is.null(ag_dyn)) return(NULL)
  tibble(
    rel_time = ag_dyn$egt,
    att      = ag_dyn$att.egt,
    se       = ag_dyn$se.egt,
    outcome  = OUTCOME_LABELS[y]
  )
}) |>
  mutate(
    ci_low  = att - 1.96 * se,
    ci_high = att + 1.96 * se
  )

fig_es_painel <- es_cs_data |>
  filter(!is.na(att), abs(rel_time) <= 8) |>
  ggplot(aes(x = rel_time, y = att)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey60") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#2196F3", alpha = 0.2) +
  geom_line(colour = "#2196F3", linewidth = 0.9) +
  geom_point(colour = "#2196F3", size = 2) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = -8:8) +
  labs(
    title    = "Event Study — Callaway & Sant'Anna (2021)",
    subtitle = "ATT dinâmico com IC 95% | Nordeste Brasileiro",
    x        = "Anos desde a primeira eólica no município",
    y        = "ATT estimado",
    caption  = "Controle: municípios nunca tratados. Controles: semi-árido, COVID."
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave(
  file.path(DIR_FIGURES, "10_event_study_cs_painel.png"),
  plot = fig_es_painel, width = 14, height = 10, dpi = 300
)

# ---- 5c. Comparação TWFE vs CS2021 (ATT simples) --------------------------

comp_tab <- bind_rows(
  purrr::map_dfr(OUTCOMES, function(y) {
    m <- twfe_resultados[[y]]
    tibble(
      outcome   = OUTCOME_LABELS[y],
      estimador = "TWFE",
      att       = coef(m)["d_post"],
      se        = sqrt(diag(vcov(m, type = "cluster")))["d_post"]
    )
  }),
  cs_simple_tab |>
    transmute(
      outcome   = OUTCOME_LABELS[outcome],
      estimador = "CS2021",
      att, se
    )
) |>
  mutate(
    ci_low  = att - 1.96 * se,
    ci_high = att + 1.96 * se
  )

fig_comp <- comp_tab |>
  filter(!is.na(att)) |>
  ggplot(aes(x = att, y = outcome, colour = estimador, shape = estimador)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high),
                  position = position_dodge(width = 0.4), linewidth = 0.8) +
  scale_colour_manual(values = c("TWFE" = "#e41a1c", "CS2021" = "#377eb8"),
                      name = "Estimador") +
  scale_shape_manual(values = c("TWFE" = 16, "CS2021" = 17), name = "Estimador") +
  labs(
    title   = "Comparação de Estimadores — ATT Médio",
    subtitle = "IC 95% | Nordeste Brasileiro",
    x = "ATT estimado", y = NULL,
    caption = "Erros padrão clusterizados no município."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  file.path(DIR_FIGURES, "10_comparacao_estimadores.png"),
  plot = fig_comp, width = 10, height = 7, dpi = 300
)

message("✓ Event study plots salvos")
message("\n10_staggered_did.R concluído.")
