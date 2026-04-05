# =============================================================================
# 09_descritivas.R
# Estatísticas descritivas, mapas e inspeção visual de tendências paralelas
#
# Saídas:
#   output/tables/tab_descritivas.tex / .html
#   output/tables/tab_coortes.tex
#   output/figures/fig_tendencias_pre.png
#   output/figures/fig_outcomes_serie.png
#   output/maps/mapa_tratados_controles.png
#   output/maps/mapa_coortes.png
# =============================================================================

source(here::here("R", "00_setup.R"))

painel  <- readRDS(file.path(DIR_FINAL, "painel_final.rds"))
mun_sf  <- carregar("municipios_nordeste")


# ============================================================================
# 1. TABELA DE ESTATÍSTICAS DESCRITIVAS (Tabela 1)
# ============================================================================

message("Gerando estatísticas descritivas...")

# Variáveis para a tabela
vars_desc <- painel |>
  mutate(
    grupo = factor(
      if_else(never_treated == 1, "Nunca tratado", "Tratado (algum ano)"),
      levels = c("Tratado (algum ano)", "Nunca tratado")
    )
  ) |>
  select(
    grupo,
    # Resultados (médias anuais por município)
    pct_nasc_adol,
    tx_intern_aborto,
    tx_obito_rn,
    tx_sem_pai,
    # Emprego
    x_vinculos_pc,
    # Controles
    x_pop,
    x_pib_pc_mil,
    x_idhm,
    x_gini,
    x_dist_capital_km,
    d_semiarido
  )

# Labels legíveis
attr(vars_desc$pct_nasc_adol,     "label") <- "Gravidez adolescente (% nasc. vivos)"
attr(vars_desc$tx_intern_aborto,  "label") <- "Internações CID O00-O08 (por 1.000 NV)"
attr(vars_desc$tx_obito_rn,       "label") <- "Óbitos neonatais (por 1.000 NV)"
attr(vars_desc$tx_sem_pai,        "label") <- "Nascimentos sem pai declarado (%)"
attr(vars_desc$x_vinculos_pc,     "label") <- "Vínculos RAIS (por 1.000 hab.)"
attr(vars_desc$x_pop,             "label") <- "População total"
attr(vars_desc$x_pib_pc_mil,      "label") <- "PIB per capita (R$ mil)"
attr(vars_desc$x_idhm,            "label") <- "IDH-M"
attr(vars_desc$x_gini,            "label") <- "Índice de Gini"
attr(vars_desc$x_dist_capital_km, "label") <- "Distância à capital (km)"
attr(vars_desc$d_semiarido,       "label") <- "Semi-árido SUDENE (fração)"

# Tabela com modelsummary
tab_desc <- modelsummary::datasummary_balance(
  formula = ~ grupo,
  data    = vars_desc,
  output  = "data.frame",
  fmt     = "%.3f"
)

modelsummary::datasummary_balance(
  formula = ~ grupo,
  data    = vars_desc,
  output  = file.path(DIR_TABLES, "tab_descritivas.tex"),
  title   = "Estatísticas Descritivas — Municípios do Nordeste",
  notes   = "Fontes: IBGE/geobr, ANEEL/SIGA, DataSUS (SIM, SINASC, SIH), Portal do Registro Civil, RAIS, IPEA."
)

modelsummary::datasummary_balance(
  formula = ~ grupo,
  data    = vars_desc,
  output  = file.path(DIR_TABLES, "tab_descritivas.html"),
  title   = "Estatísticas Descritivas — Municípios do Nordeste"
)

message("✓ Tabela descritiva salva")


# ============================================================================
# 2. TABELA DE COORTES DE TRATAMENTO
# ============================================================================

tab_coortes <- painel |>
  filter(ano == ANO_INICIO) |>
  mutate(coorte = case_when(
    never_treated == 1 ~ "Nunca tratado",
    g_cs <= 2005        ~ "2001–2005",
    g_cs <= 2010        ~ "2006–2010",
    g_cs <= 2015        ~ "2011–2015",
    g_cs <= 2020        ~ "2016–2020",
    TRUE                ~ "2021–2022"
  )) |>
  group_by(coorte) |>
  summarise(
    n_municipios    = n(),
    media_pop       = mean(x_pop, na.rm = TRUE),
    media_pct_adol  = mean(pct_nasc_adol, na.rm = TRUE),
    media_pib_pc    = mean(x_pib_pc_mil, na.rm = TRUE),
    pct_semiarido   = mean(d_semiarido,  na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  arrange(coorte)

readr::write_csv(tab_coortes, file.path(DIR_TABLES, "tab_coortes.csv"))
message("✓ Tabela de coortes salva")


# ============================================================================
# 3. INSPEÇÃO VISUAL DE TENDÊNCIAS PARALELAS (PRÉ-TRATAMENTO)
# ============================================================================

message("Gerando figura de tendências pré-tratamento...")

# Para cada variável de resultado, plotar médias anuais por grupo (tratado vs controle)
# nos anos anteriores ao tratamento. Se as linhas forem paralelas, a hipótese é plausível.

resultados_labels <- c(
  pct_nasc_adol    = "Gravidez adolescente (%)",
  tx_intern_aborto = "Internações aborto (por 1.000 NV)",
  tx_obito_rn      = "Óbitos neonatais (por 1.000 NV)",
  tx_sem_pai       = "Nasc. sem pai declarado (%)"
)

# Restrição: apenas anos pré-tratamento para cada município
pre_data <- painel |>
  filter(
    is.na(rel_time) | rel_time < 0,  # antes do tratamento ou nunca tratado
    ano >= 2003                        # 3 anos de burn-in
  ) |>
  mutate(grupo = if_else(never_treated == 1, "Nunca tratado", "Tratado (pré-tratamento)"))

fig_pre <- pre_data |>
  select(ano, grupo, all_of(names(resultados_labels))) |>
  pivot_longer(
    cols      = all_of(names(resultados_labels)),
    names_to  = "variavel",
    values_to = "valor"
  ) |>
  mutate(variavel = recode(variavel, !!!resultados_labels)) |>
  group_by(ano, grupo, variavel) |>
  summarise(media = mean(valor, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = ano, y = media, colour = grupo, group = grupo)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 2) +
  scale_colour_manual(
    values = c("Nunca tratado" = "#2c7bb6", "Tratado (pré-tratamento)" = "#d7191c"),
    name   = NULL
  ) +
  labs(
    title    = "Inspeção de Tendências Pré-Tratamento",
    subtitle = "Médias anuais por grupo — municípios do Nordeste",
    x        = "Ano",
    y        = "Média",
    caption  = "Nota: incluídos apenas anos pré-tratamento para municípios tratados."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

ggsave(
  file.path(DIR_FIGURES, "09_tendencias_pre.png"),
  plot = fig_pre, width = 12, height = 8, dpi = 300
)
message("✓ Figura tendências pré salva")


# ============================================================================
# 4. SÉRIES TEMPORAIS DOS RESULTADOS (por UF)
# ============================================================================

message("Gerando séries temporais por UF...")

fig_serie <- painel |>
  group_by(ano, sigla_uf) |>
  summarise(
    pct_adol = mean(pct_nasc_adol,   na.rm = TRUE),
    tx_abor  = mean(tx_intern_aborto, na.rm = TRUE),
    tx_rn    = mean(tx_obito_rn,      na.rm = TRUE),
    .groups  = "drop"
  ) |>
  pivot_longer(c(pct_adol, tx_abor, tx_rn),
               names_to = "indicador", values_to = "valor") |>
  mutate(indicador = recode(
    indicador,
    pct_adol = "Gravidez adolescente (%)",
    tx_abor  = "Internações aborto (por 1.000 NV)",
    tx_rn    = "Óbitos neonatais (por 1.000 NV)"
  )) |>
  ggplot(aes(x = ano, y = valor, colour = sigla_uf, group = sigla_uf)) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  facet_wrap(~ indicador, scales = "free_y") +
  scale_colour_brewer(palette = "Set1", name = "UF") +
  labs(
    title   = "Evolução dos Indicadores — Nordeste por UF",
    x       = "Ano", y = "Valor médio",
    caption = "Fonte: DataSUS (SIM, SINASC, SIH)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(DIR_FIGURES, "09_series_uf.png"),
  plot = fig_serie, width = 14, height = 6, dpi = 300
)
message("✓ Séries por UF salvas")


# ============================================================================
# 5. MAPAS: TRATADOS vs CONTROLES E COORTES
# ============================================================================

message("Gerando mapas...")

painel_2022 <- painel |>
  filter(ano == 2022) |>
  left_join(mun_sf |> select(cod_mun, geom), by = "cod_mun") |>
  sf::st_as_sf()

# 5a. Mapa: tratados vs nunca tratados
mapa_trat <- painel_2022 |>
  mutate(grupo = case_when(
    never_treated == 1                      ~ "Nunca tratado",
    ano_primeiro_eolica <= 2010             ~ "Tratado até 2010",
    ano_primeiro_eolica <= 2015             ~ "Tratado 2011-2015",
    ano_primeiro_eolica <= 2022             ~ "Tratado 2016-2022"
  )) |>
  ggplot() +
  geom_sf(aes(fill = grupo), colour = "white", linewidth = 0.05) +
  scale_fill_manual(
    values = c(
      "Nunca tratado"     = "#f5f5f5",
      "Tratado até 2010"  = "#08519c",
      "Tratado 2011-2015" = "#2171b5",
      "Tratado 2016-2022" = "#6baed6"
    ),
    name = "Grupo de coorte",
    na.value = "#f5f5f5"
  ) +
  labs(
    title   = "Municípios por Coorte de Tratamento Eólico",
    subtitle = "Nordeste Brasileiro",
    caption = "Fonte: ANEEL SIGA | IBGE/geobr"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

ggsave(
  file.path(DIR_MAPS, "09_mapa_coortes.png"),
  plot = mapa_trat, width = 10, height = 9, dpi = 300
)

# 5b. Mapa: capacidade instalada + semi-árido contorno
semiarido_sf <- geobr::read_semiarid(year = 2017, showProgress = FALSE)

mapa_cap <- painel_2022 |>
  ggplot() +
  geom_sf(aes(fill = cap_instalada_mw), colour = "white", linewidth = 0.05) +
  geom_sf(data = semiarido_sf, fill = NA, colour = "#e67e22",
          linewidth = 0.6, linetype = "dashed") +
  scale_fill_viridis_c(
    option = "plasma", trans = "log1p", name = "Capacidade (MW)",
    breaks = c(0, 50, 200, 1000, 5000),
    labels = scales::comma
  ) +
  labs(
    title   = "Capacidade Instalada Eólica — 2022",
    subtitle = "Contorno laranja: delimitação do semi-árido SUDENE",
    caption = "Fontes: ANEEL SIGA | SUDENE | IBGE/geobr"
  ) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 13))

ggsave(
  file.path(DIR_MAPS, "09_mapa_capacidade_semiarido.png"),
  plot = mapa_cap, width = 10, height = 9, dpi = 300
)

message("✓ Mapas salvos")
message("\n09_descritivas.R concluído.")
