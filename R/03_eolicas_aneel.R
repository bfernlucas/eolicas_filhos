# =============================================================================
# 03_eolicas_aneel.R
# Parques eólicos — ANEEL SIGA (Sistema de Informações de Geração da ANEEL)
#
# Download: dados.gov.br — "SIGA - Sistema de Informações de Geração da ANEEL"
# URL: https://dadosabertos.aneel.gov.br/dataset/siga-sistema-de-informacoes-de-geracao-da-aneel
#
# Variável de tratamento principal:
#   d_eolica_dum    : 1 se o município tinha ao menos 1 usina eólica operando
#   ano_primeiro_eolica : primeiro ano de operação comercial no município
#   cap_instalada_mw    : capacidade instalada acumulada no município-ano
#
# Saída:
#   data/processed/eolicas_panel.rds  — tibble município-ano
# =============================================================================

source(here::here("R", "00_setup.R"))

mun_tab <- carregar("municipios_nordeste_tab")


# ---- 1. Download do SIGA ---------------------------------------------------

siga_url <- paste0(
  "https://dadosabertos.aneel.gov.br/dataset/",
  "1213b4c3-8a02-4dc5-9b0e-25dff7b6d63e/resource/",
  "b1bd71e7-d0ad-4214-9053-cbd58e9564a7/download/siga-empreendimentos-geracao.csv"
)

siga_raw_path <- file.path(DIR_RAW, "aneel", "siga_empreendimentos.csv")

if (!file.exists(siga_raw_path)) {
  message("Baixando SIGA da ANEEL...")
  download.file(siga_url, destfile = siga_raw_path, mode = "wb")
} else {
  message("SIGA já baixado: ", siga_raw_path)
}


# ---- 2. Leitura e limpeza -------------------------------------------------

siga_raw <- read_delim(
  siga_raw_path,
  delim     = ";",
  locale    = locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = "."),
  col_types = cols(.default = "c"),
  skip      = 0
) |>
  janitor::clean_names()

# Inspecionar colunas disponíveis
message("Colunas SIGA: ", paste(names(siga_raw), collapse = ", "))


# ---- 3. Selecionar e padronizar variáveis ---------------------------------

# Nomes das colunas podem variar entre versões do SIGA; mapeamos os principais
siga_clean <- siga_raw |>
  rename_with(~ case_match(
    .x,
    "nom_municipio"                        ~ "nome_mun_aneel",
    "sig_uf"                               ~ "sigla_uf",
    "cod_municipio_ibge"                   ~ "cod_mun_str",
    "dsc_tipo_geracao"                     ~ "tipo_geracao",
    "dsc_fase_usina"                       ~ "fase_usina",
    "dat_inicio_operacao"                  ~ "dt_operacao",
    "mdc_pot_fiskalizada_kw"               ~ "pot_kw",
    "mdc_pot_instalada_kw"                 ~ "pot_instalada_kw",
    "nom_usina"                            ~ "nome_usina",
    .default                               = .x
  )) |>
  filter(
    tipo_geracao %in% c("Eólica", "EÓLICA", "EOL", "eolica", "eólica")
  ) |>
  mutate(
    # Código IBGE do município (7 dígitos)
    cod_mun = as.integer(stringr::str_sub(cod_mun_str, 1, 7)),

    # Data de início de operação
    dt_operacao = dmy(dt_operacao),
    ano_operacao = year(dt_operacao),

    # Capacidade em MW
    pot_mw = suppressWarnings(as.numeric(pot_instalada_kw)) / 1000
  )


# ---- 4. Filtrar apenas o Nordeste -----------------------------------------

eolicas_ne <- siga_clean |>
  filter(cod_mun %in% mun_tab$cod_mun | sigla_uf %in% SIGLAS_NE)

message(glue::glue(
  "Usinas eólicas no Nordeste: {nrow(eolicas_ne)} | ",
  "Municípios com eólicas: {n_distinct(eolicas_ne$cod_mun)}"
))


# ---- 5. Apenas usinas em operação comercial --------------------------------

eolicas_operando <- eolicas_ne |>
  filter(
    fase_usina %in% c("Operação", "OPERAÇÃO", "Operacao", "operação", "OperaÃ§Ã£o"),
    !is.na(ano_operacao),
    ano_operacao >= ANO_INICIO,
    ano_operacao <= ANO_FIM
  )

message(glue::glue(
  "Usinas em operação (período {ANO_INICIO}-{ANO_FIM}): {nrow(eolicas_operando)}"
))


# ---- 6. Primeiro ano de operação por município ----------------------------

primeiro_eolica <- eolicas_operando |>
  group_by(cod_mun) |>
  summarise(
    ano_primeiro_eolica = min(ano_operacao, na.rm = TRUE),
    n_usinas_total       = n_distinct(nome_usina),
    .groups              = "drop"
  )


# ---- 7. Capacidade instalada acumulada município-ano ----------------------

# Expandir para painel: para cada ano, acumular capacidade de usinas já em op.
eolicas_ano <- eolicas_operando |>
  select(cod_mun, ano_operacao, pot_mw) |>
  # Para cada usina, ela contribui com capacidade a partir do ano de operação
  tidyr::crossing(ano = ANOS) |>
  filter(ano >= ano_operacao) |>
  group_by(cod_mun, ano) |>
  summarise(
    n_usinas_operando = n(),
    cap_instalada_mw  = sum(pot_mw, na.rm = TRUE),
    .groups           = "drop"
  )


# ---- 8. Painel município-ano completo -------------------------------------

painel_eolicas <- mun_tab |>
  select(cod_mun, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS) |>
  left_join(primeiro_eolica, by = "cod_mun") |>
  left_join(eolicas_ano, by = c("cod_mun", "ano")) |>
  mutate(
    # Indicadores de tratamento
    d_eolica_dum    = as.integer(!is.na(ano_primeiro_eolica) & ano >= ano_primeiro_eolica),
    n_usinas_operando = replace_na(n_usinas_operando, 0L),
    cap_instalada_mw  = replace_na(cap_instalada_mw, 0),

    # Variável de "grupo de coorte" para estimadores de DiD
    # (ano em que o município foi primeiro tratado; Inf = nunca tratado)
    g_eolica = if_else(is.na(ano_primeiro_eolica), Inf, as.numeric(ano_primeiro_eolica)),

    # Log da capacidade (para especificações de intensidade)
    log_cap_mw = log1p(cap_instalada_mw)
  )


# ---- 9. Estatísticas descritivas ------------------------------------------

# Distribuição de municípios por ano de primeiro tratamento
dist_coortes <- primeiro_eolica |>
  count(ano_primeiro_eolica, name = "n_municipios") |>
  arrange(ano_primeiro_eolica)

message("\nDistribuição de coortes de tratamento (primeiro ano eólica):")
print(dist_coortes)

message(glue::glue(
  "\nTotal de municípios tratados: {nrow(primeiro_eolica)} / {nrow(mun_tab)}"
))

# Série temporal: capacidade instalada total no NE
serie_cap <- painel_eolicas |>
  group_by(ano) |>
  summarise(cap_total_gw = sum(cap_instalada_mw, na.rm = TRUE) / 1000,
            n_mun_tratados = sum(d_eolica_dum), .groups = "drop")

print(serie_cap)


# ---- 10. Salvar -------------------------------------------------------------

salvar(eolicas_ne,       "eolicas_ne_raw")
salvar(primeiro_eolica,  "eolicas_primeiro_ano")
salvar(painel_eolicas,   "eolicas_panel")


# ---- 11. Figura: evento de adoção escalonada --------------------------------

fig_coortes <- dist_coortes |>
  filter(is.finite(ano_primeiro_eolica)) |>
  ggplot(aes(x = ano_primeiro_eolica, y = n_municipios)) +
  geom_col(fill = "#2196F3", colour = "white") +
  geom_text(aes(label = n_municipios), vjust = -0.4, size = 3.5) +
  scale_x_continuous(breaks = dist_coortes$ano_primeiro_eolica) +
  labs(
    title    = "Adoção Escalonada: Municípios com Primeira Eólica por Ano",
    subtitle = "Nordeste brasileiro — Fonte: ANEEL SIGA",
    x        = "Ano de início de operação",
    y        = "Nº de municípios"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(DIR_FIGURES, "03_coortes_eolicas.png"),
  plot = fig_coortes, width = 10, height = 6, dpi = 300
)


# ---- 12. Mapa: municípios com eólicas (2022) --------------------------------
mun_sf <- carregar("municipios_nordeste")

mapa_eolicas <- painel_eolicas |>
  filter(ano == 2022) |>
  left_join(mun_sf |> select(cod_mun, geom), by = "cod_mun") |>
  sf::st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = cap_instalada_mw), colour = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    option = "plasma", trans = "log1p",
    name   = "Capacidade\ninstalada (MW)",
    breaks = c(0, 10, 100, 500, 2000),
    labels = c("0", "10", "100", "500", "2.000")
  ) +
  labs(
    title    = "Capacidade Instalada Eólica por Município — 2022",
    subtitle = "Fonte: ANEEL SIGA | IBGE/geobr",
    caption  = "Escala logarítmica"
  ) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 13))

ggsave(
  file.path(DIR_MAPS, "03_mapa_eolicas_2022.png"),
  plot = mapa_eolicas, width = 10, height = 8, dpi = 300
)

message("\n03_eolicas_aneel.R concluído.")
