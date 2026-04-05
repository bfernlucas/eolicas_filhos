# =============================================================================
# 06_rais.R
# Vínculos empregatícios — RAIS (Relação Anual de Informações Sociais)
#
# Fonte: Base dos Dados (basedosdados.org) via Google BigQuery
#   Dataset: `basedosdados.br_me_rais.microdados_vinculos`
#   Também disponível em: ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/
#
# Variáveis de interesse:
#   → Total de vínculos ativos por município-ano (proxy mercado de trabalho)
#   → Vínculos em CNAEs ligados a energia (3511, 3512, 3513) — setor eólico
#   → Salário médio
#   → Proporção de mulheres empregadas
#   → Vínculos na construção civil (proxy chegada de obras de infraestrutura)
#
# Saída:
#   data/processed/rais_panel.rds
# =============================================================================

source(here::here("R", "00_setup.R"))

mun_tab <- carregar("municipios_nordeste_tab")


# ---- 1. Conexão com Base dos Dados (BigQuery) -----------------------------
# Requer autenticação:  basedosdados::bd_auth()
# Projeto BigQuery:     basedosdados

# Para configurar:
#   1. Crie uma conta em https://cloud.google.com/
#   2. Execute: basedosdados::bd_auth()
#   3. Defina o projeto: options(basedosdados_project_id = "SEU_PROJETO")

rais_cache <- file.path(DIR_RAW, "rais", "rais_nordeste_aggregated.rds")

if (!file.exists(rais_cache)) {
  message("Consultando RAIS via Base dos Dados (BigQuery)...")
  message("  Requer: basedosdados::bd_auth() já executado.")
  message("  Isso pode levar 5-20 minutos (query no BigQuery).")

  # CNAEs do setor elétrico / energia eólica
  cnaes_energia <- c(
    "3511",  # Geração de energia elétrica
    "3512",  # Transmissão de energia elétrica
    "3513",  # Comércio atacadista de energia elétrica
    "4321",  # Instalações elétricas
    "7112",  # Serviços de engenharia (construção de parques eólicos)
    "4292"   # Obras portuárias, marítimas e fluviais (torres eólicas offshore)
  )
  cnaes_construcao <- c("4110", "4120", "4211", "4212", "4213",
                         "4221", "4222", "4223", "4291", "4292",
                         "4299", "4311", "4312", "4313", "4319",
                         "4321", "4322", "4329", "4391", "4399")

  estados_ne_str <- paste0("'", ESTADOS_NE, "'", collapse = ", ")

  query_rais <- glue::glue("
    SELECT
      ano,
      id_municipio                        AS cod_mun,
      COUNT(*)                            AS n_vinculos,
      SUM(CASE WHEN sexo = '2' THEN 1 ELSE 0 END) AS n_mulheres,
      AVG(SAFE_CAST(valor_remuneracao_media AS FLOAT64)) AS salario_medio,
      SUM(CASE WHEN SUBSTR(cnae_2_subclasse, 1, 4) IN
            ({paste0(\"'\", cnaes_energia, \"'\", collapse = ', ')})
          THEN 1 ELSE 0 END) AS n_vinculos_energia,
      SUM(CASE WHEN SUBSTR(cnae_2_subclasse, 1, 4) IN
            ({paste0(\"'\", cnaes_construcao, \"'\", collapse = ', ')})
          THEN 1 ELSE 0 END) AS n_vinculos_construcao
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE
      SUBSTR(id_municipio, 1, 2) IN ({estados_ne_str})
      AND ano BETWEEN {ANO_INICIO} AND {ANO_FIM}
      AND vinculo_ativo_3112 = '1'
    GROUP BY ano, id_municipio
    ORDER BY ano, id_municipio
  ")

  rais_bq <- basedosdados::read_sql(query_rais) |>
    janitor::clean_names()

  saveRDS(rais_bq, rais_cache, compress = "xz")
  message("RAIS agregado salvo em: ", rais_cache)

} else {
  message("Carregando RAIS do cache...")
  rais_bq <- readRDS(rais_cache)
}


# ---- 2. Limpeza e padronização --------------------------------------------

rais_clean <- rais_bq |>
  mutate(
    cod_mun = as.integer(stringr::str_sub(as.character(cod_mun), 1, 7)),
    ano     = as.integer(ano),
    across(c(n_vinculos, n_mulheres, n_vinculos_energia, n_vinculos_construcao),
           ~ suppressWarnings(as.integer(.x))),
    salario_medio = suppressWarnings(as.numeric(salario_medio))
  ) |>
  filter(cod_mun %in% mun_tab$cod_mun,
         ano >= ANO_INICIO, ano <= ANO_FIM)


# ---- 3. Painel município-ano completo -------------------------------------

painel_rais <- mun_tab |>
  select(cod_mun, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS) |>
  left_join(rais_clean, by = c("cod_mun", "ano")) |>
  mutate(
    across(c(n_vinculos, n_mulheres, n_vinculos_energia, n_vinculos_construcao),
           ~ replace_na(.x, 0L)),
    # Proporção de mulheres
    pct_mulheres        = 100 * n_mulheres / pmax(n_vinculos, 1),
    # Vínculos em energia por habitante (calculado em 08_build_panel.R com pop)
    # Log de vínculos
    log_vinculos        = log1p(n_vinculos),
    log_vinculos_energia = log1p(n_vinculos_energia)
  )


# ---- 4. Verificação -------------------------------------------------------

check_rais <- painel_rais |>
  group_by(ano) |>
  summarise(
    total_vinculos  = sum(n_vinculos,          na.rm = TRUE),
    total_energia   = sum(n_vinculos_energia,  na.rm = TRUE),
    salario_medio   = mean(salario_medio,       na.rm = TRUE),
    .groups         = "drop"
  )

message("\nResumo anual — RAIS NE:")
print(tail(check_rais, 10))


# ---- 5. Salvar ------------------------------------------------------------

salvar(painel_rais, "rais_panel")


# ---- 6. Figura: vínculos em energia ao longo do tempo -------------------

fig_rais <- painel_rais |>
  group_by(ano) |>
  summarise(
    vinculos_energia = sum(n_vinculos_energia, na.rm = TRUE),
    vinculos_total   = sum(n_vinculos,          na.rm = TRUE),
    .groups          = "drop"
  ) |>
  ggplot(aes(x = ano, y = vinculos_energia)) +
  geom_col(fill = "#27AE60") +
  geom_line(aes(y = vinculos_total / 100), colour = "darkred", linewidth = 0.8) +
  scale_y_continuous(
    name     = "Vínculos em energia elétrica",
    sec.axis = sec_axis(~ . * 100, name = "Total de vínculos (÷100)")
  ) +
  labs(
    title   = "Vínculos Empregatícios no Setor de Energia — Nordeste",
    subtitle = "RAIS — municípios do Nordeste",
    x       = "Ano",
    caption = "Fonte: MTE/RAIS via Base dos Dados"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(DIR_FIGURES, "06_vinculos_energia.png"),
  plot = fig_rais, width = 10, height = 6, dpi = 300
)

message("\n06_rais.R concluído.")
