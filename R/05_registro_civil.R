# =============================================================================
# 05_registro_civil.R
# Registro Civil — pais ausentes e reconhecimento de paternidade
#
# Fonte: Portal da Transparência do Registro Civil
#   https://portaldetransparencia.registrocivil.org.br/
#   Operado pelo CNJ em parceria com Associações de Registradores Civis
#
# Dado disponível via API pública (JSON) ou download em CSV por período.
# Variáveis de interesse:
#   → Nascimentos sem declaração do pai (pai ausente)
#   → Reconhecimento voluntário de paternidade
#   → Nascimentos por município de registro
#
# Nota metodológica: o município de REGISTRO pode diferir do de RESIDÊNCIA.
# Nos scripts de fusão (08_build_panel.R) usamos o município de residência
# como unidade. Os dados do registro civil são uma proxy imperfeita.
#
# Saída:
#   data/processed/registro_civil_panel.rds — tibble município-ano
# =============================================================================

source(here::here("R", "00_setup.R"))

mun_tab <- carregar("municipios_nordeste_tab")


# ---- 1. Download via Portal da Transparência do Registro Civil ------------
# O portal disponibiliza dados em CSV por ano, com granularidade municipal.
# URL padrão: https://transparencia.registrocivil.org.br/registros
#
# A API REST permite consultas por tipo de evento, UF, município e período.
# Endpoint: GET /api/record/birth?start_date=YYYY-01-01&end_date=YYYY-12-31&...

api_base <- "https://transparencia.registrocivil.org.br/api/record/birth"

baixar_rc_ano <- function(ano, uf) {
  url <- glue::glue(
    "{api_base}?",
    "start_date={ano}-01-01&end_date={ano}-12-31&",
    "state={uf}&groupBy=city"
  )

  resp <- tryCatch(
    httr::GET(url, httr::timeout(60)),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("  Falha: ", uf, " / ", ano)
    return(NULL)
  }

  conteudo <- httr::content(resp, "text", encoding = "UTF-8")
  dados     <- jsonlite::fromJSON(conteudo, flatten = TRUE)

  if (is.null(dados$data)) return(NULL)

  as_tibble(dados$data) |>
    mutate(ano = as.integer(ano), sigla_uf = uf)
}

rc_cache <- file.path(DIR_RAW, "registro_civil", "rc_nascimentos_nordeste.rds")

if (!file.exists(rc_cache)) {
  message("Baixando dados do Registro Civil — NE, 2000-2022...")
  message("  Isso pode levar 10-30 minutos (API REST).")

  rc_raw_list <- purrr::map(SIGLAS_NE, function(uf) {
    message("  Registro Civil — UF: ", uf)
    purrr::map_dfr(ANOS, ~ baixar_rc_ano(.x, uf))
  })

  rc_raw <- bind_rows(rc_raw_list) |> janitor::clean_names()

  saveRDS(rc_raw, rc_cache, compress = "xz")
  message("Registro Civil salvo em: ", rc_cache)
} else {
  message("Carregando Registro Civil do cache...")
  rc_raw <- readRDS(rc_cache)
}


# ---- 2. Limpeza e padronização das colunas --------------------------------

message("Colunas disponíveis: ", paste(names(rc_raw), collapse = ", "))

# O Portal da Transparência usa nomenclatura própria; mapeamos as mais comuns.
# Ajuste os nomes conforme a resposta real da API.
rc_clean <- rc_raw |>
  rename_with(~ case_match(
    .x,
    "city_ibge_code"  ~ "cod_mun",
    "city"            ~ "nome_mun_rc",
    "total"           ~ "n_nascimentos_rc",
    "no_father"       ~ "n_sem_pai",          # nascimentos sem pai declarado
    "acknowledged"    ~ "n_reconhecimento",   # reconhecimentos voluntários
    "father_present"  ~ "n_com_pai",
    .default = .x
  ), .cols = everything()) |>
  mutate(
    cod_mun = as.integer(stringr::str_sub(as.character(cod_mun), 1, 7)),
    ano     = as.integer(ano),
    across(c(n_nascimentos_rc, n_sem_pai, n_reconhecimento, n_com_pai),
           ~ suppressWarnings(as.integer(.x)))
  ) |>
  filter(cod_mun %in% mun_tab$cod_mun,
         ano >= ANO_INICIO, ano <= ANO_FIM)


# ---- 3. Construir painel município-ano ------------------------------------

rc_agg <- rc_clean |>
  group_by(cod_mun, ano) |>
  summarise(
    y_nasc_rc          = sum(n_nascimentos_rc, na.rm = TRUE),
    y_sem_pai          = sum(n_sem_pai,         na.rm = TRUE),
    y_reconhecimento   = sum(n_reconhecimento,  na.rm = TRUE),
    y_com_pai          = sum(n_com_pai,          na.rm = TRUE),
    .groups            = "drop"
  )


# ---- 4. Painel completo município-ano -------------------------------------

painel_rc <- mun_tab |>
  select(cod_mun, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS) |>
  left_join(rc_agg, by = c("cod_mun", "ano")) |>
  mutate(
    across(starts_with("y_"), ~ replace_na(.x, 0L)),
    # Taxa de ausência paterna (por 100 nascimentos registrados)
    tx_sem_pai         = 100 * y_sem_pai       / pmax(y_nasc_rc, 1),
    # Taxa de reconhecimento espontâneo (por 100 nascimentos sem pai)
    tx_reconhecimento  = 100 * y_reconhecimento / pmax(y_sem_pai, 1)
  )


# ---- 5. Série temporal de conferência -------------------------------------

serie_rc <- painel_rc |>
  group_by(ano) |>
  summarise(
    total_sem_pai       = sum(y_sem_pai,        na.rm = TRUE),
    total_reconhecimento = sum(y_reconhecimento, na.rm = TRUE),
    tx_media_sem_pai    = mean(tx_sem_pai,       na.rm = TRUE),
    .groups             = "drop"
  )

print(serie_rc |> filter(ano >= 2010))


# ---- 6. Salvar -------------------------------------------------------------

salvar(painel_rc, "registro_civil_panel")


# ---- 7. Figura: taxa de ausência paterna ---------------------------------

fig_rc <- painel_rc |>
  group_by(ano, sigla_uf) |>
  summarise(tx_media = mean(tx_sem_pai, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = ano, y = tx_media, colour = sigla_uf, group = sigla_uf)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_colour_brewer(palette = "Set1", name = "UF") +
  labs(
    title    = "Taxa Média de Ausência Paterna no Registro de Nascimento",
    subtitle = "Por UF — Nordeste Brasileiro",
    x        = "Ano",
    y        = "% de nascimentos sem pai declarado",
    caption  = "Fonte: Portal da Transparência do Registro Civil / CNJ"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(DIR_FIGURES, "05_ausencia_paterna.png"),
  plot = fig_rc, width = 10, height = 6, dpi = 300
)

message("\n05_registro_civil.R concluído.")
