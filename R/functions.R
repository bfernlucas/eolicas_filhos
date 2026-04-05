# =============================================================================
# functions.R
# Funções reutilizáveis do projeto — carregadas por 00_setup.R
#
# Organização:
#   1. I/O e cache
#   2. Tratamento de dados geográficos
#   3. Manipulação de painel
#   4. Transformações estatísticas
#   5. Visualização (ggplot2 helpers)
#   6. Econometria (wrappers DiD)
#   7. Formatação de tabelas
# =============================================================================


# ════════════════════════════════════════════════════════════════════════════
# 1. I/O e CACHE
# ════════════════════════════════════════════════════════════════════════════

#' Salva objeto R comprimido em data/processed/
salvar <- function(obj, nome, dir = DIR_PROCESSED, compress = "xz") {
  caminho <- file.path(dir, paste0(nome, ".rds"))
  saveRDS(obj, caminho, compress = compress)
  message("Salvo: ", caminho, " (", format(object.size(obj), units = "MB"), ")")
  invisible(caminho)
}

#' Carrega objeto R comprimido de data/processed/
carregar <- function(nome, dir = DIR_PROCESSED) {
  caminho <- file.path(dir, paste0(nome, ".rds"))
  if (!file.exists(caminho))
    stop("Arquivo não encontrado: ", caminho,
         "\nExecute o script correspondente primeiro.")
  readRDS(caminho)
}

#' Salva em Parquet (mais eficiente que RDS para dados tabulares)
salvar_parquet <- function(df, nome, dir = DIR_PROCESSED) {
  caminho <- file.path(dir, paste0(nome, ".parquet"))
  arrow::write_parquet(df, caminho, compression = "zstd")
  message("Parquet salvo: ", caminho)
  invisible(caminho)
}

#' Carrega Parquet
carregar_parquet <- function(nome, dir = DIR_PROCESSED) {
  caminho <- file.path(dir, paste0(nome, ".parquet"))
  if (!file.exists(caminho))
    stop("Parquet não encontrado: ", caminho)
  arrow::read_parquet(caminho)
}

#' Verifica se um arquivo de cache existe (e não está vazio)
cache_ok <- function(caminho) {
  file.exists(caminho) && file.size(caminho) > 1024
}


# ════════════════════════════════════════════════════════════════════════════
# 2. TRATAMENTO DE DADOS GEOGRÁFICOS
# ════════════════════════════════════════════════════════════════════════════

#' Extrai código UF (2 dígitos) do código IBGE do município
cod_uf <- function(cod_mun) {
  stringr::str_sub(as.character(cod_mun), 1, 2)
}

#' Testa se o município pertence ao Nordeste
is_nordeste <- function(cod_mun) {
  cod_uf(cod_mun) %in% ESTADOS_NE
}

#' Padroniza código IBGE para inteiro de 7 dígitos
ibge7 <- function(x) {
  as.integer(stringr::str_sub(as.character(x), 1, 7))
}

#' Distância em km entre dois pontos (Haversine)
distancia_km <- function(lon1, lat1, lon2, lat2) {
  R <- 6371.0  # raio da Terra em km
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * R * asin(sqrt(a))
}


# ════════════════════════════════════════════════════════════════════════════
# 3. MANIPULAÇÃO DE PAINEL
# ════════════════════════════════════════════════════════════════════════════

#' Cria backbone balanceado do painel (produto cartesiano município × ano)
criar_backbone <- function(mun_tab, anos = ANOS) {
  mun_tab |>
    dplyr::select(cod_mun, cod_uf, sigla_uf, nome_mun) |>
    tidyr::crossing(ano = anos)
}

#' Verifica se o painel está balanceado
checar_balanceamento <- function(painel, id_col = "cod_mun", time_col = "ano") {
  n_ids   <- dplyr::n_distinct(painel[[id_col]])
  n_times <- dplyr::n_distinct(painel[[time_col]])
  n_obs   <- nrow(painel)
  esperado <- n_ids * n_times
  lista <- list(
    balanceado  = (n_obs == esperado),
    n_ids       = n_ids,
    n_times     = n_times,
    n_obs       = n_obs,
    n_esperado  = esperado,
    n_duplicatas = sum(duplicated(painel[c(id_col, time_col)]))
  )
  if (!lista$balanceado)
    warning("Painel desbalanceado: ", n_obs, " obs vs ", esperado, " esperadas")
  if (lista$n_duplicatas > 0)
    warning("Painel com ", lista$n_duplicatas, " duplicatas!")
  invisible(lista)
}

#' Gera variáveis DiD padrão a partir do painel
gerar_vars_did <- function(painel, col_primeiro_trat = "ano_primeiro_eolica") {
  painel |>
    dplyr::mutate(
      # Para Callaway & Sant'Anna: g = 0 para nunca-tratados
      g_cs = dplyr::if_else(
        is.na(.data[[col_primeiro_trat]]), 0L,
        as.integer(.data[[col_primeiro_trat]])
      ),
      # Para Sun & Abraham: g = NA para nunca-tratados
      g_sa = dplyr::if_else(
        is.na(.data[[col_primeiro_trat]]), NA_integer_,
        as.integer(.data[[col_primeiro_trat]])
      ),
      # Post-tratamento
      d_post = as.integer(
        !is.na(.data[[col_primeiro_trat]]) & ano >= .data[[col_primeiro_trat]]
      ),
      # Tempo relativo
      rel_time = dplyr::if_else(
        !is.na(.data[[col_primeiro_trat]]),
        as.integer(ano - .data[[col_primeiro_trat]]),
        NA_integer_
      ),
      # Nunca-tratado
      never_treated = as.integer(is.na(.data[[col_primeiro_trat]]))
    )
}

#' Preenche NA com 0 em variáveis de contagem (y_*)
zeros_contagem <- function(painel) {
  painel |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("y_"), ~ tidyr::replace_na(.x, 0L))
    )
}


# ════════════════════════════════════════════════════════════════════════════
# 4. TRANSFORMAÇÕES ESTATÍSTICAS
# ════════════════════════════════════════════════════════════════════════════

#' Taxa por 1.000 (denominador mínimo = 1)
taxa_mil <- function(x, n) (x / pmax(n, 1)) * 1000

#' Taxa por 100.000
taxa_100mil <- function(x, n) (x / pmax(n, 1)) * 100000

#' Percentual (0-100)
pct <- function(x, n) 100 * x / pmax(n, 1)

#' Winsorize nos percentis p_low e p_high (sobre toda a amostra)
winsorize <- function(x, p_low = 0.01, p_high = 0.99) {
  q <- quantile(x, c(p_low, p_high), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

#' Log(x + 0.5) — tratamento de zeros em variáveis de contagem
log_shift <- function(x, shift = 0.5) log(x + shift)

#' Normaliza variável para média 0 e desvio padrão 1
padronizar <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

#' Crescimento percentual de uma série (year-over-year)
crescimento_pct <- function(x) {
  c(NA, diff(x) / abs(dplyr::lag(x)[-1]) * 100)
}


# ════════════════════════════════════════════════════════════════════════════
# 5. VISUALIZAÇÃO
# ════════════════════════════════════════════════════════════════════════════

#' Tema padrão do projeto (ggplot2)
tema_projeto <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      strip.text         = ggplot2::element_text(face = "bold"),
      plot.title         = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle      = ggplot2::element_text(colour = "grey40", size = base_size - 1),
      plot.caption       = ggplot2::element_text(colour = "grey50", size = base_size - 3,
                                                  hjust = 0),
      legend.position    = "bottom",
      axis.text          = ggplot2::element_text(size = base_size - 1)
    )
}

#' Paleta de cores do projeto
cores_projeto <- list(
  tratado   = "#2196F3",   # azul
  controle  = "#9E9E9E",   # cinza
  positivo  = "#4CAF50",   # verde
  negativo  = "#F44336",   # vermelho
  neutro    = "#FF9800",   # laranja
  semiarido = "#E67E22"    # laranja-terra
)

#' Salva figura em PNG e PDF
salvar_figura <- function(p, nome, width = 10, height = 7, dpi = 300) {
  for (ext in c("png", "pdf")) {
    caminho <- file.path(DIR_FIGURES, paste0(nome, ".", ext))
    ggplot2::ggsave(caminho, plot = p, width = width, height = height, dpi = dpi)
  }
  message("Figura salva: ", nome, " (PNG + PDF)")
  invisible(p)
}

#' Salva mapa em PNG e PDF
salvar_mapa <- function(p, nome, width = 10, height = 9, dpi = 300) {
  for (ext in c("png", "pdf")) {
    caminho <- file.path(DIR_MAPS, paste0(nome, ".", ext))
    ggplot2::ggsave(caminho, plot = p, width = width, height = height, dpi = dpi)
  }
  message("Mapa salvo: ", nome, " (PNG + PDF)")
  invisible(p)
}

#' Adiciona linha de referência no zero e linha vertical no t=0 (event study)
geom_event_study_ref <- function() {
  list(
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5),
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey60")
  )
}


# ════════════════════════════════════════════════════════════════════════════
# 6. ECONOMETRIA — WRAPPERS DiD
# ════════════════════════════════════════════════════════════════════════════

#' Roda Callaway & Sant'Anna para um outcome e retorna lista de agregações
rodar_cs2021 <- function(outcome, dados, grupo_controle = "nevertreated",
                          antecipacao = 0L, xformla = ~ d_semiarido + d_covid) {
  stopifnot(outcome %in% names(dados))

  res <- tryCatch(
    did::att_gt(
      yname         = outcome,
      tname         = "ano",
      idname        = "cod_mun",
      gname         = "g_cs",
      xformla       = xformla,
      control_group = grupo_controle,
      anticipation  = antecipacao,
      base_period   = "universal",
      est_method    = "reg",
      data          = dados,
      panel         = TRUE,
      allow_unbalanced_panel = FALSE
    ),
    error = function(e) {
      message("  ERRO CS2021 [", outcome, "]: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(res)) return(NULL)

  list(
    att_gt  = res,
    simple  = tryCatch(did::aggte(res, type = "simple",  na.rm = TRUE), error = ~ NULL),
    dynamic = tryCatch(did::aggte(res, type = "dynamic", na.rm = TRUE,
                                   min_e = -8, max_e = 8),             error = ~ NULL),
    group   = tryCatch(did::aggte(res, type = "group",   na.rm = TRUE), error = ~ NULL)
  )
}

#' Extrai ATT simples de um objeto CS2021
extrair_att_simples <- function(cs_resultado, outcome) {
  ag <- cs_resultado$simple
  if (is.null(ag)) return(tibble::tibble(outcome = outcome, att = NA, se = NA))
  tibble::tibble(
    outcome = outcome,
    att     = ag$overall.att,
    se      = ag$overall.se,
    p_value = 2 * stats::pnorm(-abs(ag$overall.att / ag$overall.se)),
    n_obs   = ag$n
  )
}

#' Roda Sun & Abraham via fixest para um outcome
rodar_sa2021 <- function(outcome, dados,
                          controles = "+ d_semiarido + d_covid") {
  formula_str <- paste0(
    outcome, " ~ sunab(g_sa, ano)", controles, " | cod_mun + ano"
  )
  tryCatch(
    fixest::feols(
      fml     = stats::as.formula(formula_str),
      data    = dados,
      cluster = ~cod_mun,
      notes   = FALSE
    ),
    error = function(e) {
      message("  ERRO SA2021 [", outcome, "]: ", conditionMessage(e))
      NULL
    }
  )
}

#' Roda TWFE simples via fixest para um outcome
rodar_twfe <- function(outcome, dados,
                        controles = "+ d_semiarido + d_covid") {
  formula_str <- paste0(
    outcome, " ~ d_post", controles, " | cod_mun + ano"
  )
  tryCatch(
    fixest::feols(
      fml     = stats::as.formula(formula_str),
      data    = dados,
      cluster = ~cod_mun,
      notes   = FALSE
    ),
    error = function(e) {
      message("  ERRO TWFE [", outcome, "]: ", conditionMessage(e))
      NULL
    }
  )
}


# ════════════════════════════════════════════════════════════════════════════
# 7. FORMATAÇÃO DE TABELAS
# ════════════════════════════════════════════════════════════════════════════

#' Formata número com separador de milhar e casas decimais
fmt_num <- function(x, digits = 2, big.mark = ".", decimal.mark = ",") {
  formatC(x, format = "f", digits = digits,
          big.mark = big.mark, decimal.mark = decimal.mark)
}

#' Adiciona asteriscos de significância
fmt_stars <- function(p) {
  dplyr::case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE      ~ ""
  )
}

#' Formata coeficiente com erro padrão e asteriscos para tabela
fmt_coef <- function(att, se, p = NULL, digits = 3) {
  coef_str <- fmt_num(att, digits)
  se_str   <- paste0("(", fmt_num(se, digits), ")")
  stars    <- if (!is.null(p)) fmt_stars(p) else ""
  paste0(coef_str, stars, "\n", se_str)
}

#' Cria tabela de comparação de estimadores (TWFE, CS2021, SA2021)
tabela_comparacao <- function(twfe_list, cs_list, sa_list, outcomes, labels) {
  purrr::map_dfr(outcomes, function(y) {
    # TWFE
    m_twfe <- twfe_list[[y]]
    att_twfe <- if (!is.null(m_twfe)) coef(m_twfe)["d_post"] else NA
    se_twfe  <- if (!is.null(m_twfe))
      sqrt(diag(fixest::vcov(m_twfe, type = "cluster")))["d_post"] else NA

    # CS2021
    ag_cs <- cs_list[[y]]$simple
    att_cs <- if (!is.null(ag_cs)) ag_cs$overall.att else NA
    se_cs  <- if (!is.null(ag_cs)) ag_cs$overall.se  else NA

    tibble::tibble(
      variavel = labels[y],
      att_twfe = att_twfe, se_twfe = se_twfe,
      att_cs   = att_cs,   se_cs   = se_cs,
      p_twfe   = 2 * stats::pnorm(-abs(att_twfe / se_twfe)),
      p_cs     = 2 * stats::pnorm(-abs(att_cs / se_cs))
    )
  })
}
