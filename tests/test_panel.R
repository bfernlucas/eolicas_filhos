# =============================================================================
# tests/test_panel.R
# Testes unitários do painel com testthat
#
# Execução:
#   testthat::test_file("tests/test_panel.R")
#   # ou, para todos os testes do projeto:
#   testthat::test_dir("tests/")
# =============================================================================

library(testthat)
library(dplyr)
library(tidyr)

# Carrega funções sem executar setup completo (evita efeitos colaterais)
source(here::here("R", "00_setup.R"))
source(here::here("R", "functions.R"))


# ════════════════════════════════════════════════════════════════════════════
# 1. Funções auxiliares — geo
# ════════════════════════════════════════════════════════════════════════════

test_that("cod_uf extrai os 2 primeiros dígitos corretamente", {
  expect_equal(cod_uf(2304400), "23")    # Ceará
  expect_equal(cod_uf(2900801), "29")    # Bahia
  expect_equal(cod_uf("2500000"), "25")  # Paraíba (string)
})

test_that("ibge7 padroniza para inteiro de 7 dígitos", {
  expect_equal(ibge7(23044001), 2304400L)   # trunca dígito verificador
  expect_equal(ibge7("2304400"), 2304400L)   # string → integer
  expect_equal(ibge7(2304400),  2304400L)    # já correto
})

test_that("is_nordeste identifica estados do NE corretamente", {
  expect_true(is_nordeste(2304400))    # CE
  expect_true(is_nordeste(2900801))    # BA
  expect_false(is_nordeste(3304557))   # RJ
  expect_false(is_nordeste(1100015))   # RO
})

test_that("distancia_km retorna valor razoável (Fortaleza → Recife ~ 800 km)", {
  d <- distancia_km(
    lon1 = -38.52, lat1 = -3.72,   # Fortaleza
    lon2 = -34.88, lat2 = -8.05    # Recife
  )
  expect_gt(d, 700)
  expect_lt(d, 900)
})


# ════════════════════════════════════════════════════════════════════════════
# 2. Funções auxiliares — estatísticas
# ════════════════════════════════════════════════════════════════════════════

test_that("taxa_mil retorna resultado correto com denominador mínimo 1", {
  expect_equal(taxa_mil(10, 1000), 10)
  expect_equal(taxa_mil(5, 0), 5000)    # pmax(0, 1) → divide por 1
  expect_equal(taxa_mil(0, 100), 0)
})

test_that("winsorize mantém valores dentro dos percentis", {
  set.seed(42)
  x <- c(rnorm(98), -100, 100)
  wx <- winsorize(x, 0.01, 0.99)
  expect_lte(max(wx), quantile(x, 0.99))
  expect_gte(min(wx), quantile(x, 0.01))
  expect_equal(length(wx), length(x))
})

test_that("log_shift trata zeros corretamente", {
  expect_equal(log_shift(0), log(0.5))
  expect_equal(log_shift(0, shift = 1), 0)  # log(0 + 1) = 0
  expect_gt(log_shift(10), 0)
})

test_that("padronizar produz média ~ 0 e sd ~ 1", {
  x <- c(1, 2, 3, 4, 5)
  px <- padronizar(x)
  expect_equal(mean(px), 0, tolerance = 1e-10)
  expect_equal(sd(px), 1, tolerance = 1e-10)
})


# ════════════════════════════════════════════════════════════════════════════
# 3. Manipulação de painel
# ════════════════════════════════════════════════════════════════════════════

# Cria painel sintético de teste
painel_sintetico <- tidyr::crossing(
  cod_mun = c(2304400L, 2900801L, 3304557L),
  ano     = 2000L:2005L
) |>
  mutate(
    cod_uf = cod_uf(cod_mun),
    sigla_uf = case_when(
      cod_uf == "23" ~ "CE",
      cod_uf == "29" ~ "BA",
      TRUE           ~ "RJ"
    ),
    nome_mun = paste("Mun", cod_mun)
  )

test_that("checar_balanceamento detecta painel balanceado", {
  result <- checar_balanceamento(painel_sintetico)
  expect_true(result$balanceado)
  expect_equal(result$n_ids, 3L)
  expect_equal(result$n_times, 6L)
  expect_equal(result$n_obs, 18L)
  expect_equal(result$n_duplicatas, 0L)
})

test_that("checar_balanceamento detecta painel desbalanceado", {
  painel_quebrado <- painel_sintetico[-1, ]
  expect_warning(
    result <- checar_balanceamento(painel_quebrado),
    "desbalanceado"
  )
  expect_false(result$balanceado)
})

test_that("gerar_vars_did cria variáveis corretas para tratados e nunca-tratados", {
  dados_teste <- painel_sintetico |>
    mutate(
      ano_primeiro_eolica = case_when(
        cod_mun == 2304400L ~ 2002L,   # tratado em 2002
        cod_mun == 2900801L ~ 2004L,   # tratado em 2004
        TRUE                ~ NA_integer_  # nunca tratado
      )
    ) |>
    gerar_vars_did()

  # g_cs = 0 para nunca-tratados
  expect_equal(
    dados_teste |> filter(cod_mun == 3304557L) |> pull(g_cs) |> unique(),
    0L
  )

  # g_sa = NA para nunca-tratados
  expect_true(
    all(is.na(dados_teste |> filter(cod_mun == 3304557L) |> pull(g_sa)))
  )

  # never_treated = 1 para nunca-tratados
  expect_equal(
    dados_teste |> filter(cod_mun == 3304557L) |> pull(never_treated) |> unique(),
    1L
  )

  # d_post = 1 somente após ano de tratamento
  mun_ce <- dados_teste |> filter(cod_mun == 2304400L)
  expect_equal(mun_ce |> filter(ano < 2002) |> pull(d_post) |> unique(), 0L)
  expect_equal(mun_ce |> filter(ano >= 2002) |> pull(d_post) |> unique(), 1L)

  # rel_time correto
  expect_equal(
    mun_ce |> filter(ano == 2002) |> pull(rel_time),
    0L
  )
  expect_equal(
    mun_ce |> filter(ano == 2000) |> pull(rel_time),
    -2L
  )

  # NA para rel_time dos nunca-tratados
  expect_true(
    all(is.na(dados_teste |> filter(cod_mun == 3304557L) |> pull(rel_time)))
  )
})

test_that("zeros_contagem substitui NA por 0L em colunas y_*", {
  df <- tibble::tibble(
    cod_mun = 1L:3L,
    y_adol  = c(1L, NA, 3L),
    y_mortes = c(NA, NA, 5L),
    outra   = c(NA, 1L, NA)   # não deve ser alterada
  )
  resultado <- zeros_contagem(df)
  expect_equal(resultado$y_adol, c(1L, 0L, 3L))
  expect_equal(resultado$y_mortes, c(0L, 0L, 5L))
  expect_equal(resultado$outra, c(NA, 1L, NA))  # inalterada
})


# ════════════════════════════════════════════════════════════════════════════
# 4. Testes de integridade do painel final (requer dados processados)
# ════════════════════════════════════════════════════════════════════════════

# Estes testes só executam se o painel final já foi construído
skip_if(
  !file.exists(here::here("data", "final", "painel_final.rds")),
  "painel_final.rds não encontrado — execute R/08_build_panel.R primeiro"
)

painel <- carregar("painel_final", dir = here::here("data", "final"))

test_that("painel final é balanceado", {
  result <- checar_balanceamento(painel)
  expect_true(result$balanceado)
  expect_equal(result$n_duplicatas, 0L)
})

test_that("painel cobre todos os anos do projeto", {
  anos_panel <- sort(unique(painel$ano))
  expect_equal(anos_panel, ANO_INICIO:ANO_FIM)
})

test_that("todos os municípios do NE estão no painel", {
  n_mun_panel <- dplyr::n_distinct(painel$cod_mun)
  expect_gte(n_mun_panel, 1700)   # NE tem 1.794 municípios
})

test_that("g_cs é 0 para nunca-tratados e > 0 para tratados", {
  expect_true(all(painel$g_cs >= 0))
  expect_true(0L %in% painel$g_cs)   # há nunca-tratados
  expect_true(any(painel$g_cs > 0))  # há tratados
})

test_that("d_post está correto em relação a g_cs e ano", {
  # Tratados: d_post = 1 somente quando ano >= g_cs (e g_cs > 0)
  inconsistentes <- painel |>
    dplyr::filter(g_cs > 0) |>
    dplyr::mutate(
      esperado = as.integer(ano >= g_cs),
      ok       = (d_post == esperado)
    ) |>
    dplyr::filter(!ok)
  expect_equal(nrow(inconsistentes), 0L)
})

test_that("variáveis de contagem y_* não têm NA", {
  colunas_y <- names(painel)[startsWith(names(painel), "y_")]
  if (length(colunas_y) > 0) {
    n_na <- painel |>
      dplyr::select(dplyr::all_of(colunas_y)) |>
      is.na() |>
      sum()
    expect_equal(n_na, 0L)
  }
})

test_that("taxas por mil não têm valores extremos (> 500)", {
  colunas_tx <- names(painel)[startsWith(names(painel), "tx_")]
  for (col in colunas_tx) {
    vals <- painel[[col]][!is.na(painel[[col]])]
    expect_lte(max(vals), 500,
               label = paste("taxa máxima em", col))
  }
})

message("\nTodos os testes concluídos.")
