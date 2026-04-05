# =============================================================================
# 00_setup.R
# Configuração do ambiente: pacotes, opções globais e diretórios
#
# Execute este script ANTES de qualquer outro.
# =============================================================================

# ---- 1. renv ----------------------------------------------------------------
# Inicializa renv na primeira execução (cria renv.lock)
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Para instalar todos os pacotes registrados em renv.lock:
#   renv::restore()
# Para registrar o estado atual dos pacotes:
#   renv::snapshot()


# ---- 2. Pacotes necessários -------------------------------------------------

pacotes <- c(
  # --- Manipulação de dados ---
  "tidyverse",      # dplyr, ggplot2, tidyr, readr, purrr, stringr, forcats
  "data.table",     # Alternativa rápida para grandes arquivos
  "janitor",        # Limpeza de nomes de colunas
  "lubridate",      # Datas
  "glue",           # Interpolação de strings

  # --- Dados espaciais ---
  "sf",             # Simple Features (shapefiles)
  "geobr",          # Shapefiles do Brasil (IBGE)

  # --- Download / APIs de dados públicos ---
  "healthbR",       # DataSUS via CRAN: sim_data(), sinasc_data(), sih_data()
  "microdatasus",   # DataSUS alternativo (GitHub, rfsaldanha) — mantido como fallback
  "basedosdados",   # Base dos Dados (BigQuery — RAIS, PIB, etc.)
  "ipeadatar",      # IPEA Data (séries macroeconômicas)
  "httr",           # Requisições HTTP
  "jsonlite",       # JSON
  "readxl",         # Planilhas Excel
  "writexl",        # Exportar Excel

  # --- Econometria: DiD ---
  "fixest",         # TWFE + Sun-Abraham (feols, sunab)
  "did",            # Callaway & Sant'Anna (2021)
  "didimputation",  # Borusyak, Jaravel & Spiess (2024)
  "HonestDiD",      # Rambachan & Roth (2023) — sensitivity analysis
  "bacondecomp",    # Goodman-Bacon decomposition

  # --- Econometria: testes e tabelas ---
  "lmtest",         # Testes de hipótese
  "sandwich",       # Erros padrão robustos (cluster)
  "modelsummary",   # Tabelas de regressão
  "gt",             # Tabelas HTML/LaTeX elegantes
  "kableExtra",     # Tabelas LaTeX adicionais
  "stargazer",      # Tabelas clássicas de regressão

  # --- Visualização ---
  "ggplot2",        # Gráficos (incluído no tidyverse, listado explicitamente)
  "patchwork",      # Combinar gráficos ggplot2
  "scales",         # Formatação de eixos
  "viridis",        # Paletas de cores acessíveis
  "RColorBrewer",   # Paletas de cores

  # --- Pipeline ---
  "targets",        # Pipeline de análise reprodutível
  "tarchetypes",    # Auxiliares para targets

  # --- Utilitários ---
  "here",           # Caminhos relativos ao projeto
  "fs",             # Operações com sistema de arquivos
  "progressr",      # Barra de progresso
  "future",         # Execução paralela
  "furrr"           # purrr com paralelismo (via future)
)

# Instala pacotes ausentes do CRAN (sem usar renv::restore())
pacotes_faltando <- pacotes[!sapply(pacotes, requireNamespace, quietly = TRUE)]
if (length(pacotes_faltando) > 0) {
  message("Instalando pacotes faltantes: ", paste(pacotes_faltando, collapse = ", "))
  install.packages(pacotes_faltando, dependencies = TRUE)
}

# microdatasus está no GitHub (fallback — healthbR é preferido)
if (!requireNamespace("microdatasus", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("rfsaldanha/microdatasus")
}

# Verificar versão do healthbR (CRAN)
if (requireNamespace("healthbR", quietly = TRUE)) {
  message("healthbR v", as.character(packageVersion("healthbR")),
          " carregado — sim_data(), sinasc_data(), sih_data() disponíveis")
}

# didimputation está no GitHub
if (!requireNamespace("didimputation", quietly = TRUE)) {
  remotes::install_github("kylebutts/didimputation")
}

# Carrega todos os pacotes
invisible(lapply(pacotes, library, character.only = TRUE))


# ---- 3. Opções globais ------------------------------------------------------

options(
  scipen       = 999,          # Desabilita notação científica
  digits       = 4,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types   = FALSE
)

# Codificação padrão UTF-8
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")


# ---- 4. Constantes do projeto -----------------------------------------------

# Anos do painel
ANO_INICIO <- 2000L
ANO_FIM    <- 2022L
ANOS       <- ANO_INICIO:ANO_FIM

# Estados do Nordeste (códigos IBGE de 2 dígitos)
ESTADOS_NE <- c(
  "21",  # Maranhão        (MA)
  "22",  # Piauí           (PI)
  "23",  # Ceará           (CE)
  "24",  # Rio Grande do Norte (RN)
  "25",  # Paraíba         (PB)
  "26",  # Pernambuco      (PE)
  "27",  # Alagoas         (AL)
  "28",  # Sergipe         (SE)
  "29"   # Bahia           (BA)
)

SIGLAS_NE <- c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA")

# Faixa etária para gravidez na adolescência
IDADE_ADOL_MIN <- 10L
IDADE_ADOL_MAX <- 19L

# CIDs de interesse — desfechos abortivos
CIDS_ABORTO <- paste0("O0", 0:8)  # O00 a O08
# Expandido: O00, O01, O02, O03, O04, O05, O06, O07, O08

# Período neonatal precoce (óbitos de recém-nascido)
IDADE_RN_MAX_DIAS <- 27L  # 0–27 dias = neonatal

# CIDs de faixa para mortalidade infantil / neonatal (CID-10 capítulo XVI)
CIDS_PERINATAL <- c("P00", "P01", "P02", "P03", "P04", "P05", "P06",
                    "P07", "P08", "P09", "P10", "P11", "P12", "P13",
                    "P14", "P15", "P20", "P21", "P22", "P23", "P24",
                    "P25", "P26", "P27", "P28", "P29", "P35", "P36",
                    "P37", "P38", "P39", "P50", "P51", "P52", "P53",
                    "P54", "P55", "P56", "P57", "P58", "P59", "P60",
                    "P61", "P70", "P71", "P72", "P74", "P75", "P76",
                    "P77", "P78", "P80", "P81", "P83", "P84", "P90",
                    "P91", "P92", "P93", "P94", "P95", "P96")


# ---- 5. Caminhos do projeto -------------------------------------------------

DIR_RAW         <- here::here("data", "raw")
DIR_PROCESSED   <- here::here("data", "processed")
DIR_FINAL       <- here::here("data", "final")
DIR_OUTPUT      <- here::here("output")
DIR_TABLES      <- here::here("output", "tables")
DIR_FIGURES     <- here::here("output", "figures")
DIR_MAPS        <- here::here("output", "maps")

# Garante que todos os diretórios existem
dirs_necessarios <- c(
  DIR_RAW, file.path(DIR_RAW, c("ibge", "sudene", "aneel",
                                  "datasus/sim", "datasus/sinasc",
                                  "datasus/sih", "registro_civil",
                                  "rais", "ipea")),
  DIR_PROCESSED, DIR_FINAL,
  DIR_OUTPUT, DIR_TABLES, DIR_FIGURES, DIR_MAPS
)
fs::dir_create(dirs_necessarios)


# ---- 6. Funções utilitárias -------------------------------------------------

#' Salva objeto R comprimido
salvar <- function(obj, nome, dir = DIR_PROCESSED) {
  caminho <- file.path(dir, paste0(nome, ".rds"))
  saveRDS(obj, caminho, compress = "xz")
  message("Salvo: ", caminho)
  invisible(caminho)
}

#' Carrega objeto R comprimido
carregar <- function(nome, dir = DIR_PROCESSED) {
  caminho <- file.path(dir, paste0(nome, ".rds"))
  if (!file.exists(caminho)) stop("Arquivo não encontrado: ", caminho)
  readRDS(caminho)
}

#' Extrai os 2 primeiros dígitos do código IBGE (UF)
cod_uf <- function(cod_mun) substr(as.character(cod_mun), 1, 2)

#' Verifica se município pertence ao Nordeste
is_nordeste <- function(cod_mun) cod_uf(cod_mun) %in% ESTADOS_NE

#' Formata número como taxa por 1.000
taxa_mil <- function(x, n) (x / n) * 1000

#' Formata número como taxa por 100.000
taxa_100mil <- function(x, n) (x / n) * 100000

#' Winsorize uma variável nos percentis p_low e p_high
winsorize <- function(x, p_low = 0.01, p_high = 0.99) {
  q <- quantile(x, c(p_low, p_high), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

message("\n=== Configuração carregada com sucesso ===")
message(glue::glue("Painel: {ANO_INICIO}–{ANO_FIM} | Nordeste ({length(ESTADOS_NE)} estados)\n"))
