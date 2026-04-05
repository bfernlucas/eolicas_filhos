# =============================================================================
# scripts/install_packages.R
# Instalação de todos os pacotes R necessários ao projeto
#
# Execute UMA VEZ antes de iniciar o projeto:
#   Rscript scripts/install_packages.R
#   # ou, dentro do RStudio:
#   source("scripts/install_packages.R")
#
# O script:
#   1. Instala renv (gerenciamento de versões)
#   2. Instala pacotes do CRAN
#   3. Instala pacotes do GitHub (didimputation, microdatasus)
#   4. Verifica instalação e reporta versões
# =============================================================================

# ---- 0. Configurações -------------------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"))
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")

cat("\n=== Instalação de pacotes — Eólicas e Filhos ===\n\n")


# ---- 1. renv ----------------------------------------------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Instalando renv...\n")
  install.packages("renv")
}

# Se houver renv.lock, oferecer restauração
if (file.exists("renv.lock")) {
  cat("renv.lock encontrado. Para restaurar o ambiente exato do projeto:\n")
  cat("  renv::restore()\n\n")
  cat("Deseja restaurar via renv agora? [s/N]: ")
  resp <- tolower(trimws(readline()))
  if (resp == "s") {
    renv::restore()
    cat("\nAmbiente restaurado via renv. Pronto!\n")
    quit(save = "no")
  }
}


# ---- 2. Pacotes do CRAN -----------------------------------------------------

pacotes_cran <- c(
  # Manipulação de dados
  "tidyverse",
  "data.table",
  "janitor",
  "lubridate",
  "glue",
  "arrow",      # Parquet (leitura/escrita)
  "zoo",        # Interpolação temporal

  # Dados espaciais
  "sf",
  "geobr",
  "geosphere",

  # APIs de dados públicos
  "healthbR",   # DataSUS (SIM, SINASC, SIH)
  "ipeadatar",  # IPEA Data
  "sidrar",     # IBGE SIDRA
  "httr",
  "jsonlite",
  "readxl",
  "writexl",

  # Econometria: DiD
  "fixest",     # TWFE + Sun-Abraham
  "did",        # Callaway & Sant'Anna 2021
  "HonestDiD",  # Rambachan & Roth 2023
  "bacondecomp", # Goodman-Bacon decomposição
  "ggdid",      # Gráficos para did

  # Tabelas
  "modelsummary",
  "gt",
  "kableExtra",
  "stargazer",
  "lmtest",
  "sandwich",

  # Visualização
  "ggplot2",
  "patchwork",
  "scales",
  "viridis",
  "RColorBrewer",

  # Pipeline
  "targets",
  "tarchetypes",

  # Utilitários
  "here",
  "fs",
  "progressr",
  "future",
  "furrr",
  "remotes",
  "testthat",   # Testes
  "quarto"      # Relatórios (interface R)
)

cat("Verificando pacotes do CRAN...\n")
ausentes <- pacotes_cran[!sapply(pacotes_cran, requireNamespace, quietly = TRUE)]

if (length(ausentes) == 0) {
  cat("  Todos os pacotes CRAN já instalados.\n")
} else {
  cat("  Instalando", length(ausentes), "pacotes ausentes:\n")
  cat("  ", paste(ausentes, collapse = ", "), "\n\n")
  install.packages(ausentes, dependencies = TRUE)
}


# ---- 3. Pacotes do GitHub ---------------------------------------------------

cat("\nVerificando pacotes do GitHub...\n")

# didimputation (Borusyak, Jaravel & Spiess 2024)
if (!requireNamespace("didimputation", quietly = TRUE)) {
  cat("  Instalando didimputation (kylebutts/didimputation)...\n")
  remotes::install_github("kylebutts/didimputation", quiet = TRUE)
} else {
  cat("  didimputation: OK (", as.character(packageVersion("didimputation")), ")\n")
}

# microdatasus (fallback para healthbR)
if (!requireNamespace("microdatasus", quietly = TRUE)) {
  cat("  Instalando microdatasus (rfsaldanha/microdatasus)...\n")
  remotes::install_github("rfsaldanha/microdatasus", quiet = TRUE)
} else {
  cat("  microdatasus: OK (", as.character(packageVersion("microdatasus")), ")\n")
}

# basedosdados (BigQuery — RAIS, Registro Civil)
if (!requireNamespace("basedosdados", quietly = TRUE)) {
  cat("  Instalando basedosdados (basedosdados/mais)...\n")
  remotes::install_github("basedosdados/mais", subdir = "python-package/basedosdados",
                           quiet = TRUE)
} else {
  cat("  basedosdados: OK\n")
}


# ---- 4. Verificação final ---------------------------------------------------

cat("\n=== Verificação de versões instaladas ===\n\n")

pkgs_verificar <- c(
  "healthbR", "fixest", "did", "didimputation", "HonestDiD",
  "bacondecomp", "geobr", "sf", "targets", "arrow", "ggdid"
)

for (pkg in pkgs_verificar) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  %-20s %s\n", pkg,
                as.character(packageVersion(pkg))))
  } else {
    cat(sprintf("  %-20s [NAO INSTALADO]\n", pkg))
  }
}

cat("\n=== Configuração concluída ===\n")
cat("Próximo passo: abrir o projeto no RStudio e executar:\n")
cat("  source('R/00_setup.R')\n")
cat("  targets::tar_make()   # pipeline completo\n\n")
