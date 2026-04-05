# =============================================================================
# Makefile — Eólicas e Filhos
# =============================================================================
# Uso:
#   make download        # baixar todos os dados (requer internet)
#   make download-aneel  # baixar apenas ANEEL SIGA
#   make aggregate       # agregar microdados DataSUS em parquets
#   make install-r       # instalar R e pacotes (Ubuntu/Debian)
#   make install-python  # instalar dependências Python
#   make run             # executar pipeline R completo (targets)
#   make clean           # limpar dados processados e outputs
#   make help            # listar comandos

PYTHON := python3
RSCRIPT := Rscript

.PHONY: help download download-aneel download-ibge download-datasus \
        download-ibge-pop download-ipea aggregate install-r install-python \
        run clean check-r check-python

help:
	@echo ""
	@echo "╔══════════════════════════════════════════════════════╗"
	@echo "║    Eólicas e Filhos — Comandos Disponíveis           ║"
	@echo "╚══════════════════════════════════════════════════════╝"
	@echo ""
	@echo "  DOWNLOAD (rodar localmente com internet):"
	@echo "    make download          Baixar TODAS as fontes"
	@echo "    make download-aneel    Apenas ANEEL SIGA"
	@echo "    make download-ibge     Apenas shapefiles IBGE/geobr"
	@echo "    make download-sudene   Apenas semi-árido SUDENE (+ templates)"
	@echo "    make download-datasus  Apenas DataSUS (SIM, SINASC, SIH)"
	@echo "    make download-ibge-pop Apenas população IBGE SIDRA"
	@echo "    make download-ipea     Apenas IPEADATA (PIB, IDH, Gini)"
	@echo ""
	@echo "  PROCESSAMENTO:"
	@echo "    make aggregate         Agregar microdados DataSUS → parquet"
	@echo ""
	@echo "  CONFIGURAÇÃO:"
	@echo "    make install-python    pip install dependências Python"
	@echo "    make install-r         Instalar R + pacotes (Ubuntu/Debian)"
	@echo "    make renv-restore      Restaurar pacotes R via renv"
	@echo ""
	@echo "  ANÁLISE (requer R instalado + dados baixados):"
	@echo "    make run               Pipeline completo via targets::tar_make()"
	@echo "    make run-geo           Apenas 01_geo_nordeste.R"
	@echo "    make run-panel         Apenas 08_build_panel.R"
	@echo "    make run-did           Apenas 10_staggered_did.R"
	@echo ""
	@echo "  MANUTENÇÃO:"
	@echo "    make clean             Limpar data/processed/ e output/"
	@echo "    make status            Mostrar status dos arquivos de dados"
	@echo ""

# ── Verificações ─────────────────────────────────────────────────────────────

check-python:
	@$(PYTHON) --version || (echo "Python 3 não encontrado" && exit 1)
	@$(PYTHON) -c "import requests, pandas, pysus" 2>/dev/null || \
		(echo "Dependências ausentes. Execute: make install-python" && exit 1)

check-r:
	@$(RSCRIPT) --version || (echo "R não encontrado. Execute: make install-r" && exit 1)

# ── Instalação ───────────────────────────────────────────────────────────────

install-python:
	@echo "Instalando dependências Python..."
	$(PYTHON) -m pip install --upgrade pip
	$(PYTHON) -m pip install requests pandas geopandas pysus pyarrow tqdm \
		openpyxl lxml tabula-py

install-r:
	@echo "Instalando R (Ubuntu/Debian)..."
	sudo apt-get update -q
	sudo apt-get install -y r-base r-base-dev libcurl4-openssl-dev \
		libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev \
		libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev \
		libjpeg-dev libgdal-dev libudunits2-dev
	$(RSCRIPT) -e "install.packages('renv', repos='https://cloud.r-project.org')"

renv-restore:
	@check-r
	$(RSCRIPT) -e "renv::restore()"

# ── Downloads ────────────────────────────────────────────────────────────────

download: check-python
	@echo "=== Baixando TODAS as fontes ==="
	$(PYTHON) scripts/download_data.py --fonte all

download-aneel: check-python
	$(PYTHON) scripts/download_data.py --fonte aneel

download-ibge: check-python
	$(PYTHON) scripts/download_data.py --fonte ibge

download-sudene: check-python
	$(PYTHON) scripts/download_data.py --fonte sudene

download-datasus: check-python
	@echo "AVISO: Download do DataSUS pode levar 2-6 horas."
	@echo "Pressione Enter para continuar ou Ctrl+C para cancelar."
	@read _
	$(PYTHON) scripts/download_data.py --fonte datasus

download-ibge-pop: check-python
	$(PYTHON) scripts/download_data.py --fonte ibge_pop

download-ipea: check-python
	$(PYTHON) scripts/download_data.py --fonte ipea

# ── Processamento ─────────────────────────────────────────────────────────────

aggregate: check-python
	@echo "=== Agregando microdados DataSUS ==="
	$(PYTHON) scripts/aggregate_datasus.py

# ── Pipeline R ───────────────────────────────────────────────────────────────

run: check-r
	@echo "=== Executando pipeline completo (targets) ==="
	$(RSCRIPT) -e "library(targets); tar_make()"

run-geo: check-r
	$(RSCRIPT) R/01_geo_nordeste.R

run-semiarido: check-r
	$(RSCRIPT) R/02_semiarido_sudene.R

run-aneel: check-r
	$(RSCRIPT) R/03_eolicas_aneel.R

run-saude: check-r
	$(RSCRIPT) R/04_saude_datasus.R

run-rc: check-r
	$(RSCRIPT) R/05_registro_civil.R

run-rais: check-r
	$(RSCRIPT) R/06_rais.R

run-controles: check-r
	$(RSCRIPT) R/07_controles_ipea.R

run-panel: check-r
	$(RSCRIPT) R/08_build_panel.R

run-descritivas: check-r
	$(RSCRIPT) R/09_descritivas.R

run-did: check-r
	$(RSCRIPT) R/10_staggered_did.R

run-robustez: check-r
	$(RSCRIPT) R/11_robustez.R

# ── Status ───────────────────────────────────────────────────────────────────

status:
	@echo ""
	@echo "=== Status dos arquivos de dados ==="
	@echo ""
	@echo "-- ANEEL --"
	@ls -lh data/raw/aneel/ 2>/dev/null | grep -v "^total" || echo "  (vazio)"
	@echo ""
	@echo "-- IBGE --"
	@ls -lh data/raw/ibge/ 2>/dev/null | grep -v "^total" | head -5 || echo "  (vazio)"
	@echo ""
	@echo "-- SUDENE --"
	@ls -lh data/raw/sudene/ 2>/dev/null | grep -v "^total" || echo "  (vazio)"
	@echo ""
	@echo "-- DataSUS --"
	@echo "  SIM:    $$(ls data/raw/datasus/sim/*.parquet 2>/dev/null | wc -l) arquivos"
	@echo "  SINASC: $$(ls data/raw/datasus/sinasc/*.parquet 2>/dev/null | wc -l) arquivos"
	@echo "  SIH:    $$(ls data/raw/datasus/sih/*.parquet 2>/dev/null | wc -l) arquivos"
	@echo ""
	@echo "-- Processados --"
	@ls -lh data/processed/*.parquet data/processed/*.rds 2>/dev/null \
		| grep -v "^total" || echo "  (vazio)"
	@echo ""
	@echo "-- Final --"
	@ls -lh data/final/ 2>/dev/null | grep -v "^total" || echo "  (vazio)"
	@echo ""

# ── Limpeza ───────────────────────────────────────────────────────────────────

clean:
	@echo "Limpando dados processados e outputs..."
	rm -f data/processed/*.rds data/processed/*.parquet data/processed/*.csv
	rm -f data/final/*.rds data/final/*.csv
	rm -rf output/tables/* output/figures/* output/maps/*
	@echo "Done."

clean-all: clean
	@echo "Limpando TODOS os dados (raw também)..."
	rm -rf data/raw/aneel/* data/raw/ibge/*.gpkg data/raw/ibge/*.parquet
	rm -rf data/raw/datasus/sim/* data/raw/datasus/sinasc/* data/raw/datasus/sih/*
	rm -rf data/raw/ipea/*.parquet
	@echo "Done. Execute 'make download' para rebaixar."
