# Eólicas e Filhos: Impactos de Parques Eólicos sobre Resultados Reprodutivos e Familiares no Nordeste Brasileiro

## Descrição

Este repositório contém o código completo para replicar o estudo de **diferenças em diferenças com adoção escalonada** (*staggered DiD*) que investiga os efeitos da instalação de parques eólicos nos municípios do Nordeste brasileiro sobre:

- Desfechos de gestação com resultado abortivo (CID O00–O08)
- Gravidez na adolescência
- Óbitos de recém-nascidos (0–27 dias)
- Reconhecimento de paternidade e pais ausentes
- Vínculos empregatícios em estabelecimentos

### Estratégia de identificação

A variação explorada é a **data de início de operação** do primeiro parque eólico em cada município (fonte: ANEEL SIGA). A adoção escalonada do tratamento — municípios recebem o tratamento em anos diferentes ao longo do período amostral — permite estimar efeitos causais usando estimadores robustos à heterogeneidade de efeitos de tratamento dinâmicos.

---

## Estrutura do Repositório

```
eolicas_filhos/
├── eolicas_filhos.Rproj       # RStudio project file
├── renv.lock                   # Snapshot de pacotes (renv)
├── _targets.R                  # Pipeline principal (targets)
│
├── R/
│   ├── 00_setup.R              # Instalação de pacotes e configurações globais
│   ├── 01_geo_nordeste.R       # Municípios do Nordeste — shapefiles IBGE (geobr)
│   ├── 02_semiarido_sudene.R   # Proxy semi-árido temporal (resoluções SUDENE)
│   ├── 03_eolicas_aneel.R      # Parques eólicos — ANEEL SIGA (primeiro ano de operação)
│   ├── 04_saude_datasus.R      # Saúde — DataSUS (SIM, SINASC, SIH): abortos, adolescência, RN
│   ├── 05_registro_civil.R     # Registro civil — paternidade e pais ausentes
│   ├── 06_rais.R               # Emprego — RAIS (vínculos por município-ano)
│   ├── 07_controles_ipea.R     # Controles adicionais (IPEA, IBGE Cidades, PIB)
│   ├── 08_build_panel.R        # Montagem do painel final município-ano
│   ├── 09_descritivas.R        # Estatísticas descritivas e mapas
│   ├── 10_staggered_did.R      # Estimação: Callaway-Sant'Anna, Sun-Abraham, TWFE
│   └── 11_robustez.R           # Robustez, event study, testes de placebo
│
├── data/
│   ├── raw/                    # Dados brutos baixados pelos scripts (git-ignorado)
│   │   ├── ibge/
│   │   ├── sudene/
│   │   ├── aneel/
│   │   ├── datasus/{sim,sinasc,sih}/
│   │   ├── registro_civil/
│   │   ├── rais/
│   │   └── ipea/
│   ├── processed/              # Dados intermediários limpos (git-ignorado)
│   └── final/                  # Painel analítico final (git-ignorado)
│
├── output/
│   ├── tables/                 # Tabelas em LaTeX/CSV
│   ├── figures/                # Gráficos event study e descritivos
│   └── maps/                   # Mapas do Nordeste
│
└── docs/
    └── codebook.md             # Dicionário completo de variáveis
```

---

## Como Reproduzir

### Pré-requisitos

- R ≥ 4.3.0
- RStudio ≥ 2024.04
- Conta no [Google BigQuery](https://cloud.google.com/bigquery) para acesso ao `basedosdados` (RAIS)
- Conexão com internet para downloads automáticos

### Passo a Passo

#### 1. Clonar o repositório

```bash
git clone https://github.com/bfernlucas/eolicas_filhos.git
cd eolicas_filhos
```

#### 2. Restaurar o ambiente de pacotes

```r
# No console do RStudio:
install.packages("renv")
renv::restore()
```

#### 3. Configurar credenciais (apenas para RAIS via basedosdados)

```r
# Autenticar no Google BigQuery (só é necessário uma vez)
basedosdados::bd_auth()
```

#### 4. Executar o pipeline completo

```r
# Opção A — Pipeline targets (recomendado):
library(targets)
tar_make()

# Opção B — Scripts sequencialmente:
source("R/00_setup.R")
source("R/01_geo_nordeste.R")
source("R/02_semiarido_sudene.R")
source("R/03_eolicas_aneel.R")
source("R/04_saude_datasus.R")
source("R/05_registro_civil.R")
source("R/06_rais.R")
source("R/07_controles_ipea.R")
source("R/08_build_panel.R")
source("R/09_descritivas.R")
source("R/10_staggered_did.R")
source("R/11_robustez.R")
```

---

## Fontes de Dados

| Fonte | Dado | Script | URL / Pacote |
|---|---|---|---|
| IBGE | Shapefile municípios | `01_geo_nordeste.R` | `geobr::read_municipality()` |
| SUDENE | Delimitação do semi-árido | `02_semiarido_sudene.R` | `geobr::read_semiarid()` + Resoluções CONDEL |
| ANEEL | SIGA — empreendimentos de geração | `03_eolicas_aneel.R` | [dados.gov.br](https://dados.gov.br/dados/conjuntos-dados/siga-sistema-de-informacoes-de-geracao-da-aneel) |
| DataSUS / SIM | Óbitos (CID O00-O08, RN) | `04_saude_datasus.R` | `microdatasus::fetch_datasus()` |
| DataSUS / SINASC | Nascidos vivos, gravidez adolescente | `04_saude_datasus.R` | `microdatasus::fetch_datasus()` |
| DataSUS / SIH | Internações (CID O00-O08) | `04_saude_datasus.R` | `microdatasus::fetch_datasus()` |
| IBGE / CNJ | Registro civil — paternidade | `05_registro_civil.R` | [portaldetransparencia.registrocivil.org.br](https://portaldetransparencia.registrocivil.org.br) |
| MTE / RAIS | Vínculos empregatícios | `06_rais.R` | `basedosdados` (BigQuery) |
| IPEA / IBGE | PIB, IDH, população | `07_controles_ipea.R` | `ipeadatar` + IBGE API |

---

## Estimadores

| Estimador | Referência | Pacote R |
|---|---|---|
| TWFE clássico | — | `fixest` |
| Callaway & Sant'Anna (2021) | DOI: 10.1016/j.jeconom.2020.12.001 | `did` |
| Sun & Abraham (2021) | DOI: 10.1016/j.jeconom.2020.09.006 | `fixest` (sunab) |
| Borusyak, Jaravel & Spiess (2024) | DOI: 10.1093/restud/rdae007 | `didimputation` |
| Honest DiD (sensitivity) | Rambachan & Roth (2023) | `HonestDiD` |

---

## Convenções de Nomenclatura

- Municípios identificados pelo **código IBGE de 7 dígitos** (`cod_mun`)
- Ano em formato inteiro (`ano`)
- Variáveis de resultado com prefixo `y_`
- Variáveis de controle com prefixo `x_`
- Variáveis de tratamento com prefixo `d_`
- Indicadores binários com sufixo `_dum`

---

## Citação

> [Autor(es)]. (*em preparação*). *Eólicas e Filhos: Efeitos de Parques de Energia Eólica sobre Resultados Reprodutivos e Familiares no Nordeste Brasileiro*. Working Paper.

---

## Licença

Código sob licença [MIT](LICENSE). Dados públicos sujeitos às licenças de cada fonte.
