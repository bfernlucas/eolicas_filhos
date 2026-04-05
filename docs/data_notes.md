# Notas de Acesso aos Dados

Este documento descreve os passos exatos para obter cada fonte de dados utilizada no projeto. Scripts de download automático estão nos arquivos `R/0X_*.R`; aqui documentamos o que requer etapas manuais.

---

## 1. IBGE — Shapefiles de Municípios

**Fonte programática:** pacote `geobr` (R).  
**Comando:** `geobr::read_municipality("all", year = 2020)`  
**Nenhum download manual necessário.**

---

## 2. SUDENE — Delimitação do Semi-Árido

### 2a. Delimitação original (2005)
- **Portaria Interministerial Nº 1, de 09/03/2005** (publicada no DOU em 10/03/2005)
- Lista de 1.135 municípios disponível no portal da SUDENE:  
  `https://www.gov.br/sudene/pt-br/assuntos/fne/semiarido`
- Baixar o arquivo de municípios do semi-árido (Excel/CSV) e salvar em:  
  `data/raw/sudene/semiarido_2005_municipios.csv`
- Colunas esperadas: `cod_ibge` (7 dígitos), `municipio`, `uf`

### 2b. Resoluções CONDEL que alteraram a delimitação

| Resolução | Data | Municípios | Nota |
|---|---|---|---|
| CONDEL nº 107/2017 | 27/07/2017 | +54 (PI:36, CE:15, BA:3) | geobr snapshot 2017 inclui esta e a 115 |
| CONDEL nº 115/2017 | 23/11/2017 | +73 | idem |
| CONDEL nº 150/2021 | 13/12/2021 | +215, -50 (inclui MG e ES) | geobr snapshot 2021 |

**Notas importantes:**
- Res. 128/2018 e Res. 173/2023 **não** alteram a delimitação geográfica do semiárido.
- Res. 150 é de **2021** (não 2019) e inclui pela primeira vez municípios de MG e ES.
- O script `R/02_semiarido_sudene.R` usa `geobr::read_semiarid(year = 2005/2017/2021)`
  diretamente — nenhuma lista manual de municípios é necessária.

**Como o painel reconstrói a variação temporal:**

```r
# Municípios no snapshot 2005  → ano_entrada = 2005
# Presentes em 2017 mas não 2005 → ano_entrada = 2017  (Res. 107 + 115)
# Presentes em 2021 mas não 2017 → ano_entrada = 2021  (Res. 150)
# Removidos pela Res. 150        → d_semiarido = 0 a partir de 2021
geobr::read_semiarid(year = 2005)  # 1.135 municípios
geobr::read_semiarid(year = 2017)  # 1.262 municípios
geobr::read_semiarid(year = 2021)  # 1.477 municípios
```

---

## 3. ANEEL — SIGA (Sistema de Informações de Geração)

**Download automático** pelo script `R/03_eolicas_aneel.R`.

**URL alternativa (caso o script falhe):**
- Portal dados abertos: `https://dadosabertos.aneel.gov.br/dataset/siga-sistema-de-informacoes-de-geracao-da-aneel`
- Arquivo direto: CSV separado por ponto e vírgula, encoding Latin-1
- Salvar em: `data/raw/aneel/siga_empreendimentos.csv`

**Variáveis-chave:**
- `CodMunicIbge`: código IBGE de 7 dígitos
- `TipGeracao`: tipo de geração — filtrar `"EOL"` (eólica)
- `DscFaseUsina`: fase — filtrar `"Operação"`
- `DatIniOpe`: data de início de operação (DD/MM/YYYY)
- `MdaPotenciaFiscalizadaKw`: capacidade instalada (kW)

---

## 4. DataSUS — SIM, SINASC, SIH

**Download automático** via pacote `microdatasus` (`R/04_saude_datasus.R`).

**Instalação:**
```r
remotes::install_github("rfsaldanha/microdatasus")
```

**Nota sobre tempo de download:**
- SIM: ~30–90 min para todos os estados do NE (2000–2022)
- SINASC: ~20–60 min
- SIH: ~60–120 min (maior volume)

**Arquivos cacheados em:** `data/raw/datasus/{sim,sinasc,sih}/`

**Download manual alternativo (FTP):**
- `ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/`
- `ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/`
- `ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/`

---

## 5. Portal da Transparência do Registro Civil (CNJ / Arpen-Brasil)

### Opção A — Base dos Dados (BigQuery) [**recomendada**]

Dataset: `basedosdados.br_arpen_brasil.registro_civil_nascimentos`

**Configuração:**
1. Criar projeto no Google Cloud Platform: `https://console.cloud.google.com/`
2. Habilitar a API do BigQuery
3. No R:
   ```r
   basedosdados::bd_auth()   # Autenticar uma vez
   options(basedosdados_project_id = "SEU_PROJETO_GCP")
   ```
4. O script `R/05_registro_civil.R` executa a query automaticamente

**Documentação do dataset:** `https://basedosdados.org/dataset/c70750d8-e11d-462d-bd8f-266f34c08904`

### Opção B — Download manual

1. Acessar: `https://transparencia.registrocivil.org.br/painel-registral/nascimentos`
2. Selecionar: "Reconhecimento de Paternidade" → "Dados Abertos"
3. Baixar por ano (2000–2022), formato CSV
4. Salvar em: `data/raw/registro_civil/paternidade_AAAA.csv`

**Colunas esperadas:** código IBGE, município, UF, total de nascimentos, nascimentos com pai, nascimentos sem pai, reconhecimentos voluntários.

---

## 6. RAIS — Relação Anual de Informações Sociais

### Opção A — Base dos Dados (BigQuery) [**recomendada**]

Dataset: `basedosdados.br_me_rais.microdados_vinculos` e `microdados_estabelecimentos`

**Configuração:** mesma do item 5 acima.

**Documentação:** `https://basedosdados.org/dataset/3e7c4d58-96ba-448e-b053-d385a829ef00`

**Atenção:** A query de vínculos pode gerar cobranças no BigQuery para volumes acima de 1 TB processados. Configure alertas de custo no GCP antes de executar. A query do script `R/06_rais.R` está otimizada para minimizar o processamento.

### Opção B — FTP do MTE

- `ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/`
- Arquivos por ano, separados por UF
- Volumosos (~2–10 GB por ano por UF)
- Parsing via `data.table::fread()` com `encoding = "Latin-1"`

---

## 7. IPEA — Dados Macroeconômicos

**Download automático** via pacote `ipeadatar` (`R/07_controles_ipea.R`).

**Séries utilizadas:**
- `POPTOT` — Estimativa da população total por município
- `PIB_MPMPC` — PIB per capita municipal
- `ADH_IDHM` — IDH-M (Censos 2000 e 2010)
- `ADH_GINI` — Índice de Gini (Censos 2000 e 2010)

---

## 8. Atlas Eólico — Velocidade do Vento

**Fonte:** CRESESB/CEPEL — Atlas do Potencial Eólico Brasileiro  
**URL:** `https://www.cresesb.cepel.br/index.php#atlas_eolico`

**Passos:**
1. Baixar o shapefile ou raster do atlas eólico
2. Extrair a velocidade média anual no centroide de cada município (ferramentas: `terra::extract()` ou `exactextractr::exact_extract()`)
3. Salvar como: `data/raw/ipea/velocidade_vento_nordeste.rds` com colunas `cod_mun` e `x_vento_ms`

**Alternativa:** ERA5 Reanalysis (Copernicus Climate Change Service) — dados de vento em grade regular 0.25°, disponíveis via API `ecmwfr`.

---

## Checklist de Download

Antes de executar `tar_make()`, verifique:

- [ ] `data/raw/aneel/siga_empreendimentos.csv` — baixado pelo script
- [ ] `data/raw/sudene/semiarido_2005_municipios.csv` — **download manual**
- [ ] `data/raw/sudene/resolucao_107_2017.csv` — **download manual**
- [ ] `data/raw/sudene/resolucao_115_2017.csv` — **download manual**
- [ ] `data/raw/sudene/resolucao_128_2018.csv` — **download manual**
- [ ] `data/raw/sudene/resolucao_150_2019.csv` — **download manual**
- [ ] `data/raw/sudene/resolucao_173_2022.csv` — **download manual**
- [ ] DataSUS (SIM, SINASC, SIH) — baixado pelos scripts (1ª execução lenta)
- [ ] Registro Civil — baixado pelo script (requer BigQuery) ou manual
- [ ] RAIS — baixado pelo script (requer BigQuery) ou manual
- [ ] `data/raw/ipea/velocidade_vento_nordeste.rds` — **download manual** (opcional)
- [ ] Google Cloud project ID configurado: `options(basedosdados_project_id = "...")`
