# Codebook — Painel Eólicas e Filhos

**Arquivo analítico principal:** `data/final/painel_final.rds`  
**Unidade de observação:** município × ano  
**Período:** 2000–2022  
**Abrangência:** municípios do Nordeste brasileiro (9 estados, ≈ 1.794 municípios)

---

## Identificadores

| Variável | Tipo | Descrição | Fonte |
|---|---|---|---|
| `cod_mun` | integer | Código IBGE do município (7 dígitos) | IBGE |
| `cod_uf` | integer | Código IBGE da UF (2 dígitos) | IBGE |
| `sigla_uf` | character | Sigla da UF (ex: "CE", "RN") | IBGE |
| `nome_mun` | character | Nome do município | IBGE |
| `ano` | integer | Ano calendário | — |

---

## Variáveis de Tratamento

| Variável | Tipo | Descrição | Fonte |
|---|---|---|---|
| `d_eolica_dum` | integer (0/1) | 1 se o município possui ≥ 1 usina eólica em operação no ano `t` | ANEEL/SIGA |
| `ano_primeiro_eolica` | integer | Ano de início de operação da primeira usina eólica no município (NA = nunca tratado) | ANEEL/SIGA |
| `g_cs` | integer | Coorte de tratamento para Callaway & Sant'Anna: primeiro ano eólica, ou 0 para nunca tratados | ANEEL/SIGA |
| `g_sa` | integer | Coorte de tratamento para Sun & Abraham: primeiro ano eólica, ou NA para nunca tratados | ANEEL/SIGA |
| `n_usinas_operando` | integer | Número de usinas eólicas em operação no ano `t` | ANEEL/SIGA |
| `cap_instalada_mw` | numeric | Capacidade instalada acumulada (MW) no ano `t` | ANEEL/SIGA |
| `log_cap_mw` | numeric | log(cap_instalada_mw + 1) — para especificações de intensidade | ANEEL/SIGA |
| `d_post` | integer (0/1) | 1 se `ano >= ano_primeiro_eolica` | ANEEL/SIGA |
| `rel_time` | integer | Tempo relativo ao tratamento: `ano - ano_primeiro_eolica` (NA para nunca tratados) | — |
| `never_treated` | integer (0/1) | 1 se o município nunca recebeu eólica no período amostral | ANEEL/SIGA |

---

## Covariável Temporal: Semi-Árido SUDENE

| Variável | Tipo | Descrição | Fonte |
|---|---|---|---|
| `d_semiarido` | integer (0/1) | 1 se o município pertencia ao semi-árido SUDENE no ano `t` | SUDENE |
| `ano_entrada_semiarido` | integer | Ano em que o município foi incluído na delimitação semi-árida (NA = nunca incluído) | SUDENE |

**Histórico de resoluções:**
- 2005: Portaria Interministerial nº 1 (delimitação original — 1.135 municípios)
- 2017: Resolução CONDEL/SUDENE nº 107 (+54 municípios) e nº 115 (+73 municípios)
- 2018: Resolução CONDEL/SUDENE nº 128 (+15 municípios)
- 2019: Resolução CONDEL/SUDENE nº 150 (+9 municípios)
- 2022: Resolução CONDEL/SUDENE nº 173 (+7 municípios)

---

## Variáveis de Resultado (prefixo `y_` = contagens; `tx_` = taxas; `pct_` = percentuais)

### Saúde Materno-Infantil — DataSUS

| Variável | Tipo | Descrição | Fonte | Denominador |
|---|---|---|---|---|
| `y_nascidos_vivos` | integer | Total de nascidos vivos residentes no município | SINASC | — |
| `y_nasc_adol` | integer | Nascimentos de mães com 10–19 anos | SINASC | — |
| `y_nasc_adol_10_14` | integer | Nascimentos de mães com 10–14 anos | SINASC | — |
| `y_nasc_adol_15_19` | integer | Nascimentos de mães com 15–19 anos | SINASC | — |
| `pct_nasc_adol` | numeric | % de nascimentos de mães adolescentes (10–19) | SINASC | nascidos vivos |
| `tx_nasc_adol` | numeric | Taxa de gravidez adolescente (por 1.000 nascidos vivos) | SINASC | nascidos vivos |
| `tx_nasc_adol_w` | numeric | `tx_nasc_adol` winsorizada nos percentis 1% e 99% | SINASC | — |
| `y_obito_aborto` | integer | Óbitos com causa básica CID O00–O08 (desfechos abortivos) | SIM-DO | — |
| `tx_obito_aborto` | numeric | Taxa de óbito por CID O00–O08 (por 1.000 NV) | SIM-DO | nascidos vivos |
| `y_obito_rn` | integer | Óbitos de recém-nascidos (0–27 dias de vida) | SIM-DO | — |
| `tx_obito_rn` | numeric | Taxa de mortalidade neonatal (por 1.000 NV) | SIM-DO | nascidos vivos |
| `y_obito_materno` | integer | Óbitos com causa básica CID O00–O99 (causas maternas) | SIM-DO | — |
| `tx_obito_materno` | numeric | Taxa de mortalidade materna (por 1.000 NV) | SIM-DO | nascidos vivos |
| `y_intern_aborto` | integer | Internações hospitalares (AIH) com diagnóstico principal CID O00–O08 | SIH-RD | — |
| `y_intern_o03` | integer | Internações — CID O03 (aborto espontâneo) | SIH-RD | — |
| `y_intern_o04` | integer | Internações — CID O04 (aborto por razões médicas) | SIH-RD | — |
| `y_intern_o07` | integer | Internações — CID O07 (falha na tentativa de aborto) | SIH-RD | — |
| `tx_intern_aborto` | numeric | Taxa de internação por CID O00–O08 (por 1.000 NV) | SIH-RD | nascidos vivos |
| `tx_intern_aborto_w` | numeric | `tx_intern_aborto` winsorizada | SIH-RD | — |
| `x_prenatal_media` | numeric | Média de consultas de pré-natal no município-ano | SINASC | — |
| `x_prenatal_nenhum` | integer | Número de nascimentos com zero consultas de pré-natal | SINASC | — |

**Classificação CID-10 O00–O08:**

| CID | Descrição |
|---|---|
| O00 | Gravidez ectópica |
| O01 | Mola hidatiforme |
| O02 | Outros produtos anormais da concepção |
| O03 | Aborto espontâneo |
| O04 | Aborto por razões médicas / legais |
| O05 | Outros tipos de aborto |
| O06 | Aborto não especificado |
| O07 | Falha na tentativa de aborto |
| O08 | Complicações após aborto e gravidez ectópica/molar |

### Registro Civil — Paternidade

| Variável | Tipo | Descrição | Fonte |
|---|---|---|---|
| `y_nasc_rc` | integer | Total de nascimentos registrados no município | Reg. Civil / CNJ |
| `y_sem_pai` | integer | Nascimentos sem declaração do pai | Reg. Civil / CNJ |
| `y_reconhecimento` | integer | Reconhecimentos voluntários de paternidade | Reg. Civil / CNJ |
| `y_com_pai` | integer | Nascimentos com pai declarado | Reg. Civil / CNJ |
| `tx_sem_pai` | numeric | % de nascimentos sem pai declarado | Reg. Civil / CNJ |
| `tx_sem_pai_w` | numeric | `tx_sem_pai` winsorizada | — |
| `tx_reconhecimento` | numeric | % de reconhecimentos sobre nascimentos sem pai | Reg. Civil / CNJ |

### Emprego — RAIS

| Variável | Tipo | Descrição | Fonte |
|---|---|---|---|
| `n_vinculos` | integer | Total de vínculos empregatícios formais ativos (31/12) | RAIS/MTE |
| `n_mulheres` | integer | Vínculos de trabalhadoras mulheres | RAIS/MTE |
| `n_vinculos_energia` | integer | Vínculos em CNAE 3511 (geração de energia elétrica) | RAIS/MTE |
| `n_vinculos_construcao` | integer | Vínculos em CNAEs da construção civil | RAIS/MTE |
| `salario_medio` | numeric | Salário médio mensal (R$ correntes) | RAIS/MTE |
| `pct_mulheres` | numeric | % de mulheres no total de vínculos | RAIS/MTE |
| `log_vinculos` | numeric | log(n_vinculos + 1) | — |
| `log_vinculos_energia` | numeric | log(n_vinculos_energia + 1) — **variável de resultado** | — |
| `x_vinculos_pc` | numeric | Vínculos por 1.000 habitantes | — |
| `x_energia_pc` | numeric | Vínculos em energia por 1.000 habitantes | — |

---

## Variáveis de Controle (prefixo `x_`)

| Variável | Tipo | Descrição | Fonte |
|---|---|---|---|
| `x_pop` | numeric | Estimativa populacional total | IBGE |
| `x_pib_pc` | numeric | PIB per capita (R$ constantes 2010) | IBGE/IPEA |
| `x_pib_pc_mil` | numeric | PIB per capita em R$ mil | — |
| `x_idhm` | numeric | IDH Municipal (interpolado entre Censos 2000 e 2010) | Atlas Brasil |
| `x_gini` | numeric | Índice de Gini da renda (interpolado entre Censos) | Atlas Brasil |
| `x_dist_capital_km` | numeric | Distância à capital estadual (km, fórmula de Haversine) | IBGE/geobr |
| `x_vento_ms` | numeric | Velocidade média do vento (m/s) no município | CRESESB/CEPEL |
| `lon` | numeric | Longitude do centroide do município (graus decimais) | IBGE/geobr |
| `lat` | numeric | Latitude do centroide do município (graus decimais) | IBGE/geobr |
| `area_km2` | numeric | Área do município (km²) | IBGE/geobr |
| `x_dens_dem` | numeric | Densidade demográfica (hab/km²) | — |
| `log_pop` | numeric | log(x_pop + 1) | — |
| `log_pib_pc` | numeric | log(x_pib_pc + 1) | — |

---

## Indicadores Auxiliares

| Variável | Tipo | Descrição |
|---|---|---|
| `d_covid` | integer (0/1) | 1 para anos 2020 e 2021 (controle COVID) |
| `log_y_obito_aborto` | numeric | log(y_obito_aborto + 0.5) |
| `log_y_obito_rn` | numeric | log(y_obito_rn + 0.5) |
| `log_y_intern_aborto` | numeric | log(y_intern_aborto + 0.5) |
| `log_y_nasc_adol` | numeric | log(y_nasc_adol + 0.5) |
| `log_y_sem_pai` | numeric | log(y_sem_pai + 0.5) |

---

## Notas Metodológicas

### Sobre zeros nas variáveis de contagem
Municípios-anos sem registro nas bases DataSUS recebem valor **0** nas contagens (não NA), pois a ausência de registro de óbito ou internação representa genuinamente zero eventos. A exceção é quando o arquivo DataSUS da UF × ano não foi baixado com sucesso — nesses casos o registro permanece NA e é excluído da amostra de estimação.

### Sobre taxas
Todas as taxas que usam nascidos vivos como denominador substituem `y_nascidos_vivos = 0` por `1` para evitar divisão por zero. Municípios com menos de 30 nascimentos anuais são potencialmente ruidosos nas taxas — verificar sensibilidade excluindo esses municípios em análise de robustez.

### Sobre o painel balanceado
O painel é **fortemente balanceado**: todos os municípios do Nordeste presentes no shapefile IBGE 2020 aparecem em todos os anos 2000–2022. Municípios criados após 2000 por desmembramento têm dados imputados pelo município-pai no período anterior à criação (verificar em `data/processed/notas_municipios_novos.csv` se aplicável).

### Sobre winsorização
Variáveis com sufixo `_w` foram winsorizadas nos percentis 1% e 99% calculados **sobre toda a amostra** (não por estado ou ano), seguindo prática padrão em literatura de DiD com dados municipais brasileiros.

---

## Estrutura do Pipeline

```
01_geo_nordeste.R  →  municipios_nordeste.rds
02_semiarido_sudene.R  →  semiarido_panel.rds
03_eolicas_aneel.R  →  eolicas_panel.rds
04_saude_datasus.R  →  saude_panel.rds
05_registro_civil.R  →  registro_civil_panel.rds
06_rais.R  →  rais_panel.rds
07_controles_ipea.R  →  controles_panel.rds
08_build_panel.R  →  painel_final.rds ← ARQUIVO ANALÍTICO PRINCIPAL
09_descritivas.R  →  output/tables/ + output/figures/ + output/maps/
10_staggered_did.R  →  output/tables/ + output/figures/
11_robustez.R  →  output/tables/ + output/figures/
```
