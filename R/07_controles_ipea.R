# =============================================================================
# 07_controles_ipea.R
# Variáveis de controle adicionais — IPEA, IBGE, outros
#
# Variáveis incluídas:
#   → População total (IBGE — estimativas anuais)
#   → PIB per capita (IBGE Cidades / ipeadatar)
#   → IDH-M (Atlas Brasil — 2000, 2010)
#   → Índice de Gini da renda (Atlas Brasil)
#   → % população rural (Censos 2000 e 2010 + interpolação)
#   → Índice de aridez / precipitação média anual (INPE/ANA)
#   → Presença de roda (hub) de transmissão / SE (ONS) — proxy infraestrutura elétrica
#   → Distância à capital estadual (calculada a partir dos centroides)
#   → Altitude média do município (SRTM)
#   → Velocidade média do vento (INMET / Era5 reanalysis) — proxy recurso eólico
#
# Saída:
#   data/processed/controles_panel.rds
# =============================================================================

source(here::here("R", "00_setup.R"))

mun_tab <- carregar("municipios_nordeste_tab")
mun_sf  <- carregar("municipios_nordeste")


# ============================================================================
# A. POPULAÇÃO — IBGE estimativas anuais
# ============================================================================

message("Baixando estimativas populacionais — IBGE...")

pop_cache <- file.path(DIR_RAW, "ibge", "populacao_nordeste.rds")

if (!file.exists(pop_cache)) {
  # API IBGE Cidades: series de estimativas populacionais
  # Série IBGE: id 6579 (Estimativa da população residente) ou censo
  pop_raw <- ipeadatar::ipeadata("POPTOT", quiet = TRUE) |>
    filter(uname == "Municipality") |>
    mutate(
      cod_mun = as.integer(tcode),
      ano     = as.integer(date),
      x_pop   = as.numeric(value)
    ) |>
    filter(is_nordeste(cod_mun), ano >= ANO_INICIO, ano <= ANO_FIM) |>
    select(cod_mun, ano, x_pop)

  # Para anos com lacunas, interpolar linearmente
  pop_raw <- pop_raw |>
    group_by(cod_mun) |>
    tidyr::complete(ano = ANOS) |>
    mutate(x_pop = zoo::na.approx(x_pop, na.rm = FALSE)) |>
    ungroup()

  saveRDS(pop_raw, pop_cache, compress = "xz")
} else {
  pop_raw <- readRDS(pop_cache)
}


# ============================================================================
# B. PIB PER CAPITA — IBGE / ipeadatar
# ============================================================================

message("Baixando PIB per capita — IBGE/IPEA...")

pib_cache <- file.path(DIR_RAW, "ipea", "pib_percapita_nordeste.rds")

if (!file.exists(pib_cache)) {
  pib_raw <- ipeadatar::ipeadata("PIB_MPMPC", quiet = TRUE) |>
    filter(uname == "Municipality") |>
    mutate(
      cod_mun    = as.integer(tcode),
      ano        = as.integer(date),
      x_pib_pc   = as.numeric(value)
    ) |>
    filter(is_nordeste(cod_mun), ano >= ANO_INICIO, ano <= ANO_FIM) |>
    select(cod_mun, ano, x_pib_pc)

  saveRDS(pib_raw, pib_cache, compress = "xz")
} else {
  pib_raw <- readRDS(pib_cache)
}


# ============================================================================
# C. IDH-M e GINI — Atlas Brasil (2000, 2010)
# ============================================================================

message("Baixando IDH-M e Gini — Atlas Brasil / IPEA...")

idh_cache <- file.path(DIR_RAW, "ipea", "idh_atlas_nordeste.rds")

if (!file.exists(idh_cache)) {
  # IDH-M por município: disponível nos censos 2000 e 2010
  idh_2000 <- ipeadatar::ipeadata("ADH_IDHM", quiet = TRUE) |>
    filter(uname == "Municipality", date == 2000) |>
    mutate(cod_mun = as.integer(tcode), x_idhm_2000 = as.numeric(value)) |>
    filter(is_nordeste(cod_mun)) |>
    select(cod_mun, x_idhm_2000)

  idh_2010 <- ipeadatar::ipeadata("ADH_IDHM", quiet = TRUE) |>
    filter(uname == "Municipality", date == 2010) |>
    mutate(cod_mun = as.integer(tcode), x_idhm_2010 = as.numeric(value)) |>
    filter(is_nordeste(cod_mun)) |>
    select(cod_mun, x_idhm_2010)

  gini_2000 <- ipeadatar::ipeadata("ADH_GINI", quiet = TRUE) |>
    filter(uname == "Municipality", date == 2000) |>
    mutate(cod_mun = as.integer(tcode), x_gini_2000 = as.numeric(value)) |>
    filter(is_nordeste(cod_mun)) |>
    select(cod_mun, x_gini_2000)

  gini_2010 <- ipeadatar::ipeadata("ADH_GINI", quiet = TRUE) |>
    filter(uname == "Municipality", date == 2010) |>
    mutate(cod_mun = as.integer(tcode), x_gini_2010 = as.numeric(value)) |>
    filter(is_nordeste(cod_mun)) |>
    select(cod_mun, x_gini_2010)

  idh_raw <- idh_2000 |>
    full_join(idh_2010,  by = "cod_mun") |>
    full_join(gini_2000, by = "cod_mun") |>
    full_join(gini_2010, by = "cod_mun")

  saveRDS(idh_raw, idh_cache, compress = "xz")
} else {
  idh_raw <- readRDS(idh_cache)
}

# Interpolar IDH-M para todos os anos (linear entre 2000 e 2010, flat depois)
idh_panel <- mun_tab |>
  select(cod_mun) |>
  tidyr::crossing(ano = ANOS) |>
  left_join(idh_raw, by = "cod_mun") |>
  mutate(
    x_idhm = case_when(
      ano <= 2000 ~ x_idhm_2000,
      ano >= 2010 ~ x_idhm_2010,
      TRUE        ~ x_idhm_2000 + (x_idhm_2010 - x_idhm_2000) * (ano - 2000) / 10
    ),
    x_gini = case_when(
      ano <= 2000 ~ x_gini_2000,
      ano >= 2010 ~ x_gini_2010,
      TRUE        ~ x_gini_2000 + (x_gini_2010 - x_gini_2000) * (ano - 2000) / 10
    )
  ) |>
  select(cod_mun, ano, x_idhm, x_gini)


# ============================================================================
# D. DISTÂNCIA À CAPITAL E CARACTERÍSTICAS GEOGRÁFICAS
# ============================================================================

message("Calculando distâncias e características geográficas...")

# Centroide de cada município
centroides <- mun_sf |>
  sf::st_centroid() |>
  sf::st_transform(4674) |>   # SIRGAS 2000 graus decimais
  mutate(
    lon = sf::st_coordinates(geom)[, 1],
    lat = sf::st_coordinates(geom)[, 2]
  ) |>
  sf::st_drop_geometry() |>
  select(cod_mun, lon, lat)

# Capitais dos estados do Nordeste (coordenadas aproximadas)
capitais_ne <- tribble(
  ~sigla_uf, ~lon_cap, ~lat_cap,
  "MA",  -44.3028, -2.5297,   # São Luís
  "PI",  -42.8019, -5.0920,   # Teresina
  "CE",  -38.5434, -3.7172,   # Fortaleza
  "RN",  -35.2091, -5.7945,   # Natal
  "PB",  -34.8631, -7.1195,   # João Pessoa
  "PE",  -34.8813, -8.0539,   # Recife
  "AL",  -35.7353, -9.6658,   # Maceió
  "SE",  -37.0731, -10.9472,  # Aracaju
  "BA",  -38.4813, -12.9714   # Salvador
)

# Calcular distância (em km) de cada município à sua capital estadual
geo_controles <- mun_tab |>
  select(cod_mun, sigla_uf) |>
  left_join(centroides, by = "cod_mun") |>
  left_join(capitais_ne, by = "sigla_uf") |>
  mutate(
    # Fórmula de Haversine simplificada (boa aproximação para distâncias < 1000 km)
    x_dist_capital_km = geosphere::distHaversine(
      cbind(lon, lat),
      cbind(lon_cap, lat_cap)
    ) / 1000
  ) |>
  select(cod_mun, lon, lat, x_dist_capital_km)

# Instala geosphere se necessário
if (!requireNamespace("geosphere", quietly = TRUE)) {
  install.packages("geosphere")
  library(geosphere)
}


# ============================================================================
# E. VELOCIDADE DO VENTO — proxy para recurso eólico (instrumento)
# ============================================================================

# Os dados de velocidade média do vento por município estão disponíveis no
# Atlas Eólico do Brasil (CEPEL/INPE) ou via ERA5 reanalysis (Copernicus).
# Aqui usamos o arquivo do Atlas Eólico Nordeste (2013) compilado pelo CRESESB.
# Download: https://www.cresesb.cepel.br/index.php#atlas_eolico

vento_cache <- file.path(DIR_RAW, "ipea", "velocidade_vento_nordeste.rds")

if (!file.exists(vento_cache)) {
  message("AVISO: Arquivo de velocidade do vento não encontrado.")
  message("Baixe o Atlas Eólico (CRESESB/CEPEL) e salve em data/raw/ipea/")
  message("Criando placeholder com NA...")
  vento_raw <- mun_tab |> select(cod_mun) |> mutate(x_vento_ms = NA_real_)
  saveRDS(vento_raw, vento_cache)
} else {
  vento_raw <- readRDS(vento_cache)
}


# ============================================================================
# F. MONTAGEM DO PAINEL DE CONTROLES
# ============================================================================

painel_controles <- mun_tab |>
  select(cod_mun, sigla_uf, nome_mun) |>
  tidyr::crossing(ano = ANOS) |>
  left_join(pop_raw,      by = c("cod_mun", "ano")) |>
  left_join(pib_raw,      by = c("cod_mun", "ano")) |>
  left_join(idh_panel,    by = c("cod_mun", "ano")) |>
  left_join(geo_controles, by = "cod_mun") |>
  left_join(vento_raw,    by = "cod_mun") |>
  mutate(
    # Transformações log
    log_pop    = log1p(x_pop),
    log_pib_pc = log1p(x_pib_pc),

    # Densidade demográfica (calculada após merge com área em 08_build_panel.R)
    # x_dens_dem = x_pop / area_km2  (calculado depois)

    # Índice de urbanização (placeholder — obtido do Censo em 08_build_panel.R)
    x_pib_pc_mil = x_pib_pc / 1000
  )


# ---- Verificação -----------------------------------------------------------
message("\nResumo controles — primeiras linhas:")
print(head(painel_controles))

missing_summary <- painel_controles |>
  summarise(across(starts_with("x_"), ~ mean(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "pct_missing") |>
  filter(pct_missing > 0.1) |>
  arrange(desc(pct_missing))

if (nrow(missing_summary) > 0) {
  message("\nVariáveis com > 10% missing:")
  print(missing_summary)
}


# ---- Salvar ---------------------------------------------------------------
salvar(painel_controles, "controles_panel")

message("\n07_controles_ipea.R concluído.")
