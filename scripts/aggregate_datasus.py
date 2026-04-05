#!/usr/bin/env python3
"""
aggregate_datasus.py
====================
Agrega os microdados DataSUS (SIM, SINASC, SIH) — baixados por download_data.py
— em arquivos Parquet município-ano prontos para uso nos scripts R.

Execute DEPOIS de download_data.py:

    python3 scripts/aggregate_datasus.py

Saídas em data/processed/:
    sim_agregado_ne.parquet     — óbitos por CID (O00-O08 + neonatal)
    sinasc_agregado_ne.parquet  — nascidos vivos + gravidez adolescente
    sih_agregado_ne.parquet     — internações CID O00-O08
"""

import os
import sys
import logging
import re
from pathlib import Path

import pandas as pd

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-8s  %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger(__name__)

ROOT      = Path(__file__).resolve().parent.parent
RAW       = ROOT / "data" / "raw"
PROCESSED = ROOT / "data" / "processed"
PROCESSED.mkdir(parents=True, exist_ok=True)

SIM_DIR    = RAW / "datasus" / "sim"
SINASC_DIR = RAW / "datasus" / "sinasc"
SIH_DIR    = RAW / "datasus" / "sih"

ANOS_PANEL   = range(2000, 2023)
ESTADOS_NE   = ["MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"]
CODIGOS_NE   = {21, 22, 23, 24, 25, 26, 27, 28, 29}
CID_ABORTO   = {f"O0{i}" for i in range(9)}   # O00 a O08
IDADE_ADOL   = (10, 19)
NEONATAL_MAX = 27   # dias


def cod_mun_7(s) -> str:
    """Retorna os 7 primeiros dígitos do código IBGE."""
    return str(s)[:7] if pd.notna(s) else ""


def is_nordeste(cod_mun: str) -> bool:
    """True se o município pertence ao NE."""
    try:
        return int(cod_mun[:2]) in CODIGOS_NE
    except (ValueError, TypeError):
        return False


# ════════════════════════════════════════════════════════════════════════════
# SIM — Óbitos
# ════════════════════════════════════════════════════════════════════════════

def agregar_sim():
    log.info("=== SIM — Agregando óbitos ===")

    arquivos = sorted(SIM_DIR.glob("DO*.parquet"))
    if not arquivos:
        log.warning("Nenhum arquivo SIM encontrado em %s", SIM_DIR)
        return

    resultados = []
    for f in arquivos:
        try:
            df = pd.read_parquet(f)
            df.columns = df.columns.str.lower()

            # Coluna de município de residência
            mun_col = next((c for c in df.columns
                            if "codmunres" in c or "munres" in c), None)
            ano_col = next((c for c in df.columns
                            if "ano" in c or "dtobito" in c), None)
            cid_col = next((c for c in df.columns
                            if "causabas" in c), None)
            idade_col = next((c for c in df.columns if "idade" in c), None)

            if not all([mun_col, ano_col, cid_col]):
                log.warning("Colunas ausentes em %s", f.name)
                continue

            df["cod_mun"] = df[mun_col].apply(cod_mun_7)
            df = df[df["cod_mun"].apply(is_nordeste)]

            # Extrair ano
            if "dtobito" in ano_col:
                df["ano"] = pd.to_datetime(df[ano_col], errors="coerce").dt.year
            else:
                df["ano"] = pd.to_numeric(df[ano_col], errors="coerce")

            df = df[df["ano"].between(2000, 2022)]
            df["cid3"] = df[cid_col].astype(str).str[:3].str.upper()

            # Óbitos abortivos (O00-O08)
            df_ab = df[df["cid3"].isin(CID_ABORTO)]
            agg_ab = (df_ab.groupby(["cod_mun", "ano"])
                      .size().reset_index(name="y_obito_aborto"))

            # Óbitos neonatais (0-27 dias)
            if idade_col:
                # Encoding SIM: 2xxx = dias (3 últimos dígitos), 1xxx = horas
                df["idade_dias"] = df[idade_col].astype(str).apply(
                    lambda x: int(x[1:]) if x.startswith("2") else
                              (0 if x.startswith("1") else None)
                )
                df_rn = df[df["idade_dias"].notna() & (df["idade_dias"] <= 27)]
                agg_rn = (df_rn.groupby(["cod_mun", "ano"])
                          .size().reset_index(name="y_obito_rn"))
            else:
                agg_rn = pd.DataFrame(columns=["cod_mun", "ano", "y_obito_rn"])

            # Óbitos maternos (O00-O99)
            df_mat = df[df["cid3"].str.startswith("O", na=False)]
            agg_mat = (df_mat.groupby(["cod_mun", "ano"])
                       .size().reset_index(name="y_obito_materno"))

            # Juntar
            agg = (agg_ab
                   .merge(agg_rn,  on=["cod_mun", "ano"], how="outer")
                   .merge(agg_mat, on=["cod_mun", "ano"], how="outer"))
            resultados.append(agg)
            log.info("  ✓ %s: %d registros", f.name, len(agg))

        except Exception as e:
            log.error("  Erro em %s: %s", f.name, e)

    if not resultados:
        log.warning("Sem resultados SIM — verifique os arquivos em %s", SIM_DIR)
        return

    df_final = pd.concat(resultados, ignore_index=True)
    df_final = (df_final.groupby(["cod_mun", "ano"])
                .sum(numeric_only=True).reset_index())
    df_final.fillna(0, inplace=True)
    df_final[["y_obito_aborto", "y_obito_rn", "y_obito_materno"]] = \
        df_final[["y_obito_aborto", "y_obito_rn", "y_obito_materno"]].astype(int)

    dest = PROCESSED / "sim_agregado_ne.parquet"
    df_final.to_parquet(dest, index=False, compression="zstd")
    log.info("✓ SIM salvo: %s (%d linhas)", dest, len(df_final))


# ════════════════════════════════════════════════════════════════════════════
# SINASC — Nascidos Vivos
# ════════════════════════════════════════════════════════════════════════════

def agregar_sinasc():
    log.info("=== SINASC — Agregando nascidos vivos ===")

    arquivos = sorted(SINASC_DIR.glob("DN*.parquet"))
    if not arquivos:
        log.warning("Nenhum arquivo SINASC em %s", SINASC_DIR)
        return

    resultados = []
    for f in arquivos:
        try:
            df = pd.read_parquet(f)
            df.columns = df.columns.str.lower()

            mun_col   = next((c for c in df.columns if "codmunres" in c), None)
            data_col  = next((c for c in df.columns if "dtnasc" in c), None)
            idade_col = next((c for c in df.columns if "idademae" in c), None)

            if not all([mun_col, data_col]):
                log.warning("Colunas ausentes em %s", f.name)
                continue

            df["cod_mun"] = df[mun_col].apply(cod_mun_7)
            df = df[df["cod_mun"].apply(is_nordeste)]
            df["ano"] = pd.to_datetime(df[data_col], format="%d%m%Y",
                                        errors="coerce").dt.year
            df = df[df["ano"].between(2000, 2022)]

            # Total de nascidos vivos
            agg_total = (df.groupby(["cod_mun", "ano"])
                         .size().reset_index(name="y_nascidos_vivos"))

            # Gravidez adolescente
            if idade_col:
                df["idade_mae"] = pd.to_numeric(df[idade_col], errors="coerce")
                df_adol = df[df["idade_mae"].between(IDADE_ADOL[0], IDADE_ADOL[1])]
                df_10_14 = df[df["idade_mae"].between(10, 14)]
                df_15_19 = df[df["idade_mae"].between(15, 19)]

                agg_adol = (df_adol.groupby(["cod_mun", "ano"])
                            .size().reset_index(name="y_nasc_adol"))
                agg_10_14 = (df_10_14.groupby(["cod_mun", "ano"])
                             .size().reset_index(name="y_nasc_adol_10_14"))
                agg_15_19 = (df_15_19.groupby(["cod_mun", "ano"])
                             .size().reset_index(name="y_nasc_adol_15_19"))

                agg = (agg_total
                       .merge(agg_adol,  on=["cod_mun", "ano"], how="left")
                       .merge(agg_10_14, on=["cod_mun", "ano"], how="left")
                       .merge(agg_15_19, on=["cod_mun", "ano"], how="left"))
            else:
                agg = agg_total

            resultados.append(agg)
            log.info("  ✓ %s: %d registros", f.name, len(agg))

        except Exception as e:
            log.error("  Erro em %s: %s", f.name, e)

    if not resultados:
        log.warning("Sem resultados SINASC")
        return

    df_final = pd.concat(resultados, ignore_index=True)
    df_final = (df_final.groupby(["cod_mun", "ano"])
                .sum(numeric_only=True).reset_index())
    df_final.fillna(0, inplace=True)
    for c in ["y_nascidos_vivos", "y_nasc_adol", "y_nasc_adol_10_14", "y_nasc_adol_15_19"]:
        if c in df_final.columns:
            df_final[c] = df_final[c].astype(int)

    dest = PROCESSED / "sinasc_agregado_ne.parquet"
    df_final.to_parquet(dest, index=False, compression="zstd")
    log.info("✓ SINASC salvo: %s (%d linhas)", dest, len(df_final))


# ════════════════════════════════════════════════════════════════════════════
# SIH — Internações
# ════════════════════════════════════════════════════════════════════════════

def agregar_sih():
    log.info("=== SIH — Agregando internações CID O00-O08 ===")

    arquivos = sorted(SIH_DIR.glob("RD*aborto.parquet"))
    if not arquivos:
        log.warning("Nenhum arquivo SIH em %s", SIH_DIR)
        return

    resultados = []
    for f in arquivos:
        try:
            df = pd.read_parquet(f)
            df.columns = df.columns.str.lower()

            mun_col = next((c for c in df.columns
                            if "munres" in c or "munic_res" in c), None)
            ano_col = next((c for c in df.columns
                            if "ano" in c or "dt_inter" in c), None)
            cid_col = next((c for c in df.columns
                            if "diag_princ" in c or "diag" in c), None)

            if not all([mun_col, ano_col, cid_col]):
                log.warning("Colunas ausentes em %s", f.name)
                continue

            df["cod_mun"] = df[mun_col].apply(cod_mun_7)
            df = df[df["cod_mun"].apply(is_nordeste)]

            if "dt_inter" in ano_col:
                df["ano"] = pd.to_datetime(df[ano_col], errors="coerce").dt.year
            else:
                df["ano"] = pd.to_numeric(df[ano_col], errors="coerce")

            df = df[df["ano"].between(2000, 2022)]
            df["cid3"] = df[cid_col].astype(str).str[:3].str.upper()
            df = df[df["cid3"].isin(CID_ABORTO)]

            agg = (df.groupby(["cod_mun", "ano"])
                   .agg(
                       y_intern_aborto=("cid3", "count"),
                       y_intern_o03=("cid3", lambda x: (x == "O03").sum()),
                       y_intern_o04=("cid3", lambda x: (x == "O04").sum()),
                       y_intern_o07=("cid3", lambda x: (x == "O07").sum()),
                   ).reset_index())

            resultados.append(agg)
            log.info("  ✓ %s: %d registros", f.name, len(agg))

        except Exception as e:
            log.error("  Erro em %s: %s", f.name, e)

    if not resultados:
        log.warning("Sem resultados SIH")
        return

    df_final = pd.concat(resultados, ignore_index=True)
    df_final = (df_final.groupby(["cod_mun", "ano"])
                .sum(numeric_only=True).reset_index())
    df_final.fillna(0, inplace=True)

    dest = PROCESSED / "sih_agregado_ne.parquet"
    df_final.to_parquet(dest, index=False, compression="zstd")
    log.info("✓ SIH salvo: %s (%d linhas)", dest, len(df_final))


# ════════════════════════════════════════════════════════════════════════════
# PONTO DE ENTRADA
# ════════════════════════════════════════════════════════════════════════════

if __name__ == "__main__":
    log.info("Agregando microdados DataSUS — Nordeste 2000-2022")
    agregar_sim()
    agregar_sinasc()
    agregar_sih()
    log.info("Concluído. Arquivos em %s", PROCESSED)
