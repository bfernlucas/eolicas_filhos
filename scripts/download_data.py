#!/usr/bin/env python3
"""
download_data.py
================
Script principal de download de dados — Eólicas e Filhos

Execute na sua máquina local com acesso irrestrito à internet:

    python3 scripts/download_data.py [--fonte FONTE] [--anos 2000-2022]

Fontes disponíveis (argumento --fonte):
    all             Baixar tudo (padrão)
    ibge            Shapefile de municípios (geobr/IPEA)
    aneel           SIGA ANEEL — parques eólicos
    sudene          Delimitação semi-árido (geobr/IPEA)
    datasus         SIM + SINASC + SIH (pysus)
    ibge_pop        Estimativas populacionais (IBGE SIDRA API)
    ipea            PIB per capita e IDH (IPEADATA API)

Dependências Python:
    pip install requests pandas geopandas pysus tqdm

Para RAIS e Registro Civil, consulte docs/data_notes.md
(requer autenticação Google BigQuery).
"""

import os
import sys
import json
import time
import argparse
import zipfile
import logging
import requests
from pathlib import Path
from datetime import datetime

# ── Configuração de logging ──────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-8s  %(message)s",
    datefmt="%H:%M:%S",
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler("scripts/download.log", encoding="utf-8"),
    ],
)
log = logging.getLogger(__name__)

# ── Diretórios do projeto ─────────────────────────────────────────────────────
ROOT     = Path(__file__).resolve().parent.parent
RAW      = ROOT / "data" / "raw"
IBGE_DIR = RAW / "ibge"
ANEEL_DIR = RAW / "aneel"
SUDENE_DIR = RAW / "sudene"
DTASUS_DIR = RAW / "datasus"
SIM_DIR  = DTASUS_DIR / "sim"
SINASC_DIR = DTASUS_DIR / "sinasc"
SIH_DIR  = DTASUS_DIR / "sih"
IPEA_DIR = RAW / "ipea"

for d in [IBGE_DIR, ANEEL_DIR, SUDENE_DIR, SIM_DIR, SINASC_DIR, SIH_DIR, IPEA_DIR]:
    d.mkdir(parents=True, exist_ok=True)

# ── Constantes ────────────────────────────────────────────────────────────────
ANOS_PANEL = range(2000, 2023)
ESTADOS_NE = ["MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"]
CODIGOS_NE = [21, 22, 23, 24, 25, 26, 27, 28, 29]

SESSION = requests.Session()
SESSION.headers.update({
    "User-Agent": "EolicasFilhos-DataDownloader/1.0 (pesquisa academica)"
})


# ════════════════════════════════════════════════════════════════════════════
# UTILIDADES
# ════════════════════════════════════════════════════════════════════════════

def download_file(url: str, dest: Path, desc: str = "", chunk_mb: int = 4,
                  retries: int = 4) -> bool:
    """Download com retry exponential backoff e barra de progresso simples."""
    if dest.exists():
        log.info("  Cache: %s", dest.name)
        return True

    dest.parent.mkdir(parents=True, exist_ok=True)
    chunk = chunk_mb * 1024 * 1024
    wait = 2

    for attempt in range(1, retries + 1):
        try:
            log.info("  [%d/%d] %s -> %s", attempt, retries, url, dest.name)
            r = SESSION.get(url, stream=True, timeout=(30, 300))
            r.raise_for_status()

            total = int(r.headers.get("content-length", 0))
            downloaded = 0
            tmp = dest.with_suffix(dest.suffix + ".tmp")

            with open(tmp, "wb") as f:
                for data in r.iter_content(chunk_size=chunk):
                    f.write(data)
                    downloaded += len(data)
                    if total:
                        pct = downloaded / total * 100
                        print(f"\r  {desc or dest.name}: {pct:.1f}%  "
                              f"({downloaded/1024/1024:.1f}/{total/1024/1024:.1f} MB)",
                              end="", flush=True)

            print()
            tmp.rename(dest)
            log.info("  ✓ Salvo: %s (%.1f MB)", dest, dest.stat().st_size/1024/1024)
            return True

        except Exception as e:
            log.warning("  Tentativa %d falhou: %s", attempt, e)
            if attempt < retries:
                log.info("  Aguardando %ds...", wait)
                time.sleep(wait)
                wait *= 2
            else:
                log.error("  ✗ Falha em: %s", url)
                if tmp.exists():
                    tmp.unlink()
                return False


def check_dependency(pkg: str) -> bool:
    try:
        __import__(pkg)
        return True
    except ImportError:
        log.warning("Pacote '%s' não encontrado. Instale com: pip install %s", pkg, pkg)
        return False


# ════════════════════════════════════════════════════════════════════════════
# 1. IBGE — SHAPEFILE DE MUNICÍPIOS (geobr/IPEA)
# ════════════════════════════════════════════════════════════════════════════

def download_ibge_municipios():
    """Baixa shapefile gpkg de municípios do Nordeste via metadados geobr."""
    log.info("=== IBGE: Municípios do Nordeste ===")

    # Metadados do geobr (hospedados no GitHub)
    meta_url = ("https://github.com/ipeaGIT/geobr/releases/download/"
                "v1.7.0/metadata_1.7.0_gpkg.csv")
    meta_dest = IBGE_DIR / "geobr_metadata.csv"

    if not download_file(meta_url, meta_dest, "geobr metadata"):
        log.error("Falha ao baixar metadados geobr")
        return

    import csv
    # Códigos IBGE dos estados do NE (2 dígitos como string)
    codigos_ne_str = [str(c) for c in CODIGOS_NE]

    baixados = 0
    with open(meta_dest, encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            geo  = row.get("geo", "")
            year = row.get("year", "")
            code = row.get("code", "")
            path = row.get("download_path", "")

            # Apenas municípios 2020, estados do NE, sem simplificado
            if (geo == "municipality" and year == "2020"
                    and code in codigos_ne_str
                    and "simplified" not in path):

                fname = Path(path).name
                dest  = IBGE_DIR / fname
                download_file(path, dest, fname)
                baixados += 1

    log.info("✓ %d arquivos gpkg baixados para %s", baixados, IBGE_DIR)

    # Também baixar semi-árido
    meta_dest2 = IBGE_DIR / "geobr_metadata_semiarido.csv"
    with open(meta_dest, encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            if (row.get("geo") == "semiarid"
                    and "simplified" not in row.get("download_path", "")):
                dest = SUDENE_DIR / Path(row["download_path"]).name
                download_file(row["download_path"], dest, "semiarido gpkg")
                break

    log.info("✓ IBGE municípios concluído")


# ════════════════════════════════════════════════════════════════════════════
# 2. ANEEL — SIGA
# ════════════════════════════════════════════════════════════════════════════

def download_aneel_siga():
    """Baixa o arquivo SIGA da ANEEL (dados.gov.br)."""
    log.info("=== ANEEL: SIGA Empreendimentos ===")

    urls = [
        # URL primária (dados.gov.br)
        ("https://dadosabertos.aneel.gov.br/dataset/"
         "1213b4c3-8a02-4dc5-9b0e-25dff7b6d63e/resource/"
         "b1bd71e7-d0ad-4214-9053-cbd58e9564a7/download/"
         "siga-empreendimentos-geracao.csv"),
        # URL alternativa via ANEEL direta
        "https://www.aneel.gov.br/documents/656877/14854008/siga.csv",
    ]

    dest = ANEEL_DIR / "siga_empreendimentos.csv"
    for url in urls:
        if download_file(url, dest, "SIGA ANEEL"):
            log.info("✓ ANEEL SIGA baixado: %s", dest)
            _validar_aneel(dest)
            return

    log.error("✗ Falha no download do SIGA ANEEL. Baixe manualmente de:\n"
              "  https://dadosabertos.aneel.gov.br/dataset/siga-sistema-de-informacoes-de-geracao-da-aneel")


def _validar_aneel(path: Path):
    """Valida o arquivo SIGA (colunas básicas esperadas)."""
    import csv
    with open(path, encoding="latin-1") as f:
        header = next(csv.reader(f, delimiter=";"))
    colunas_esperadas = ["CodMunicIbge", "DatIniOpe", "TipGeracao", "DscFaseUsina"]
    faltando = [c for c in colunas_esperadas if not any(c.lower() in h.lower() for h in header)]
    if faltando:
        log.warning("  Colunas esperadas ausentes: %s", faltando)
        log.warning("  Colunas encontradas: %s", header[:10])
    else:
        log.info("  ✓ Validação SIGA: colunas OK")


# ════════════════════════════════════════════════════════════════════════════
# 3. SUDENE — Delimitação do Semi-Árido (instruções manuais + gpkg geobr)
# ════════════════════════════════════════════════════════════════════════════

def download_sudene():
    """Instrui o usuário e baixa o gpkg 2017 do semi-árido."""
    log.info("=== SUDENE: Semi-Árido ===")

    # gpkg baixado em download_ibge_municipios() se disponível
    # Verificar se está presente
    gpkg_files = list(SUDENE_DIR.glob("*.gpkg"))
    if gpkg_files:
        log.info("  ✓ gpkg semi-árido encontrado: %s", gpkg_files[0])
    else:
        # Tentar baixar diretamente via metadados geobr
        meta_url = ("https://github.com/ipeaGIT/geobr/releases/download/"
                    "v1.7.0/metadata_1.7.0_gpkg.csv")
        meta_path = IBGE_DIR / "geobr_metadata.csv"
        if meta_path.exists():
            import csv
            with open(meta_path) as f:
                for row in csv.DictReader(f):
                    if (row.get("geo") == "semiarid"
                            and "simplified" not in row.get("download_path", "")):
                        dest = SUDENE_DIR / Path(row["download_path"]).name
                        download_file(row["download_path"], dest, "semiarido")
                        break

    # Gerar template CSV para as resoluções CONDEL (preenchimento manual)
    resolucoes_info = [
        ("semiarido_2005_municipios.csv",
         "Portaria Interministerial nº 1, de 09/03/2005 — delimitação original",
         "https://www.gov.br/sudene/pt-br/assuntos/fne/semiarido"),
        ("resolucao_107_2017.csv",
         "CONDEL/SUDENE nº 107, de 27/07/2017 — +54 municípios",
         "https://www.gov.br/sudene/resolucoes-condel-sudene/resolucao-condel-sudene-no-107-de-27-de-julho-de-2017"),
        ("resolucao_115_2017.csv",
         "CONDEL/SUDENE nº 115, de 23/11/2017 — +73 municípios",
         "https://www.gov.br/sudene/resolucoes-condel-sudene/resolucao-condel-sudene-no-115-de-23-de-novembro-de-2017"),
        ("resolucao_128_2018.csv",
         "CONDEL/SUDENE nº 128, de 10/08/2018 — +15 municípios",
         "https://www.gov.br/sudene/resolucoes-condel-sudene/resolucao-condel-sudene-no-128-de-10-de-agosto-de-2018"),
        ("resolucao_150_2019.csv",
         "CONDEL/SUDENE nº 150, de 13/12/2019 — +9 municípios",
         "https://www.gov.br/sudene/resolucoes-condel-sudene/resolucao-condel-sudene-no-150-de-13-de-dezembro-de-2019"),
        ("resolucao_173_2022.csv",
         "CONDEL/SUDENE nº 173, de 15/12/2021 — +7 municípios",
         "https://www.gov.br/sudene/resolucoes-condel-sudene/resolucao-condel-sudene-no-173-de-15-de-dezembro-de-2021"),
    ]

    template_header = "cod_ibge,municipio,uf,resolucao,ano_vigencia\n"
    template_example = "2304400,Granja,CE,107/2017,2017\n"

    for fname, desc, url in resolucoes_info:
        dest = SUDENE_DIR / fname
        if not dest.exists():
            log.warning("  AÇÃO MANUAL NECESSÁRIA: %s", fname)
            log.warning("  Fonte: %s", desc)
            log.warning("  URL:   %s", url)
            # Criar template vazio com header
            with open(dest, "w", encoding="utf-8") as f:
                f.write(f"# {desc}\n")
                f.write(f"# URL: {url}\n")
                f.write("# Preencha os municípios abaixo com os dados do Anexo da resolução\n")
                f.write(template_header)
                f.write("# Exemplo:\n")
                f.write(f"# {template_example}")
            log.info("  Template criado: %s", dest)
        else:
            log.info("  ✓ Já existe: %s", fname)

    log.info("✓ SUDENE: templates criados em %s", SUDENE_DIR)
    log.info("  IMPORTANTE: preencher os CSVs de resolução manualmente (ver docs/data_notes.md)")


# ════════════════════════════════════════════════════════════════════════════
# 4. DataSUS — SIM, SINASC, SIH (via pysus)
# ════════════════════════════════════════════════════════════════════════════

def download_datasus():
    """Baixa microdados DataSUS para os estados do NE (2000-2022)."""
    log.info("=== DataSUS: SIM + SINASC + SIH ===")

    if not check_dependency("pysus"):
        log.error("pysus não instalado. Execute: pip install pysus")
        return

    # SIM — Sistema de Informações sobre Mortalidade
    _download_sim()

    # SINASC — Sistema de Informações sobre Nascidos Vivos
    _download_sinasc()

    # SIH — Sistema de Informações Hospitalares
    _download_sih()


def _download_sim():
    log.info("  --- SIM (Óbitos) ---")
    try:
        from pysus.online_data import SIM
        for uf in ESTADOS_NE:
            for ano in ANOS_PANEL:
                dest = SIM_DIR / f"DO{uf}{ano}.parquet"
                if dest.exists():
                    log.info("    Cache: DO%s%s", uf, ano)
                    continue
                try:
                    log.info("    Baixando SIM: %s %s", uf, ano)
                    df = SIM.download(states=uf, years=ano)
                    if df is not None and len(df) > 0:
                        df.to_parquet(dest, index=False, compression="zstd")
                        log.info("    ✓ %d óbitos — %s %s", len(df), uf, ano)
                    else:
                        log.warning("    Sem dados: %s %s", uf, ano)
                except Exception as e:
                    log.warning("    Erro SIM %s %s: %s", uf, ano, e)
    except ImportError as e:
        log.error("Erro ao importar pysus.SIM: %s", e)
        log.info("Alternativa: FTP ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/")


def _download_sinasc():
    log.info("  --- SINASC (Nascidos Vivos) ---")
    try:
        from pysus.online_data import SINASC
        for uf in ESTADOS_NE:
            for ano in ANOS_PANEL:
                dest = SINASC_DIR / f"DN{uf}{ano}.parquet"
                if dest.exists():
                    continue
                try:
                    log.info("    Baixando SINASC: %s %s", uf, ano)
                    df = SINASC.download(states=uf, years=ano)
                    if df is not None and len(df) > 0:
                        df.to_parquet(dest, index=False, compression="zstd")
                        log.info("    ✓ %d nascimentos — %s %s", len(df), uf, ano)
                except Exception as e:
                    log.warning("    Erro SINASC %s %s: %s", uf, ano, e)
    except ImportError as e:
        log.error("Erro ao importar pysus.SINASC: %s", e)


def _download_sih():
    log.info("  --- SIH (Internações — apenas CID O00-O08) ---")
    try:
        from pysus.online_data import SIH
        for uf in ESTADOS_NE:
            for ano in ANOS_PANEL:
                dest = SIH_DIR / f"RD{uf}{ano}_aborto.parquet"
                if dest.exists():
                    continue
                try:
                    log.info("    Baixando SIH: %s %s", uf, ano)
                    df = SIH.download(states=uf, years=ano)
                    if df is not None and len(df) > 0:
                        # Filtrar apenas CID O00-O08 para reduzir tamanho
                        cid_col = next((c for c in df.columns
                                        if "diag" in c.lower() or "cid" in c.lower()), None)
                        if cid_col:
                            df = df[df[cid_col].str.startswith("O0", na=False)
                                    & df[cid_col].str.match(r"^O0[0-8]", na=False)]
                        if len(df) > 0:
                            df.to_parquet(dest, index=False, compression="zstd")
                            log.info("    ✓ %d AIH (O00-O08) — %s %s", len(df), uf, ano)
                        else:
                            log.info("    0 registros O00-O08: %s %s", uf, ano)
                except Exception as e:
                    log.warning("    Erro SIH %s %s: %s", uf, ano, e)
    except ImportError as e:
        log.error("Erro ao importar pysus.SIH: %s", e)


# ════════════════════════════════════════════════════════════════════════════
# 5. IBGE SIDRA — Estimativas Populacionais
# ════════════════════════════════════════════════════════════════════════════

def download_ibge_populacao():
    """Baixa estimativas populacionais por município via API IBGE SIDRA."""
    log.info("=== IBGE SIDRA: Estimativas Populacionais ===")

    dest_pop = IBGE_DIR / "populacao_municipios_ne.json"
    if dest_pop.exists():
        log.info("  Cache: %s", dest_pop.name)
        return

    # Tabela 6579 — Estimativa da população residente
    # Variável 9324 — Total
    # N6[all] — todos os municípios do Brasil
    periodos = "|".join(str(a) for a in ANOS_PANEL)
    url = (f"https://servicodados.ibge.gov.br/api/v3/agregados/6579"
           f"/periodos/{periodos}/variaveis/9324"
           f"?localidades=N6[{','.join(str(c) for c in CODIGOS_NE)}[all]]"
           f"&classificacao=all")

    log.info("  Consultando API IBGE SIDRA...")
    try:
        r = SESSION.get(url, timeout=120)
        r.raise_for_status()
        dados = r.json()

        # Aplanar o JSON
        import pandas as pd
        registros = []
        for resultado in dados:
            for local in resultado.get("resultados", []):
                for serie in local.get("series", []):
                    loc_id = serie["localidade"]["id"]
                    loc_nm = serie["localidade"]["nome"]
                    for ano_str, valor in serie["serie"].items():
                        try:
                            pop = float(valor)
                        except (ValueError, TypeError):
                            pop = None
                        registros.append({
                            "cod_mun": loc_id,
                            "nome_mun": loc_nm,
                            "ano": int(ano_str),
                            "populacao": pop
                        })

        df_pop = pd.DataFrame(registros)
        # Converter cod_mun para 7 dígitos
        df_pop["cod_mun"] = df_pop["cod_mun"].astype(str).str[:7]

        df_pop.to_parquet(IBGE_DIR / "populacao_municipios_ne.parquet",
                          index=False, compression="zstd")
        log.info("  ✓ %d registros de população salvos", len(df_pop))

    except Exception as e:
        log.error("  Erro IBGE SIDRA: %s", e)
        log.info("  URL tentada: %s", url)

    log.info("✓ IBGE população concluído")


# ════════════════════════════════════════════════════════════════════════════
# 6. IPEADATA API — PIB per capita municipal
# ════════════════════════════════════════════════════════════════════════════

def download_ipea():
    """Baixa PIB per capita e IDH-M do IPEADATA."""
    log.info("=== IPEADATA: PIB per capita e IDH-M ===")

    series = {
        "PIB_pc": "PIB_MPMPC",   # PIB per capita municipal
        "IDH_M":  "ADH_IDHM",   # IDH Municipal
        "GINI":   "ADH_GINI",    # Índice de Gini
        "POP":    "POPTOT",      # População total
    }

    import pandas as pd

    for nome, codigo in series.items():
        dest = IPEA_DIR / f"{nome.lower()}_nordeste.parquet"
        if dest.exists():
            log.info("  Cache: %s", dest.name)
            continue

        url = f"http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='{codigo}')"
        log.info("  Baixando %s (%s)...", nome, codigo)

        try:
            r = SESSION.get(url, timeout=120)
            r.raise_for_status()
            dados = r.json().get("value", [])

            df = pd.DataFrame(dados)
            if df.empty:
                log.warning("  Sem dados: %s", codigo)
                continue

            # Filtrar municípios do NE (primeiros 2 dígitos do TERCODIGO)
            if "TERCODIGO" in df.columns:
                df["uf_cod"] = df["TERCODIGO"].astype(str).str[:2]
                df = df[df["uf_cod"].isin([str(c) for c in CODIGOS_NE])]
                df = df.rename(columns={"TERCODIGO": "cod_mun", "VALDATA": "ano",
                                        "VALVALOR": "valor"})
                # Extrair ano
                if "ano" in df.columns:
                    df["ano"] = pd.to_datetime(df["ano"], errors="coerce").dt.year

                df_out = df[["cod_mun", "ano", "valor"]].dropna(subset=["cod_mun"])
                df_out["cod_mun"] = df_out["cod_mun"].astype(str).str[:7]
                df_out.to_parquet(dest, index=False, compression="zstd")
                log.info("  ✓ %d registros — %s", len(df_out), nome)
            else:
                log.warning("  Estrutura inesperada: %s", df.columns.tolist()[:5])

        except Exception as e:
            log.error("  Erro IPEADATA [%s]: %s", codigo, e)

    log.info("✓ IPEADATA concluído")


# ════════════════════════════════════════════════════════════════════════════
# PONTO DE ENTRADA
# ════════════════════════════════════════════════════════════════════════════

FONTES_DISPONIVEIS = {
    "ibge":     download_ibge_municipios,
    "aneel":    download_aneel_siga,
    "sudene":   download_sudene,
    "datasus":  download_datasus,
    "ibge_pop": download_ibge_populacao,
    "ipea":     download_ipea,
}


def main():
    parser = argparse.ArgumentParser(
        description="Download de dados — Eólicas e Filhos",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument(
        "--fonte", default="all",
        choices=["all"] + list(FONTES_DISPONIVEIS.keys()),
        help="Fonte de dados a baixar (padrão: all)"
    )
    args = parser.parse_args()

    inicio = datetime.now()
    log.info("=" * 60)
    log.info("Eólicas e Filhos — Download de Dados")
    log.info("Fonte: %s | Início: %s", args.fonte, inicio.strftime("%Y-%m-%d %H:%M"))
    log.info("Diretório raiz: %s", ROOT)
    log.info("=" * 60)

    if args.fonte == "all":
        fontes = list(FONTES_DISPONIVEIS.values())
    else:
        fontes = [FONTES_DISPONIVEIS[args.fonte]]

    erros = 0
    for fn in fontes:
        try:
            fn()
        except KeyboardInterrupt:
            log.warning("Interrompido pelo usuário.")
            sys.exit(1)
        except Exception as e:
            log.error("Erro inesperado em %s: %s", fn.__name__, e)
            erros += 1

    fim = datetime.now()
    log.info("=" * 60)
    log.info("Concluído em %.1f min | Erros: %d", (fim - inicio).seconds / 60, erros)
    log.info("Logs salvos em: scripts/download.log")
    log.info("=" * 60)

    if erros > 0:
        sys.exit(1)


if __name__ == "__main__":
    main()
