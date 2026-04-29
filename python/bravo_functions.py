"""Python rewrite of `R/bravo_functions.R`.

Implements BRAVO cleaning, day labelling, symptom parsing, and worst-day/
average AET summaries using pandas.
"""

from __future__ import annotations

import re
from typing import Iterable

import numpy as np
import pandas as pd


def _check_cols(df: pd.DataFrame, cols: Iterable[str]) -> None:
    missing = [c for c in cols if c not in df.columns]
    if missing:
        raise ValueError(f"Missing required columns: {missing}")


def _null_to_na(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out = out.replace({"": np.nan, "null": np.nan, "NULL": np.nan, "NA": np.nan})
    return out


def _to_date(series: pd.Series) -> pd.Series:
    return pd.to_datetime(series, errors="coerce").dt.date


def _to_numeric(series: pd.Series) -> pd.Series:
    return pd.to_numeric(series.astype("string").str.replace("%", "", regex=False), errors="coerce")


def data_bravo_clean(df: pd.DataFrame, date_col: str = "VisitDate") -> pd.DataFrame:
    """Clean and standardize merged BRAVO dataframe.

    Equivalent to R `dataBRAVOClean`.
    """
    _check_cols(df, ["BravoID"])
    out = _null_to_na(df)

    if date_col in out.columns:
        out[date_col] = _to_date(out[date_col])

    ph_pattern = re.compile(r"FractionTime|DeMeester|SAP|^SI|Episode|Longest|Composite")
    num_cols = [c for c in out.columns if ph_pattern.search(c)]
    for c in num_cols:
        out[c] = _to_numeric(out[c])

    out = out.drop_duplicates(subset=["BravoID"], keep="first")
    return out


def data_bravo_day_labeller(
    df: pd.DataFrame,
    id_col: str = "HospNum_Id",
    date_col: str = "VisitDate",
) -> pd.DataFrame:
    """Label BRAVO study day AET columns as bravoDay1..bravoDay6.

    Equivalent to R `dataBRAVODayLabeller`.
    """
    day_pool = [
        "ReflDay1FractionTimepHLessThan4Total",
        "ReflDay2FractionTimepHLessThan4Total",
        "ReflDay1_2FractionTimepHLessThan4Total",
        "ReflDay2_2FractionTimepHLessThan4Total",
        "ReflDay3_2FractionTimepHLessThan4Total",
        "ReflDay4_2FractionTimepHLessThan4Total",
    ]

    out = df.copy()
    present = [c for c in day_pool if c in out.columns]
    if not present:
        return out

    for c in present:
        out[c] = _to_numeric(out[c])

    for i, c in enumerate(present, start=1):
        out[f"bravoDay{i}"] = out[c]

    day_cols = [f"bravoDay{i}" for i in range(1, len(present) + 1)]
    out["bravoNDays"] = out[day_cols].notna().sum(axis=1)
    return out


def data_bravo_symptoms(df: pd.DataFrame, symp_col: str = "Symptoms") -> pd.DataFrame:
    """Extract and group BRAVO symptoms from free-text column.

    Equivalent to R `dataBRAVOSymptoms`.
    """
    out = df.copy()
    if symp_col not in out.columns:
        out["AllSymps_BRAVO"] = pd.NA
        out["AllSymps_BRAVOgrouped"] = pd.NA
        out["AllSymps_BRAVOcompartment"] = pd.NA
        return out

    oesophageal_sx = {"Heartburn", "Regurgitation", "ChestPain", "Belch", "Vomiting", "Nausea"}
    lpr_sx = {"Cough", "Throat", "Hoarseness", "Globus"}

    all_symps = []
    grouped = []
    compartment = []

    for raw in out[symp_col]:
        if pd.isna(raw) or str(raw).strip() == "":
            all_symps.append(pd.NA)
            grouped.append(pd.NA)
            compartment.append(pd.NA)
            continue

        syms = [s.strip() for s in re.split(r"[|,;]", str(raw)) if s.strip()]
        syms = sorted(set("StomachPain" if s == "Epigastric" else s for s in syms))

        has_oeso = any(s in oesophageal_sx for s in syms)
        has_lpr = any(s in lpr_sx for s in syms)
        if has_oeso and has_lpr:
            g = "Mixed"
        elif has_oeso:
            g = "Oesophageal"
        elif has_lpr:
            g = "LPR"
        else:
            g = "Other"

        if g == "LPR":
            cpt = "Supraesophageal"
        elif g == "Oesophageal":
            cpt = "Oesophageal"
        elif g == "Mixed":
            cpt = "Mixed"
        else:
            cpt = "Other"

        all_symps.append(",".join(syms))
        grouped.append(g)
        compartment.append(cpt)

    out["AllSymps_BRAVO"] = all_symps
    out["AllSymps_BRAVOgrouped"] = grouped
    out["AllSymps_BRAVOcompartment"] = compartment
    return out


def gord_bravo_wda_and_average(
    df: pd.DataFrame,
    n_days: int = 4,
    aet_threshold: float = 6,
) -> pd.DataFrame:
    """Compute worst-day and average AET from bravoDay columns.

    Equivalent to R `GORD_BravoWDAAndAverage`.
    """
    out = df.copy()
    day_cols = [c for c in (f"bravoDay{i}" for i in range(1, n_days + 1)) if c in out.columns]
    if not day_cols:
        raise ValueError("No standardised bravoDay* columns found. Run data_bravo_day_labeller first.")

    mat = out[day_cols].apply(pd.to_numeric, errors="coerce")
    out["worstt"] = mat.max(axis=1, skipna=True)
    out["average"] = mat.mean(axis=1, skipna=True)

    def _worst_day(row: pd.Series) -> object:
        if row.isna().all():
            return pd.NA
        return f"Day{int(np.nanargmax(row.to_numpy(dtype=float)) + 1)}"

    out["worstDaypH"] = mat.apply(_worst_day, axis=1)
    out["NumDaysBravoPositive"] = (mat > aet_threshold).sum(axis=1)
    out["WorstD_ayAnalysisGORDPositive"] = (out["worstt"] > aet_threshold).astype("Int64")
    return out
