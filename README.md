## PhysiMineR

<img src="img/PhysiMineRLogo.png" align="right" width="180" alt="PhysiMineR logo"/>

PhysiMineR is an R package for cleaning, standardising, and analysing upper GI physiology datasets, including:

- High-resolution manometry (HRM)
- Ambulatory pH-impedance monitoring
- Wireless pH capsule (BRAVO)

The package includes modernized, test-backed workflows for:

- Data cleaning (`HRMCleanUp1`, `dataImpClean`, `dataBRAVOClean`)
- Merge and completeness utilities (`testMerge`, `tripleTestMerge`, `filterByCompleteness`)
- Classification logic aligned to Lyon/Chicago frameworks (`GORD_AcidImp_Lyon`, `GORD_AcidBRAVO_Lyon`, `HRMDiagnoses`)

## Installation

```r
# install.packages("remotes")
remotes::install_github("sebastiz/PhysiMineR")
```

## Quick start

```r
library(PhysiMineR)

# Example: clean and classify impedance data
imp_clean <- dataImpClean(raw_impedance_df)
imp_sx    <- dataImpSymptoms(imp_clean)
lyon_out  <- GORD_AcidImp_Lyon(imp_sx)

# Example: clean HRM and derive diagnoses
hrm_clean <- HRMCleanUp1(raw_hrm_df)
hrm_dx    <- HRMDiagnoses(hrm_clean)
```

## Development notes

- Legacy scripts are still present for historical workflows.
- The package `Collate` order in `DESCRIPTION` forces deterministic loading so the modern APIs are the canonical active definitions.
- CI is configured through GitHub Actions (`.github/workflows/R-CMD-check.yaml`).
# PhysiMineR <img src="img/PhysiMineRLogo.png" align="right" width="140"/>

> **R package for extraction, cleaning, and analysis of upper GI physiological data**  
> Acid reflux classified per **Lyon Consensus 2.0** · Motility diagnosed per **Chicago Classification v4.0**

[![R ≥ 4.0](https://img.shields.io/badge/R-%E2%89%A54.0-blue)](https://cran.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

---

## Contents

1. [Installation](#installation)
2. [Overview](#overview)
3. [BRAVO wireless pH capsule](#1-bravo-wireless-ph-capsule)
4. [Ambulatory pH-impedance monitoring](#2-ambulatory-ph-impedance-monitoring)
5. [High-resolution manometry (HRM)](#3-high-resolution-manometry-hrm)
6. [Cross-test merging](#4-cross-test-merging)
7. [Symptom classification](#5-symptom-classification)
8. [Data quality filtering](#6-data-quality-filtering)
9. [Lyon Consensus 2.0 — criteria reference](#lyon-consensus-20--criteria-reference)
10. [Chicago Classification v4.0 — criteria reference](#chicago-classification-v40--criteria-reference)
11. [Full worked example](#full-worked-example)
12. [Function reference](#function-reference)
13. [References](#references)

---

## Installation

```r
# Install from GitHub (requires remotes)
install.packages("remotes")
remotes::install_github("sebastiz/PhysiMineR")
```

```r
library(PhysiMineR)
```

---

## Overview

PhysiMineR provides a **three-stage pipeline** for each test type:

```
Import raw export files
        ↓
Clean & standardise  (dataBRAVOClean / HRMCleanUp1 / dataImpClean)
        ↓
Classify             (GORD_AcidBRAVO_Lyon / HRMDiagnoses / GORD_AcidImp_Lyon)
```

After cleaning individual tests, use `testMerge()` to combine tests from the
same patient by matching the closest visit dates.

| Test | Clean | Classify |
|------|-------|---------|
| BRAVO wireless pH | `dataBRAVOClean()` → `dataBRAVODayLabeller()` | `GORD_AcidBRAVO_Lyon()` |
| pH-impedance | `dataImpClean()` → `dataImpSymptoms()` | `GORD_AcidImp_Lyon()` |
| HRM | `HRMCleanUp1()` | `HRMDiagnoses()` |

---

## 1. BRAVO wireless pH capsule

### 1.1 Import and merge raw exports

Sierra exports BRAVO data across three separate files (days 1–2, days 3–4,
and a totals summary). Merge them on the shared study ID before cleaning:

```r
library(PhysiMineR)
library(dplyr)

day12 <- read.csv("BravoDay1And2.csv")
day34 <- read.csv("BravoDay3And4.csv")
total <- read.csv("BRAVOTotal.csv")

raw_bravo <- merge(day12, day34, by = "BravoID", all = TRUE) %>%
             merge(total,        by = "BravoID", all = TRUE)
```

### 1.2 Clean

`dataBRAVOClean()` coerces pH/SAP/SI columns to numeric, converts the visit
date, and deduplicates within each study:

```r
clean_bravo <- dataBRAVOClean(raw_bravo, date_col = "VisitDate")
```

### 1.3 Label per-day AET columns

Sierra uses inconsistent column names across exports
(`ReflDay1...`, `ReflDay1_2...`, etc.).  `dataBRAVODayLabeller()` maps these
to standardised `bravoDay1` … `bravoDay6` columns:

```r
labelled_bravo <- dataBRAVODayLabeller(clean_bravo,
                                        id_col   = "HospNum_Id",
                                        date_col = "VisitDate")

# New columns added:
# bravoDay1, bravoDay2, bravoDay3, bravoDay4, bravoDay5, bravoDay6
# bravoNDays  — number of days with a recorded AET value
```

### 1.4 Extract BRAVO symptoms

```r
labelled_bravo <- dataBRAVOSymptoms(labelled_bravo, symp_col = "Symptoms")

# New columns:
# AllSymps_BRAVO           — comma-separated unique symptom list
# AllSymps_BRAVOgrouped    — "Oesophageal", "LPR", "Mixed", or "Other"
# AllSymps_BRAVOcompartment — anatomical compartment
```

### 1.5 Classify — Lyon Consensus 2.0

```r
# Three-tier Lyon classification (default thresholds: 6% / 4%, >= 2 positive days)
classified_bravo <- GORD_AcidBRAVO_Lyon(labelled_bravo)

table(classified_bravo$AcidRefluxBRAVO_Lyon)
# GORD excluded   Inconclusive  Conclusive GORD
#            12             18               34
```

**Minimal reproducible example:**

```r
df <- data.frame(
  HospNum_Id = c("P001", "P002", "P003", "P004", "P005"),
  bravoDay1  = c(7.2,  3.1,  5.0,  6.5,  6.8),
  bravoDay2  = c(6.8,  2.9,  4.5,  5.8,  7.1),
  bravoDay3  = c(7.5,  3.5,  4.8,  NA,   6.4),
  bravoDay4  = c(6.1,  3.0,  5.2,  NA,   7.2),
  bravoNDays = c(4,    4,    4,    2,    4)
)

out <- GORD_AcidBRAVO_Lyon(df)
out[, c("HospNum_Id", "AcidRefluxBRAVO_Lyon", "AcidRefluxBRAVO",
        "AcidRefluxBRAVOTotalOnly", "AcidRefluxBRAVOAv")]

#   HospNum_Id    AcidRefluxBRAVO_Lyon AcidRefluxBRAVO AcidRefluxBRAVOTotalOnly AcidRefluxBRAVOAv
# 1       P001         Conclusive GORD               1                        0                 1
# 2       P002           GORD excluded               0                        1                 0
# 3       P003            Inconclusive               0                        0                 0
# 4       P004            Inconclusive               0                        0                 1
# 5       P005         Conclusive GORD               1                        0                 1
```

**Changing thresholds** (e.g. for a sensitivity analysis):

```r
# Require only 1 positive day instead of 2
GORD_AcidBRAVO_Lyon(df, min_positive_days = 1)

# Use older 5.3% threshold
GORD_AcidBRAVO_Lyon(df, pathological_aet = 5.3, normal_aet = 4)
```

### 1.6 Worst-day and average-day analyses

```r
# Adds: worstt, average, worstDaypH, NumDaysBravoPositive,
#        WorstD_ayAnalysisGORDPositive
result <- GORD_BravoWDAAndAverage(classified_bravo, n_days = 4, aet_threshold = 6)

# Which day was worst most often?
table(result$worstDaypH)

# How many positive days did GORD patients have?
hist(result$NumDaysBravoPositive[result$AcidRefluxBRAVO == 1])
```

---

## 2. Ambulatory pH-impedance monitoring

### 2.1 Import and clean

```r
imp  <- read.csv("Impedance2.csv")
symp <- read.csv("Imp_Symp.csv")

raw_imp    <- merge(imp, symp, by = "Imp_Id", all = TRUE)
clean_imp  <- dataImpClean(raw_imp)
```

`dataImpClean()` coerces all reflux metric columns to numeric, converts dates,
and creates binary `Sx_*` indicators for each symptom type based on non-missing
SAP/SI entries.

### 2.2 Categorise symptoms

```r
imp_with_sx <- dataImpSymptoms(clean_imp)

# New columns:
# SAPOesophageal  — max SAP across oesophageal symptoms (heartburn, regurg, etc.)
# SAPLPR          — max SAP across LPR symptoms (cough, throat)
# AllSymps_Impgrouped — "Oesophageal", "LPR", "Mixed", "Other"
# AllImpSymptom   — comma-separated list of symptom types with SAP recorded

table(imp_with_sx$AllSymps_Impgrouped)
```

### 2.3 Classify — Lyon Consensus 2.0

```r
# Basic classification (AET only)
imp_lyon <- GORD_AcidImp_Lyon(imp_with_sx)

table(imp_lyon$AcidReflux_Lyon)
# GORD excluded   Inconclusive  Conclusive GORD
#            45             22               61
```

**With adjunctive metrics** (upgrades borderline AET 4–6% cases):

```r
# Add MNBI and PSPW data first
mnbi  <- read_excel("MNBI.xlsx")
mnbi2 <- read_excel("MNBI2.xlsx")
pspw  <- read_excel("Seb_PSPW_list.xlsx") %>%
          rename(HospNum_Id = HospitalNum)

imp_adj <- addAdjunctiveMetrics(imp_with_sx,
                                 mnbi_df = rbind(mnbi, mnbi2) %>%
                                             rename(HospNum_Id = HospitalNumber),
                                 pspw_df = pspw)

# Re-classify with adjunctive support
imp_lyon_adj <- GORD_AcidImp_Lyon(imp_adj,
                                   mnbi_col = "M1_av",
                                   pspw_col = "pspw_index")

# Borderline patients with MNBI < 2292 Ω or PSPW < 61% are upgraded to
# "Conclusive GORD (adjunctive)"
table(imp_lyon_adj$AcidReflux_Lyon)
```

**Full example with all thresholds shown:**

```r
df_imp <- data.frame(
  HospNum_Id = c("B001", "B002", "B003", "B004", "B005"),
  MainAcidExpTotalClearanceChannelPercentTime = c(8.2, 5.1, 2.8, 4.9, 4.7),
  M1_av      = c(1800, 2500, 3100, 2000, 2800),  # MNBI (Ω)
  pspw_index = c(45,   70,   80,   55,   65)      # PSPW (%)
)

GORD_AcidImp_Lyon(df_imp, mnbi_col = "M1_av", pspw_col = "pspw_index") %>%
  select(HospNum_Id, AcidReflux_Lyon, AcidReflux_Imp)

#   HospNum_Id                 AcidReflux_Lyon AcidReflux_Imp
# 1       B001                 Conclusive GORD              1
# 2       B002                    Inconclusive              0
# 3       B003                   GORD excluded              0
# 4       B004 Conclusive GORD (adjunctive)               1   # MNBI 2000 < 2292
# 5       B005                    Inconclusive              0  # MNBI 2800 >= 2292
```

**Legacy threshold (pre-Lyon, AET > 4.2%):**

```r
# Kept for backward compatibility and historical comparison
imp_legacy <- GORD_AcidImp(imp_with_sx)
# Adds: AcidReflux_Imp (1 if AET > 4.2%)
```

---

## 3. High-resolution manometry (HRM)

### 3.1 Import and clean

```r
main      <- read.csv("HRMImportMain.csv")
swallows  <- read.csv("HRMImportSwallows.csv")

# Remove per-swallow raw numeric trace columns before merging
swallows <- swallows[, !grepl("Num\\d+", names(swallows))]

merged_hrm <- merge(main, swallows, by = "HRM_Id", all = TRUE)
clean_hrm  <- HRMCleanUp1(merged_hrm)
# Returns one row per HRM study (collapses duplicate rows, coerces types)
```

### 3.2 Aggregate per-swallow metrics

If you have the raw swallow-level file, compute study-level summary statistics:

```r
swallow_summary <- HRMSwallowSummary(
  swallows,
  dci_col   = "DCImmHgcms",
  dl_col    = "DistallatencyS",
  cfv_col   = "ContractilefrontvelocityCms",
  break_col = "BreakCm"
)

# Columns added per HRM_Id:
# DCI_mean, DCI_median, pct_failed, pct_weak, pct_hypercontract
# DL_mean, pct_premature, CFV_mean
# pct_large_break, pct_small_break
```

### 3.3 Diagnose — Chicago Classification v4.0

```r
hrm_diagnosed <- HRMDiagnoses(clean_hrm)

table(hrm_diagnosed$ChicagoV4Diagnosis)
table(hrm_diagnosed$ChicagoV4DiagnosisGroup)
```

**Full example showing all nine diagnoses:**

```r
df_hrm <- data.frame(
  HRM_Id = paste0("H00", 1:9),
  ResidualmeanmmHg             = c(20, 18, 16,  8,  8, 12,  6, 10,  9),
  failedChicagoClassification  = c(100, 20, 30,  0,  0,  0,100, 80,  0),
  panesophagealpressurization  = c(0,  30,  0,  0,  0,  0,  0,  0,  0),
  prematurecontraction         = c(0,   0, 25, 25,  0,  0,  0,  0,  0),
  rapidcontraction             = c(0,   0,  0,  0, 25,  0,  0,  5,  5),
  DistalcontractileintegralmeanmmHgcms = c(50, 300, 200, 1200, 9500, 800, 60, 200, 800),
  largebreaks                  = c(0,   0,  0,  0,  0,  0,  0, 55,  0),
  smallbreaks                  = c(0,   0,  0,  0,  0,  0,  0,  0, 55),
  Simultaneous                 = c(0,   0,  0,  0,  0,  0,  0,  0,  0)
)

HRMDiagnoses(df_hrm)[, c("HRM_Id", "ChicagoV4Diagnosis", "ChicagoV4DiagnosisGroup")]

#   HRM_Id                          ChicagoV4Diagnosis      ChicagoV4DiagnosisGroup
# 1   H001                           Achalasia Type I                     Achalasia
# 2   H002                          Achalasia Type II                     Achalasia
# 3   H003                         Achalasia Type III                     Achalasia
# 4   H004                  Distal Oesophageal Spasm    Major Peristalsis Disorder
# 5   H005 Hypercontractile Oesophagus (Jackhammer)    Major Peristalsis Disorder
# 6   H006                                    Normal                        Normal
# 7   H007                      Absent Contractility    Major Peristalsis Disorder
# 8   H008          Ineffective Oesophageal Motility    Minor Peristalsis Disorder
# 9   H009                   Fragmented Peristalsis    Minor Peristalsis Disorder
```

**Changing the IRP upper limit of normal** (system-dependent):

```r
# Sierra / Medtronic: ULN = 15 mmHg (default)
HRMDiagnoses(df_hrm, irp_uln = 15)

# Diversatek: ULN = 12 mmHg
HRMDiagnoses(df_hrm, irp_uln = 12)
```

---

## 4. Cross-test merging

When patients have had multiple test types, use `testMerge()` to match studies
by patient and select the pair that are closest in time.

### 4.1 Two-test merge

```r
hrm_imp <- testMerge(clean_hrm, imp_lyon_adj,
                      id_col         = "HospNum_Id",
                      join_type      = "inner",
                      max_days_apart = 365)  # drop pairs > 1 year apart

nrow(hrm_imp)  # one row per patient
```

**Example with patient having two HRM studies:**

```r
hrm <- data.frame(
  HospNum_Id  = c("A", "A",  "B"),
  VisitDate.x = as.Date(c("2022-01-01", "2023-06-01", "2022-03-01")),
  IRP = c(8, 10, 14)
)
bravo <- data.frame(
  HospNum_Id  = c("A", "B"),
  VisitDate.y = as.Date(c("2022-01-20", "2022-03-10")),
  AET_day1    = c(7.2, 3.1)
)

testMerge(hrm, bravo)
#   HospNum_Id  VisitDate.x  VisitDate.y IRP AET_day1 Date_ABS_Diff
# 1          A   2022-01-01   2022-01-20   8      7.2            19
# 2          B   2022-03-01   2022-03-10  14      3.1             9
# Patient A's 2022 HRM is chosen (19 days from BRAVO) over the 2023 study (516 days)
```

### 4.2 Three-test merge

```r
triple <- tripleTestMerge(clean_hrm, imp_lyon_adj, classified_bravo,
                           id_col         = "HospNum_Id",
                           join_type      = "inner",
                           max_days_apart = 365)
```

---

## 5. Symptom classification

### Classify SAP / SI per Lyon Consensus

```r
# Adds SAP_positive_* and SI_positive_* binary columns per symptom,
# plus summary columns AnySAP_positive, AnySI_positive, SymptomAssoc_positive
imp_sx <- classifySymptomAssociation(imp_lyon_adj,
                                      sap_threshold = 95,
                                      si_threshold  = 50)

# How many patients have a positive symptom association?
table(imp_sx$SymptomAssoc_positive)
```

### Extract symptoms from free text

```r
notes <- data.frame(
  HospNum_Id  = c("P001", "P002", "P003"),
  ClinicalNote = c(
    "Patient complains of heartburn and regurgitation daily",
    "Chronic cough and throat clearing, no heartburn",
    "Dysphagia to solids, occasional vomiting"
  )
)

notes_sx <- extractSymptoms(notes, text_cols = "ClinicalNote")

# Binary columns added:
# Sx_Heartburn, Sx_Regurgitation, Sx_Dysphagia, Sx_ChestPain,
# Sx_Belching, Sx_Cough, Sx_ThroatSymptoms, Sx_Nausea,
# Sx_Vomiting, Sx_StomachPain

notes_sx[, grep("^Sx_", names(notes_sx))]

# Sx_Heartburn Sx_Regurgitation Sx_Dysphagia Sx_Cough Sx_ThroatSymptoms Sx_Vomiting
#            1                1            0        0                 0           0
#            0                0            0        1                 1           0
#            0                0            1        0                 0           1
```

### Summarise symptom burden

```r
notes_burden <- symptomBurdenSummary(notes_sx)

notes_burden[, c("HospNum_Id", "nSymptoms", "symptomBurden")]
#   HospNum_Id nSymptoms symptomBurden
# 1       P001         2      Moderate
# 2       P002         2      Moderate
# 3       P003         2      Moderate
```

---

## 6. Data quality filtering

Remove columns with too much missing data before modelling:

```r
# Keep only columns >= 90% complete (default)
df_clean <- filterByCompleteness(triple,
                                  col_complete_threshold = 0.9,
                                  verbose = TRUE)
# Dropping 4 column(s) with < 90% complete data:
#   SAPTotalNausea
#   SAPTotalVomiting
#   ...

# Also enforce row-level completeness
df_clean <- filterByCompleteness(triple,
                                  col_complete_threshold = 0.9,
                                  row_complete_threshold = 0.8)
```

---

## Lyon Consensus 2.0 — criteria reference

| Metric | Threshold | Interpretation |
|--------|-----------|----------------|
| AET (catheter-based, 24 h) | > 6% | Conclusive GORD |
| AET (catheter-based, 24 h) | 4–6% | Inconclusive (borderline) |
| AET (catheter-based, 24 h) | < 4% | GORD excluded |
| AET (BRAVO, 96 h) | > 6% on **≥ 2 days** | Conclusive GORD |
| AET (BRAVO, 96 h) | < 4% on **all days** | GORD excluded |
| MNBI | < 2292 Ω | Supportive of GORD (adjunctive) |
| PSPW index | < 61% | Supportive of GORD (adjunctive) |
| Total reflux episodes | > 80 | Supportive of GORD (adjunctive) |
| SAP | ≥ 95% | Positive symptom-reflux association |
| SI | ≥ 50% | Positive symptom index |

Adjunctive metrics upgrade a borderline AET to **Conclusive GORD (adjunctive)**
when at least one threshold is exceeded.

---

## Chicago Classification v4.0 — criteria reference

| Diagnosis | IRP | Additional criteria |
|-----------|-----|---------------------|
| Achalasia Type I | > ULN | 100% failed peristalsis, no pan-oesophageal pressurisation |
| Achalasia Type II | > ULN | ≥ 20% pan-oesophageal pressurisation |
| Achalasia Type III | > ULN | ≥ 20% premature contractions (DL < 4.5 s) |
| EGJ Outflow Obstruction | > ULN | Does not meet Types I–III |
| Distal Oesophageal Spasm | Normal | ≥ 20% premature contractions |
| Hypercontractile (Jackhammer) | Any | ≥ 20% swallows with DCI > 8000 mmHg·cm·s |
| Absent Contractility | Normal | 100% failed peristalsis (DCI < 100) |
| IEM | Normal | ≥ 70% ineffective swallows |
| Fragmented Peristalsis | Normal | ≥ 50% fragmented (large break ≥ 5 cm) AND mean DCI > 100 |
| Normal | Normal | None of the above |

IRP upper limit of normal (ULN): **15 mmHg** for Sierra/Medtronic systems,
**12 mmHg** for Diversatek. Set with `irp_uln` argument in `HRMDiagnoses()`.

---

## Full worked example

End-to-end workflow combining all three test types, matching the
`NegImpPredictorsOfAllPosBRAVO` study design:

```r
library(PhysiMineR)
library(dplyr)
library(here)

# ── 1. Import ──────────────────────────────────────────────────────────────────
day12  <- read.csv(here("data/BravoDay1And2.csv"))
day34  <- read.csv(here("data/BravoDay3And4.csv"))
total  <- read.csv(here("data/BRAVOTotal.csv"))
imp    <- read.csv(here("data/Impedance2.csv"))
symp   <- read.csv(here("data/Imp_Symp.csv"))
main   <- read.csv(here("data/HRMImportMain.csv"))
swalls <- read.csv(here("data/HRMImportSwallows.csv"))

# ── 2. Clean ───────────────────────────────────────────────────────────────────
# BRAVO
all_bravo <- merge(day12, day34, by = "BravoID", all = TRUE) %>%
             merge(total, by = "BravoID", all = TRUE) %>%
             dataBRAVOClean() %>%
             dataBRAVODayLabeller() %>%
             dataBRAVOSymptoms() %>%
             GORD_AcidBRAVO_Lyon() %>%
             GORD_BravoWDAAndAverage(n_days = 4)

# Impedance
all_imp <- merge(imp, symp, by = "Imp_Id", all = TRUE) %>%
           dataImpClean() %>%
           dataImpSymptoms() %>%
           GORD_AcidImp()   # legacy; re-run with Lyon after adjunctive merge

# HRM
swalls_trim <- swalls[, !grepl("Num\\d+", names(swalls))]
all_hrm <- merge(main, swalls_trim, by = "HRM_Id", all = TRUE) %>%
           HRMCleanUp1() %>%
           HRMDiagnoses()

# ── 3. Cross-test merge ────────────────────────────────────────────────────────
combined <- tripleTestMerge(all_hrm, all_imp, all_bravo,
                             max_days_apart = 365)

# ── 4. Filter: negative impedance patients only ────────────────────────────────
neg_imp <- combined %>%
  filter(AcidReflux_Imp == 0,
         (MainRflxEpisodeTotalAcid + MainRflxEpisodeTotalNonacid) < 80)

# ── 5. Quality filter ──────────────────────────────────────────────────────────
neg_imp_qc <- filterByCompleteness(neg_imp, col_complete_threshold = 0.9)

# ── 6. Outcome summary ─────────────────────────────────────────────────────────
cat("N patients:", nrow(neg_imp_qc), "\n")
cat("BRAVO-positive (Lyon average-day):",
    sum(neg_imp_qc$AcidRefluxBRAVOAv), "\n")
cat("Chicago diagnosis breakdown:\n")
print(table(neg_imp_qc$ChicagoV4Diagnosis))
```

---

## Function reference

| Function | Description |
|----------|-------------|
| `dataBRAVOClean()` | Clean and deduplicate merged BRAVO export |
| `dataBRAVODayLabeller()` | Standardise per-day AET column names |
| `dataBRAVOSymptoms()` | Parse BRAVO symptom free text |
| `GORD_AcidBRAVO_Lyon()` | Lyon 2.0 three-tier BRAVO classification |
| `GORD_BravoWDAAndAverage()` | Worst-day and average-day AET analysis |
| `dataImpClean()` | Clean merged pH-impedance export |
| `dataImpSymptoms()` | Categorise impedance symptoms by group |
| `addAdjunctiveMetrics()` | Merge MNBI and PSPW index |
| `GORD_AcidImp_Lyon()` | Lyon 2.0 impedance classification with adjunctive support |
| `GORD_AcidImp()` | Legacy 4.2% AET threshold classification |
| `HRMCleanUp1()` | Clean and collapse merged HRM export |
| `HRMSwallowSummary()` | Aggregate per-swallow metrics to study level |
| `HRMDiagnoses()` | Chicago Classification v4.0 motility diagnosis |
| `testMerge()` | Merge two test datasets by closest visit date |
| `tripleTestMerge()` | Merge three test datasets by closest visit dates |
| `filterByCompleteness()` | Remove low-completeness columns/rows |
| `classifySymptomAssociation()` | Binary SAP/SI classification per Lyon 2.0 |
| `extractSymptoms()` | Extract symptom presence from free text |
| `symptomBurdenSummary()` | Tally and categorise symptom burden |

---

## References

- Gyawali CP, Kahrilas PJ, Savarino E, et al. (2021). Modern diagnosis of GERD:
  the Lyon Consensus. *Gut*, 70(7), 1351–1362.
  doi:[10.1136/gutjnl-2020-323270](https://doi.org/10.1136/gutjnl-2020-323270)

- Yadlapati R, Kahrilas PJ, Fox MR, et al. (2021). Esophageal motility disorders
  on high-resolution manometry: Chicago Classification version 4.0.
  *Neurogastroenterology & Motility*, 33(1), e14058.
  doi:[10.1111/nmo.14058](https://doi.org/10.1111/nmo.14058)

- Sweis R, Kaufman E, Anggiansah A, et al. (2014). Post-reflux swallow-induced
  peristaltic wave index and nocturnal baseline impedance can link unexplained
  symptoms to reflux in patients with negative 24-h pH-impedance. *Neurogastroenterology
  & Motility*, 26(12), 1723–1732.
