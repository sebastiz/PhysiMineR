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
