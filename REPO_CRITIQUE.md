# Repository Critique (April 2026)

## Overall assessment

This repository has strong domain ambition and a meaningful migration toward modern, testable R package code. However, it currently mixes **legacy pipeline scripts** and **modern package functions** in a way that introduces reliability and maintenance risk.

## What is good

1. **Clear package metadata and scope in `DESCRIPTION`** (modernized package definition, explicit dependencies, testthat edition).  
2. **Presence of focused unit tests** in `tests/testthat/` for merge behavior.  
3. **Newer modular files** (`*_functions.R`, `lyon_consensus.R`, `chicago_classification.R`) are cleaner and more documented than historical monolith files.

## Key issues and why they matter

### 1) Duplicate function names across legacy and modern files (high risk)

Functions such as `dataImpClean`, `dataBRAVOClean`, `dataBRAVODayLabeller`, `dataImpSymptoms`, `HRMCleanUp1`, `HRMDiagnoses`, `testMerge`, `GORD_AcidImp`, and `GORD_BravoWDAAndAverage` are defined in both old and new files.

- Modern versions exist in files like `R/impedance_functions.R` and `R/hrm_functions.R`.
- Legacy versions also exist in `R/ImpedanceFunctions.R` and `R/MotilityFunctions.R`.

This can lead to accidental masking/override depending on package collation and source order, making behavior difficult to predict and reproduce.

**Recommendation:** split legacy code into `legacy/` or archive branch, then keep only one canonical implementation per exported function.

### 2) Hidden/undeclared runtime dependency on `EndoMineR` (high risk)

`EndoMineR::SurveilTimeByRow` is called in the legacy BRAVO day-labelling function, but `EndoMineR` is not declared in `DESCRIPTION` Imports/Suggests.

This can break runtime or checks on clean environments.

**Recommendation:** either (a) declare `EndoMineR` properly, or (b) remove the legacy path and keep only the modern implementation that avoids this dependency.

### 3) README quality is currently below package maturity (medium/high)

`README.md` has multiple typos and stale framing (e.g., “Once completed...”, “dta extraction”, “wold”, “extractng”), and example formatting is malformed (inline fenced code start).

This hurts first impressions, onboarding, and trust for external users.

**Recommendation:** rewrite README around:
- installation,
- minimal reproducible examples,
- supported data schemas,
- migration notes from legacy APIs.

### 4) CI configuration appears legacy/inactive (medium)

A `.travis.yml` file is present, but Travis is no longer a common default for modern R package CI.

**Recommendation:** migrate to GitHub Actions (`r-lib/actions`) for `R CMD check`, test matrix, and coverage upload.

### 5) Deprecated and brittle idioms in legacy scripts (medium)

The legacy code uses patterns that are difficult to maintain and potentially brittle:
- broad string substitution and base coercion across many columns,
- deprecated dplyr verbs (e.g., `rename_all`, `summarise_all` in older files),
- `try(...)` used for non-critical data cleaning in legacy `HRMCleanUp1`.

**Recommendation:** keep current tidy-eval/data-validation style from modern files and remove legacy transformations after parity tests.

## Suggested 30-day cleanup plan

1. **Decide canonical API surface** (which exported names stay).  
2. **Deprecate/archive legacy files** (`R/ImpedanceFunctions.R`, `R/MotilityFunctions.R`, etc.) after adding compatibility wrappers where necessary.  
3. **Fix dependency declarations** (or remove dependency-requiring paths).  
4. **Refresh README and vignettes** to match current API and include runnable examples.  
5. **Adopt GitHub Actions CI** and require green checks before merge.  
6. **Expand tests** to assert expected behavior for every exported function and to pin edge-case date handling.

## Bottom line

The project has promising modern components, but the coexistence of old and new implementations is the main architectural weakness. Resolving duplicate definitions and dependency hygiene will immediately improve reliability, maintainability, and user confidence.
