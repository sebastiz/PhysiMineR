# ─────────────────────────────────────────────────────────────────────────────
# Chicago Classification v4.0 oesophageal motility diagnosis
# Reference: Yadlapati R et al. Neurogastroenterol Motil 2021;33:e14058
# ─────────────────────────────────────────────────────────────────────────────

#' Apply Chicago Classification v4.0 to HRM summary metrics
#'
#' Assigns a motility diagnosis to each HRM study using the hierarchical
#' decision algorithm defined in Chicago Classification version 4.0 (CC v4).
#'
#' \strong{Diagnostic hierarchy (applied in order):}
#' \enumerate{
#'   \item \strong{Achalasia Type I} – IRP > ULN AND 100\% failed peristalsis
#'     (DCI < 100) AND no pan-oesophageal pressurisation.
#'   \item \strong{Achalasia Type II} – IRP > ULN AND ≥ 20\% swallows with
#'     pan-oesophageal pressurisation.
#'   \item \strong{Achalasia Type III} – IRP > ULN AND ≥ 20\% premature
#'     contractions (DL < 4.5 s), with some preserved peristaltic fragments.
#'   \item \strong{EGJ Outflow Obstruction (EGJOO)} – IRP > ULN without
#'     meeting Types I–III criteria.
#'   \item \strong{Distal Oesophageal Spasm (DES)} – Normal IRP AND ≥ 20\%
#'     premature contractions.
#'   \item \strong{Hypercontractile (Jackhammer) Oesophagus} – ≥ 20\% swallows
#'     with DCI > 8000 mmHg·cm·s (IRP may be elevated or normal).
#'   \item \strong{Absent Contractility} – Normal / low IRP AND 100\% failed
#'     peristalsis.
#'   \item \strong{Ineffective Oesophageal Motility (IEM)} – ≥ 70\% ineffective
#'     swallows (failed DCI < 100 OR weak DCI < 450).
#'   \item \strong{Fragmented Peristalsis} – ≥ 50\% fragmented contractions
#'     (large break ≥ 5 cm) AND mean DCI > 100.
#'   \item \strong{Normal} – None of the above criteria are met.
#' }
#'
#' All percentage columns should be expressed as proportions (0–100), not
#' fractions (0–1).
#'
#' @param df A data frame of HRM summary metrics (one row per study).
#' @param irp_col Name of the mean Integrated Relaxation Pressure column
#'   (default \code{"ResidualmeanmmHg"}).
#' @param irp_uln Upper limit of normal for IRP (mmHg).  The value depends on
#'   the HRM system used: \strong{15} for Sierra Scientific / Medtronic
#'   (default), 12 for Diversatek.
#' @param failed_col Name of the column containing the percentage of failed
#'   peristaltic swallows (DCI < 100 mmHg·cm·s).
#'   Default \code{"failedChicagoClassification"}.
#' @param panoesopha_col Name of the column for pan-oesophageal pressurisation
#'   percentage.  Default \code{"panesophagealpressurization"}.
#' @param premature_col Name of the column for premature contraction percentage
#'   (DL < 4.5 s).  Default \code{"prematurecontraction"}.
#' @param hypercontract_col Name of the column for the percentage of
#'   hypercontractile swallows (DCI > 8000 mmHg·cm·s).
#'   Default \code{"rapidcontraction"}.  Note: rename your column if Sierra
#'   exports it under a different name.
#' @param dci_mean_col Name of the mean DCI column.
#'   Default \code{"DistalcontractileintegralmeanmmHgcms"}.
#' @param large_breaks_col Name of the large-break (≥ 5 cm) percentage column.
#'   Default \code{"largebreaks"}.
#' @param small_breaks_col Name of the small-break (< 5 cm) percentage column.
#'   Default \code{"smallbreaks"}.
#' @param simultaneous_col Name of the simultaneous-contraction column (used
#'   as a proxy for some premature-contraction exports).
#'   Default \code{"Simultaneous"}.
#'
#' @return \code{df} with two new columns:
#' \describe{
#'   \item{\code{ChicagoV4Diagnosis}}{Character string with the assigned
#'     diagnosis.}
#'   \item{\code{ChicagoV4DiagnosisGroup}}{Broad group: \code{"Achalasia"},
#'     \code{"EGJ Outflow"}, \code{"Major Peristalsis Disorder"},
#'     \code{"Minor Peristalsis Disorder"}, or \code{"Normal"}.}
#' }
#'
#' @references
#' Yadlapati R, Kahrilas PJ, Fox MR, et al. (2021).
#' Esophageal motility disorders on high-resolution manometry: Chicago
#' Classification version 4.0.
#' \emph{Neurogastroenterology & Motility}, 33(1), e14058.
#' \doi{10.1111/nmo.14058}
#'
#' @examples
#' df <- data.frame(
#'   HRM_Id               = c("H001", "H002", "H003", "H004", "H005"),
#'   ResidualmeanmmHg      = c(20,     18,     8,      6,      10   ),
#'   failedChicagoClassification = c(100,  30, 0,     100,     80  ),
#'   panesophagealpressurization = c(0,    25, 0,      0,       0  ),
#'   prematurecontraction        = c(0,     0, 25,     0,       0  ),
#'   rapidcontraction            = c(0,     0,  0,     0,       5  ),
#'   DistalcontractileintegralmeanmmHgcms = c(50, 300, 1200, 60, 200),
#'   largebreaks                 = c(0,     0,  0,     0,      55  ),
#'   smallbreaks                 = c(0,     0,  0,     0,       0  ),
#'   Simultaneous                = c(0,     0,  0,     0,       0  )
#' )
#' result <- HRMDiagnoses(df)
#' result[, c("HRM_Id", "ChicagoV4Diagnosis", "ChicagoV4DiagnosisGroup")]
#'
#' @export
HRMDiagnoses <- function(df,
                          irp_col            = "ResidualmeanmmHg",
                          irp_uln            = 15,
                          failed_col         = "failedChicagoClassification",
                          panoesopha_col     = "panesophagealpressurization",
                          premature_col      = "prematurecontraction",
                          hypercontract_col  = "rapidcontraction",
                          dci_mean_col       = "DistalcontractileintegralmeanmmHgcms",
                          large_breaks_col   = "largebreaks",
                          small_breaks_col   = "smallbreaks",
                          simultaneous_col   = "Simultaneous") {

  # ── Helper: safely retrieve a numeric column, returning NAs if absent ──
  get_num <- function(col) {
    if (col %in% names(df)) to_numeric(df[[col]]) else rep(NA_real_, nrow(df))
  }

  irp           <- get_num(irp_col)
  pct_failed    <- get_num(failed_col)
  pct_panoesopha <- get_num(panoesopha_col)
  pct_premature <- get_num(premature_col)
  pct_hypercon  <- get_num(hypercontract_col)
  dci_mean      <- get_num(dci_mean_col)
  pct_large_brk <- get_num(large_breaks_col)
  pct_small_brk <- get_num(small_breaks_col)
  pct_simult    <- get_num(simultaneous_col)

  # Combine premature + simultaneous (some export systems differ)
  pct_premature_total <- pmax(pct_premature, pct_simult, na.rm = TRUE)

  # Ineffective = failed + weak (DCI < 450); approximate via failed + large break
  pct_ineffective <- pmin(
    100,
    rowSums(cbind(pct_failed, pct_large_brk, pct_small_brk), na.rm = TRUE)
  )

  elevated_irp <- !is.na(irp) & irp > irp_uln
  normal_irp   <- !is.na(irp) & irp <= irp_uln

  diag <- dplyr::case_when(
    # ── Disorders of EGJ outflow ──────────────────────────────────────────
    elevated_irp &
      !is.na(pct_failed) & pct_failed == 100 &
      (is.na(pct_panoesopha) | pct_panoesopha < 20) &
      (is.na(pct_premature_total) | pct_premature_total < 20)
    ~ "Achalasia Type I",

    elevated_irp &
      !is.na(pct_panoesopha) & pct_panoesopha >= 20
    ~ "Achalasia Type II",

    elevated_irp &
      !is.na(pct_premature_total) & pct_premature_total >= 20
    ~ "Achalasia Type III",

    elevated_irp
    ~ "EGJ Outflow Obstruction",

    # ── Major disorders of peristalsis ────────────────────────────────────
    normal_irp &
      !is.na(pct_premature_total) & pct_premature_total >= 20
    ~ "Distal Oesophageal Spasm",

    !is.na(pct_hypercon) & pct_hypercon >= 20
    ~ "Hypercontractile Oesophagus (Jackhammer)",

    normal_irp &
      !is.na(pct_failed) & pct_failed == 100
    ~ "Absent Contractility",

    # ── Minor disorders of peristalsis ────────────────────────────────────
    !is.na(pct_ineffective) & pct_ineffective >= 70
    ~ "Ineffective Oesophageal Motility",

    !is.na(pct_large_brk) & pct_large_brk >= 50 &
      !is.na(dci_mean) & dci_mean > 100
    ~ "Fragmented Peristalsis",

    # ── Normal ────────────────────────────────────────────────────────────
    TRUE ~ "Normal"
  )

  group <- dplyr::case_when(
    grepl("^Achalasia", diag)              ~ "Achalasia",
    diag == "EGJ Outflow Obstruction"      ~ "EGJ Outflow",
    diag %in% c("Distal Oesophageal Spasm",
                "Hypercontractile Oesophagus (Jackhammer)",
                "Absent Contractility")    ~ "Major Peristalsis Disorder",
    diag %in% c("Ineffective Oesophageal Motility",
                "Fragmented Peristalsis")  ~ "Minor Peristalsis Disorder",
    TRUE                                   ~ "Normal"
  )

  df$ChicagoV4Diagnosis      <- diag
  df$ChicagoV4DiagnosisGroup <- group
  df
}
