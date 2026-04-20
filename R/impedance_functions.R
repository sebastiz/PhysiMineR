# ─────────────────────────────────────────────────────────────────────────────
# Ambulatory pH-impedance monitoring functions
# ─────────────────────────────────────────────────────────────────────────────

#' Clean a merged pH-impedance dataset
#'
#' Standardises a data frame produced by merging the Sierra impedance export
#' (Impedance2) with the symptom export (Imp_Symp).  Performs:
#' \enumerate{
#'   \item Null / empty-string → \code{NA} substitution.
#'   \item Numeric coercion for all reflux metric columns.
#'   \item Date coercion for visit-date columns.
#'   \item Creation of binary indicator columns for each symptom type
#'     (\code{0}/\code{1}) based on whether any SAP or SI value was recorded.
#' }
#'
#' @param df A data frame containing merged impedance and symptom data.
#'   Must include a column \code{Imp_Id}.
#' @param date_col Name of the visit-date column (default \code{"VisitDate"}).
#'
#' @return A cleaned data frame with one row per impedance study.
#'
#' @examples
#' \dontrun{
#' imp  <- read.csv("Impedance2.csv")
#' symp <- read.csv("Imp_Symp.csv")
#' raw  <- merge(imp, symp, by = "Imp_Id", all = TRUE)
#' clean <- dataImpClean(raw)
#' }
#'
#' @export
dataImpClean <- function(df, date_col = "VisitDate") {
  check_cols(df, "Imp_Id")
  df <- null_to_na(df)

  if (date_col %in% names(df)) df[[date_col]] <- to_date(df[[date_col]])

  # Numeric coercion: reflux metrics, SAP, SI
  num_pattern <- "AcidExp|Episode|Clearance|DeMeester|Composite|SAP|^SI|Fraction|Longest|Bolus|Proximal"
  num_cols    <- grep(num_pattern, names(df), value = TRUE, ignore.case = TRUE)
  df[num_cols] <- lapply(df[num_cols], to_numeric)

  # Create binary symptom indicators (1 if any SAP or SI column is non-NA)
  symptom_types <- c("Heartburn", "ChestPain", "Regurgitation",
                     "Belch", "Cough", "Throat", "Epigastric",
                     "Vomiting", "Nausea", "Other")

  for (sx in symptom_types) {
    sap_col <- paste0("SAPTotal", sx)
    si_col  <- paste0("SITotal", sx)
    ind_col <- paste0("Sx_", sx)
    has_sap <- sap_col %in% names(df) && any(!is.na(df[[sap_col]]))
    has_si  <- si_col  %in% names(df) && any(!is.na(df[[si_col]]))
    if (has_sap || has_si) {
      sap_val <- if (has_sap) to_numeric(df[[sap_col]]) else rep(NA_real_, nrow(df))
      si_val  <- if (has_si)  to_numeric(df[[si_col]])  else rep(NA_real_, nrow(df))
      df[[ind_col]] <- as.integer(!is.na(sap_val) | !is.na(si_val))
    }
  }

  df
}


#' Categorise symptoms from a cleaned impedance dataset
#'
#' Aggregates individual symptom indicators into three clinically meaningful
#' groups:
#' \describe{
#'   \item{\code{AllImpSymptom}}{All symptom types concatenated.}
#'   \item{\code{AllSymps_Impgrouped}}{Broad group: \code{"Oesophageal"},
#'     \code{"LPR"}, \code{"Mixed"}, or \code{"Other"}.}
#'   \item{\code{SAPOesophageal}}{Maximum SAP across oesophageal symptoms.}
#'   \item{\code{SAPLPR}}{Maximum SAP across LPR symptoms.}
#' }
#'
#' @param df A cleaned impedance data frame (output of
#'   \code{\link{dataImpClean}}).
#'
#' @return \code{df} with additional grouped symptom columns.
#'
#' @examples
#' \dontrun{
#' clean    <- dataImpClean(raw)
#' with_sx  <- dataImpSymptoms(clean)
#' table(with_sx$AllSymps_Impgrouped)
#' }
#'
#' @export
dataImpSymptoms <- function(df) {
  oeso_sx <- c("Heartburn", "Regurgitation", "ChestPain",
               "Belch", "Vomiting", "Nausea", "Epigastric")
  lpr_sx  <- c("Cough", "Throat")

  # Gather SAP values per group
  sap_oeso <- grep(paste0("SAPTotal(", paste(oeso_sx, collapse = "|"), ")"),
                   names(df), value = TRUE)
  sap_lpr  <- grep(paste0("SAPTotal(", paste(lpr_sx,  collapse = "|"), ")"),
                   names(df), value = TRUE)

  df$SAPOesophageal <- if (length(sap_oeso) > 0)
    apply(df[sap_oeso], 1, max, na.rm = TRUE)
  else NA_real_

  df$SAPLPR <- if (length(sap_lpr) > 0)
    apply(df[sap_lpr], 1, max, na.rm = TRUE)
  else NA_real_

  # Replace -Inf from all-NA rows
  df$SAPOesophageal[is.infinite(df$SAPOesophageal)] <- NA_real_
  df$SAPLPR[is.infinite(df$SAPLPR)] <- NA_real_

  # Group
  has_oeso <- !is.na(df$SAPOesophageal) & df$SAPOesophageal >= 95
  has_lpr  <- !is.na(df$SAPLPR)         & df$SAPLPR         >= 95

  df$AllSymps_Impgrouped <- dplyr::case_when(
    has_oeso & has_lpr ~ "Mixed",
    has_oeso           ~ "Oesophageal",
    has_lpr            ~ "LPR",
    TRUE               ~ "Other"
  )

  df$AllImpSymptom <- apply(df[grep("^Sx_", names(df))], 1, function(r) {
    paste(names(r)[!is.na(r) & r == 1], collapse = ",")
  })

  df
}


#' Extract Mean Nocturnal Baseline Impedance (MNBI) and PSPW index
#'
#' Merges MNBI and PSPW data into the main impedance data frame, ready for
#' use with \code{\link{GORD_AcidImp_Lyon}} as adjunctive metrics.
#'
#' @param imp_df  Main impedance data frame (must contain \code{HospNum_Id}).
#' @param mnbi_df Data frame with MNBI values (must contain \code{HospNum_Id}
#'   and a column named \code{MethodOne_average} or \code{M1_av}).
#' @param pspw_df Data frame with PSPW index (must contain \code{HospNum_Id}
#'   and a column named \code{pspw_index}).
#' @param id_col  Name of the shared patient identifier column (default
#'   \code{"HospNum_Id"}).
#'
#' @return \code{imp_df} with columns \code{M1_av} (MNBI) and \code{pspw_index}
#'   merged in.
#'
#' @examples
#' \dontrun{
#' mnbi  <- read_excel("MNBI.xlsx")
#' mnbi2 <- read_excel("MNBI2.xlsx")
#' mnbi_all <- rbind(mnbi, mnbi2)
#' pspw  <- readxl::read_xlsx("Seb_PSPW_list.xlsx") %>%#'           dplyr::rename(HospNum_Id = HospitalNum)
#' imp_with_adj <- addAdjunctiveMetrics(clean_imp, mnbi_all, pspw)
#' }
#'
#' @export
addAdjunctiveMetrics <- function(imp_df,
                                  mnbi_df = NULL,
                                  pspw_df = NULL,
                                  id_col  = "HospNum_Id") {
  check_cols(imp_df, id_col)

  if (!is.null(mnbi_df)) {
    check_cols(mnbi_df, id_col)
    # Standardise column name
    if ("MethodOne_average" %in% names(mnbi_df) && !"M1_av" %in% names(mnbi_df)) {
      mnbi_df <- dplyr::rename(mnbi_df, M1_av = "MethodOne_average")
    }
    keep_mnbi <- intersect(c(id_col, "M1_av", "pspw_index", "comments"),
                           names(mnbi_df))
    mnbi_slim <- dplyr::distinct(mnbi_df[keep_mnbi])
    imp_df <- dplyr::left_join(imp_df, mnbi_slim, by = id_col)
  }

  if (!is.null(pspw_df) && !"pspw_index" %in% names(imp_df)) {
    check_cols(pspw_df, id_col)
    keep_pspw <- intersect(c(id_col, "pspw_index"), names(pspw_df))
    pspw_slim <- dplyr::distinct(pspw_df[keep_pspw])
    imp_df <- dplyr::left_join(imp_df, pspw_slim, by = id_col)
  }

  if ("M1_av" %in% names(imp_df))    imp_df$M1_av      <- to_numeric(imp_df$M1_av)
  if ("pspw_index" %in% names(imp_df)) imp_df$pspw_index <- to_numeric(imp_df$pspw_index)

  imp_df
}
