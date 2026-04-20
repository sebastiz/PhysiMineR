# ─────────────────────────────────────────────────────────────────────────────
# Symptom extraction and SAP / SI classification helpers
# ─────────────────────────────────────────────────────────────────────────────

#' Classify symptom association as positive or negative per Lyon Consensus
#'
#' Converts raw Symptom Association Probability (SAP) and Symptom Index (SI)
#' values into a tidy binary/factor classification.
#'
#' Lyon Consensus 2.0 thresholds:
#' \itemize{
#'   \item SAP ≥ 95\% → positive symptom-reflux association.
#'   \item SI  ≥ 50\% → positive symptom index.
#' }
#'
#' @param df A data frame containing SAP and/or SI columns.
#' @param sap_threshold SAP threshold for a positive association.  Default
#'   \strong{95}.
#' @param si_threshold  SI threshold for a positive association.  Default
#'   \strong{50}.
#' @param prefix_sap Column-name prefix identifying SAP columns.  Default
#'   \code{"SAPTotal"}.
#' @param prefix_si  Column-name prefix identifying SI columns.  Default
#'   \code{"SITotal"}.
#'
#' @return \code{df} with additional binary columns
#'   \code{SAP_positive_<symptom>} and \code{SI_positive_<symptom>}, and
#'   summary columns:
#' \describe{
#'   \item{\code{AnySAP_positive}}{1 if any symptom SAP ≥ 95\%.}
#'   \item{\code{AnySI_positive}}{1 if any symptom SI ≥ 50\%.}
#'   \item{\code{SymptomAssoc_positive}}{1 if either AnySAP or AnySI positive.}
#' }
#'
#' @references
#' Gyawali CP, Kahrilas PJ, Savarino E, et al. (2021).
#' Modern diagnosis of GERD: the Lyon Consensus.
#' \emph{Gut}, 70(7), 1351–1362.
#'
#' @examples
#' df <- data.frame(
#'   SAPTotalHeartburn     = c(97, 80, 50),
#'   SAPTotalRegurgitation = c(40, 96, NA),
#'   SITotalHeartburn      = c(55, 30, 60)
#' )
#' out <- classifySymptomAssociation(df)
#' out[, c("AnySAP_positive", "AnySI_positive", "SymptomAssoc_positive")]
#'
#' @export
classifySymptomAssociation <- function(df,
                                        sap_threshold = 95,
                                        si_threshold  = 50,
                                        prefix_sap    = "SAPTotal",
                                        prefix_si     = "SITotal") {
  sap_cols <- grep(paste0("^", prefix_sap), names(df), value = TRUE)
  si_cols  <- grep(paste0("^", prefix_si),  names(df), value = TRUE)

  # Binary indicator per symptom
  for (col in sap_cols) {
    vals <- to_numeric(df[[col]])
    suffix <- sub(prefix_sap, "", col)
    df[[paste0("SAP_positive_", suffix)]] <- as.integer(!is.na(vals) & vals >= sap_threshold)
  }
  for (col in si_cols) {
    vals <- to_numeric(df[[col]])
    suffix <- sub(prefix_si, "", col)
    df[[paste0("SI_positive_", suffix)]]  <- as.integer(!is.na(vals) & vals >= si_threshold)
  }

  # Summary columns
  sap_bin_cols <- grep("^SAP_positive_", names(df), value = TRUE)
  si_bin_cols  <- grep("^SI_positive_",  names(df), value = TRUE)

  df$AnySAP_positive <- if (length(sap_bin_cols) > 0)
    as.integer(rowSums(df[sap_bin_cols], na.rm = TRUE) > 0)
  else NA_integer_

  df$AnySI_positive <- if (length(si_bin_cols) > 0)
    as.integer(rowSums(df[si_bin_cols], na.rm = TRUE) > 0)
  else NA_integer_

  df$SymptomAssoc_positive <- as.integer(
    (!is.na(df$AnySAP_positive) & df$AnySAP_positive == 1) |
    (!is.na(df$AnySI_positive)  & df$AnySI_positive  == 1)
  )

  df
}


#' Extract symptom presence from free-text fields
#'
#' Searches one or more free-text columns for a predefined list of oesophageal
#' symptoms, returning a binary column per symptom.  Matching is
#' case-insensitive.
#'
#' @param df A data frame.
#' @param text_cols Character vector of column names to search.  If more than
#'   one column is specified, all are searched and results are combined with
#'   OR logic.
#' @param symptoms A named character vector where \strong{names} are the output
#'   column names and \strong{values} are regular expressions to match.
#'   Defaults to a standard oesophageal symptom list.
#'
#' @return \code{df} with one binary column per symptom (1 = present, 0 =
#'   absent / not mentioned).
#'
#' @examples
#' df <- data.frame(
#'   ClinicalNote = c("Patient reports heartburn and regurgitation",
#'                    "Chronic cough, no heartburn",
#'                    "Dysphagia to solids")
#' )
#' extractSymptoms(df, text_cols = "ClinicalNote")
#'
#' @export
extractSymptoms <- function(
    df,
    text_cols = NULL,
    symptoms  = c(
      Heartburn      = "heartburn|pyrosis",
      Regurgitation  = "regurgit",
      Dysphagia      = "dysphagia|difficulty swallow",
      ChestPain      = "chest pain|chest discomfort",
      Belching       = "belch|burp|eructat",
      Cough          = "cough",
      ThroatSymptoms = "throat|globus|lump in throat|hoarse",
      Nausea         = "nausea",
      Vomiting       = "vomit",
      StomachPain    = "epigastric|stomach pain|abdominal pain"
    )
) {
  if (is.null(text_cols)) {
    warning("No text_cols supplied; returning df unchanged.")
    return(df)
  }
  check_cols(df, text_cols)

  # Combine all text columns into one search string per row
  combined_text <- apply(
    df[text_cols], 1,
    function(r) tolower(paste(r, collapse = " "))
  )

  for (sx_name in names(symptoms)) {
    pattern <- symptoms[[sx_name]]
    df[[paste0("Sx_", sx_name)]] <- as.integer(
      grepl(pattern, combined_text, ignore.case = TRUE)
    )
  }

  df
}


#' Compute symptom burden summary statistics
#'
#' Tallies the number of distinct symptom types reported and categorises the
#' overall symptom burden.
#'
#' @param df A data frame with binary symptom columns (prefixed \code{"Sx_"}).
#'
#' @return \code{df} with columns:
#' \describe{
#'   \item{\code{nSymptoms}}{Number of distinct symptom types reported (≥ 1).}
#'   \item{\code{symptomBurden}}{Factor: \code{"None"}, \code{"Mild"} (1),
#'     \code{"Moderate"} (2–3), \code{"Severe"} (≥ 4).}
#' }
#'
#' @examples
#' df <- data.frame(Sx_Heartburn = c(1,0,1), Sx_Cough = c(0,1,1),
#'                  Sx_Regurgitation = c(1,0,1))
#' symptomBurdenSummary(df)
#'
#' @export
symptomBurdenSummary <- function(df) {
  sx_cols <- grep("^Sx_", names(df), value = TRUE)
  if (length(sx_cols) == 0L) {
    warning("No 'Sx_*' columns found.")
    return(df)
  }

  df$nSymptoms <- rowSums(df[sx_cols] == 1, na.rm = TRUE)
  df$symptomBurden <- cut(df$nSymptoms,
                           breaks = c(-Inf, 0, 1, 3, Inf),
                           labels = c("None", "Mild", "Moderate", "Severe"),
                           right  = TRUE)
  df
}
