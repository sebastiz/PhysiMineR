# ─────────────────────────────────────────────────────────────────────────────
# High-resolution manometry (HRM) data cleaning and utilities
# ─────────────────────────────────────────────────────────────────────────────

#' Clean and standardise a merged HRM dataset
#'
#' Performs the following steps on a merged HRM main + swallows data frame:
#' \enumerate{
#'   \item Replaces empty strings and "null" with \code{NA}.
#'   \item Coerces all pressure / distance / velocity metric columns to numeric.
#'   \item Converts \code{VisitDate} (or an equivalent column) to \code{Date}.
#'   \item Collapses multiple rows for the same study (same \code{HRM_Id} and
#'     \code{DistalLESfromnarescm}) by taking the first non-\code{NA} value for
#'     each column.
#'   \item Parses free-text percentage columns (e.g. \code{"failedChicago..."},
#'     \code{"panesophageal..."}) stripping any \% symbol and converting to
#'     numeric.
#' }
#'
#' @param df A data frame produced by merging the Sierra HRM main-summary export
#'   with the per-swallow export.  Must contain a column \code{HRM_Id}.
#' @param date_col Name of the visit-date column (default \code{"VisitDate"}).
#'
#' @return A cleaned data frame with one row per HRM study.
#'
#' @examples
#' \dontrun{
#' main    <- read.csv("HRMImportMain.csv")
#' swallows <- read.csv("HRMImportSwallows.csv")
#' # Remove per-swallow numeric columns before merging
#' swallows_clean <- swallows[, !grepl("Num\\d+", names(swallows))]
#' merged  <- merge(main, swallows_clean, by = "HRM_Id", all = TRUE)
#' clean   <- HRMCleanUp1(merged)
#' nrow(clean)  # One row per HRM study
#' }
#'
#' @export
HRMCleanUp1 <- function(df, date_col = "VisitDate") {
  check_cols(df, "HRM_Id")
  df <- null_to_na(df)

  if (date_col %in% names(df)) {
    df[[date_col]] <- to_date(df[[date_col]])
  }

  # Strip % signs from percentage-style text columns
  pct_cols <- grep("percent|fraction|Pression|pressuri|failed|break|premature|
                    rapid|simultaneous|panesoph",
                   names(df), value = TRUE, ignore.case = TRUE)
  df[pct_cols] <- lapply(df[pct_cols], function(x) {
    to_numeric(gsub("%", "", as.character(x)))
  })

  # Coerce all remaining character columns that look numeric
  metric_cols <- grep(
    "mmHg|cms|cm$|velocity|latency|DCI|IRP|LES|UES|residual|basal|
     contraction|integral|front|intra",
    names(df), value = TRUE, ignore.case = TRUE
  )
  df[metric_cols] <- lapply(df[metric_cols], to_numeric)

  # Collapse to one row per study: first non-NA value wins
  first_nonNA <- function(x) {
    nona <- x[!is.na(x) & x != ""]
    if (length(nona) == 0L) return(NA)
    nona[1L]
  }

  id_vars <- intersect(c("HRM_Id", "HospNum_Id", date_col), names(df))
  df <- df %>%    dplyr::group_by(dplyr::across(dplyr::all_of(id_vars))) %>%    dplyr::summarise(
      dplyr::across(dplyr::everything(), first_nonNA),
      .groups = "drop"
    )

  df
}


#' Summarise per-swallow HRM data to study-level metrics
#'
#' Takes the raw per-swallow export from Sierra (one row per swallow) and
#' returns one row per study with aggregated metrics:
#' \itemize{
#'   \item Mean, median and range of DCI, DL, and CFV.
#'   \item Percentage of swallows classified as failed, weak (small break),
#'     weak (large break), and hypercontractile per CC v4 thresholds.
#'   \item Percentage of premature contractions (DL < 4.5 s).
#' }
#'
#' @param df A data frame of per-swallow HRM data with columns
#'   \code{HRM_Id}, \code{DCImmHgcms}, \code{DistallatencyS},
#'   and \code{ContractilefrontvelocityCms}.
#' @param dci_col   Name of the DCI column (default \code{"DCImmHgcms"}).
#' @param dl_col    Name of the distal-latency column (default
#'   \code{"DistallatencyS"}).
#' @param cfv_col   Name of the contractile-front-velocity column (default
#'   \code{"ContractilefrontvelocityCms"}).
#' @param break_col Name of the break-size column in cm (default \code{"BreakCm"}).
#'
#' @return A data frame with one row per \code{HRM_Id} and aggregated columns.
#'
#' @examples
#' \dontrun{
#' swallows <- read.csv("HRMImportSwallows.csv")
#' summary  <- HRMSwallowSummary(swallows)
#' head(summary)
#' }
#'
#' @export
HRMSwallowSummary <- function(df,
                               dci_col   = "DCImmHgcms",
                               dl_col    = "DistallatencyS",
                               cfv_col   = "ContractilefrontvelocityCms",
                               break_col = "BreakCm") {

  check_cols(df, "HRM_Id")

  # Ensure numeric
  for (col in c(dci_col, dl_col, cfv_col)) {
    if (col %in% names(df)) df[[col]] <- to_numeric(df[[col]])
  }

  dci_present  <- dci_col  %in% names(df)
  dl_present   <- dl_col   %in% names(df)
  cfv_present  <- cfv_col  %in% names(df)
  brk_present  <- break_col %in% names(df)

  df %>%    dplyr::group_by(.data$HRM_Id) %>%    dplyr::summarise(
      n_swallows = dplyr::n(),
      # DCI metrics
      DCI_mean   = if (dci_present) mean(.data[[dci_col]],  na.rm = TRUE) else NA_real_,
      DCI_median = if (dci_present) stats::median(.data[[dci_col]], na.rm = TRUE) else NA_real_,
      pct_failed       = if (dci_present) mean(.data[[dci_col]] < 100,  na.rm = TRUE) * 100 else NA_real_,
      pct_weak         = if (dci_present) mean(.data[[dci_col]] >= 100 & .data[[dci_col]] < 450, na.rm = TRUE) * 100 else NA_real_,
      pct_hypercontract = if (dci_present) mean(.data[[dci_col]] > 8000, na.rm = TRUE) * 100 else NA_real_,
      # DL metrics
      DL_mean    = if (dl_present)  mean(.data[[dl_col]],   na.rm = TRUE) else NA_real_,
      pct_premature = if (dl_present) mean(.data[[dl_col]] < 4.5, na.rm = TRUE) * 100 else NA_real_,
      # CFV
      CFV_mean   = if (cfv_present) mean(.data[[cfv_col]],  na.rm = TRUE) else NA_real_,
      # Breaks
      pct_large_break = if (brk_present) mean(.data[[break_col]] >= 5, na.rm = TRUE) * 100 else NA_real_,
      pct_small_break = if (brk_present) mean(.data[[break_col]] > 0 & .data[[break_col]] < 5, na.rm = TRUE) * 100 else NA_real_,
      .groups = "drop"
    )
}
