# ─────────────────────────────────────────────────────────────────────────────
# BRAVO wireless pH capsule functions
# ─────────────────────────────────────────────────────────────────────────────

#' Clean and standardise a merged BRAVO dataset
#'
#' Converts date columns, coerces pH metric columns to numeric, and removes
#' duplicate rows within the same study (identified by \code{BravoID}).
#'
#' @param df A data frame produced by merging the day-1/2 and day-3/4 BRAVO
#'   exports together with the BRAVO totals export.  Must contain a column
#'   named \code{BravoID}.
#' @param date_col Name of the visit-date column (default \code{"VisitDate"}).
#'
#' @return The cleaned data frame with standardised column types.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' day12 <- read.csv("BravoDay1And2.csv")
#' day34 <- read.csv("BravoDay3And4.csv")
#' total <- read.csv("BRAVOTotal.csv")
#' raw   <- day12 %>% full_join(day34, by = "BravoID") %>%#'                   full_join(total,  by = "BravoID")
#' clean <- dataBRAVOClean(raw)
#' }
#'
#' @export
dataBRAVOClean <- function(df, date_col = "VisitDate") {
  check_cols(df, "BravoID")
  df <- null_to_na(df)

  # Coerce date
  if (date_col %in% names(df)) {
    df[[date_col]] <- to_date(df[[date_col]])
  }

  # Coerce all pH fraction / symptom numeric columns
  ph_pattern <- "FractionTime|DeMeester|SAP|^SI|Episode|Longest|Composite"
  num_cols   <- grep(ph_pattern, names(df), value = TRUE)
  df[num_cols] <- lapply(df[num_cols], to_numeric)

  # Deduplicate within study (keep first row per BravoID)
  df <- df[!duplicated(df$BravoID), ]

  df
}


#' Label BRAVO study days across merged day-level columns
#'
#' The raw BRAVO export uses inconsistent column naming (e.g.
#' \code{ReflDay1…}, \code{ReflDay1_2…}, \code{ReflDay2_2…}).  This function
#' standardises these to \code{bravoDay1}, \code{bravoDay2}, \ldots
#' \code{bravoDay6} by detecting which columns contain per-day acid-exposure
#' time (AET) values and renaming them in chronological order.
#'
#' @param df A cleaned BRAVO data frame (output of \code{\link{dataBRAVOClean}}).
#' @param id_col   Name of the patient-identifier column (default
#'   \code{"HospNum_Id"}).
#' @param date_col Name of the visit-date column (default \code{"VisitDate"}).
#'
#' @return \code{df} with additional columns \code{bravoDay1} … \code{bravoDay6}
#'   (AET % for each monitored day) and \code{bravoNDays} (number of days
#'   with a recorded AET value).
#'
#' @examples
#' \dontrun{
#' labelled <- dataBRAVODayLabeller(clean, "HospNum_Id", "VisitDate")
#' head(labelled[, c("HospNum_Id", "bravoDay1", "bravoDay2", "bravoNDays")])
#' }
#'
#' @export
dataBRAVODayLabeller <- function(df,
                                  id_col   = "HospNum_Id",
                                  date_col = "VisitDate") {
  # Canonical day column pool (in chronological order as exported by Sierra)
  day_pool <- c(
    "ReflDay1FractionTimepHLessThan4Total",
    "ReflDay2FractionTimepHLessThan4Total",
    "ReflDay1_2FractionTimepHLessThan4Total",
    "ReflDay2_2FractionTimepHLessThan4Total",
    "ReflDay3_2FractionTimepHLessThan4Total",
    "ReflDay4_2FractionTimepHLessThan4Total"
  )

  present <- intersect(day_pool, names(df))
  if (length(present) == 0L) {
    warning("No BRAVO day-level AET columns detected; day labelling skipped.")
    return(df)
  }

  # Ensure numeric
  df[present] <- lapply(df[present], to_numeric)

  # Map to standardised names
  day_names <- paste0("bravoDay", seq_along(present))
  for (i in seq_along(present)) {
    df[[day_names[i]]] <- df[[present[i]]]
  }

  df$bravoNDays <- rowSums(!is.na(df[day_names]))
  df
}


#' Extract and categorise BRAVO symptom entries
#'
#' Parses the free-text symptom column exported from the BRAVO system and
#' returns tidy columns: \code{AllSymps_BRAVO} (comma-separated unique
#' symptom list), \code{AllSymps_BRAVOgrouped} (broad category), and
#' \code{AllSymps_BRAVOcompartment} (anatomical compartment).
#'
#' @param df A data frame with a column \code{Symptoms} containing raw BRAVO
#'   symptom text (multiple entries may be pipe- or comma-separated).
#' @param symp_col Name of the raw symptom column (default \code{"Symptoms"}).
#'
#' @return \code{df} with three additional symptom columns.
#'
#' @examples
#' \dontrun{
#' df$Symptoms <- "Heartburn|Regurgitation"
#' df <- dataBRAVOSymptoms(df)
#' df[, c("AllSymps_BRAVO", "AllSymps_BRAVOgrouped")]
#' }
#'
#' @export
dataBRAVOSymptoms <- function(df, symp_col = "Symptoms") {
  if (!symp_col %in% names(df)) {
    warning("Column '", symp_col, "' not found; symptom extraction skipped.")
    df$AllSymps_BRAVO           <- NA_character_
    df$AllSymps_BRAVOgrouped    <- NA_character_
    df$AllSymps_BRAVOcompartment <- NA_character_
    return(df)
  }

  oesophageal_sx <- c("Heartburn", "Regurgitation", "ChestPain",
                       "Belch", "Vomiting", "Nausea")
  lpr_sx         <- c("Cough", "Throat", "Hoarseness", "Globus")

  parse_row <- function(raw) {
    if (is.na(raw) || trimws(raw) == "") return(rep(NA_character_, 3))
    syms <- unique(trimws(unlist(strsplit(as.character(raw), "[|,;]"))))
    syms <- syms[nzchar(syms)]
    syms <- gsub("Epigastric", "StomachPain", syms)
    all_str <- paste(sort(syms), collapse = ",")
    grouped <- dplyr::case_when(
      any(syms %in% oesophageal_sx) & any(syms %in% lpr_sx) ~ "Mixed",
      any(syms %in% oesophageal_sx)                          ~ "Oesophageal",
      any(syms %in% lpr_sx)                                  ~ "LPR",
      TRUE                                                    ~ "Other"
    )
    compartment <- dplyr::case_when(
      grouped == "LPR"        ~ "Supraesophageal",
      grouped == "Oesophageal" ~ "Oesophageal",
      grouped == "Mixed"      ~ "Mixed",
      TRUE                    ~ "Other"
    )
    c(all_str, grouped, compartment)
  }

  parsed <- t(vapply(df[[symp_col]], parse_row, character(3)))
  df$AllSymps_BRAVO            <- parsed[, 1]
  df$AllSymps_BRAVOgrouped     <- parsed[, 2]
  df$AllSymps_BRAVOcompartment <- parsed[, 3]
  df
}


#' Compute worst-day and average AET across monitored BRAVO days
#'
#' For each patient, calculates:
#' \itemize{
#'   \item \code{worstt} – the highest single-day AET (%)
#'   \item \code{average} – the mean AET across all available days
#'   \item \code{worstDaypH} – which calendar day had the worst AET
#'   \item \code{NumDaysBravoPositive} – number of days with AET > the threshold
#'   \item \code{WorstD_ayAnalysisGORDPositive} – 1 if worst-day AET exceeds
#'     the threshold, 0 otherwise
#' }
#'
#' @param df A labelled BRAVO data frame (output of
#'   \code{\link{dataBRAVODayLabeller}}).
#' @param n_days Total number of monitored days exported (typically 4 for the
#'   standard 96-hour BRAVO protocol).
#' @param aet_threshold AET threshold (%) used to define a "positive" day.
#'   Per Lyon Consensus 2.0, this is \strong{6\%} (default).
#'
#' @return \code{df} augmented with the columns described above.
#'
#' @references
#' Gyawali CP et al. (2021). Modern diagnosis of GERD: the Lyon Consensus.
#' \emph{Gut}, 70(7), 1351–1362. \doi{10.1136/gutjnl-2020-323270}
#'
#' @examples
#' \dontrun{
#' labelled <- dataBRAVODayLabeller(clean)
#' result   <- GORD_BravoWDAAndAverage(labelled, n_days = 4)
#' table(result$WorstD_ayAnalysisGORDPositive)
#' }
#'
#' @export
GORD_BravoWDAAndAverage <- function(df, n_days = 4, aet_threshold = 6) {
  day_cols <- paste0("bravoDay", seq_len(n_days))
  day_cols <- intersect(day_cols, names(df))

  if (length(day_cols) == 0L) {
    stop("No standardised bravoDay* columns found. ",
         "Run dataBRAVODayLabeller() first.")
  }

  mat <- as.matrix(df[day_cols])
  mode(mat) <- "numeric"

  df$worstt   <- apply(mat, 1, max,  na.rm = TRUE)
  df$average  <- rowMeans(mat, na.rm = TRUE)
  df$worstt[is.infinite(df$worstt)] <- NA

  # Which day was worst?
  worst_idx <- apply(mat, 1, function(r) {
    idx <- which(r == max(r, na.rm = TRUE))
    if (length(idx) == 0L) return(NA_character_)
    paste0("Day", idx[1])
  })
  df$worstDaypH <- worst_idx

  df$NumDaysBravoPositive         <- rowSums(mat > aet_threshold, na.rm = TRUE)
  df$WorstD_ayAnalysisGORDPositive <- as.integer(df$worstt > aet_threshold)
  df
}
