# ─────────────────────────────────────────────────────────────────────────────
# Lyon Consensus 2.0 acid-reflux classification
# Reference: Gyawali CP et al. Gut 2021;70:1351-1362
# ─────────────────────────────────────────────────────────────────────────────

#' Classify BRAVO wireless pH studies using Lyon Consensus 2.0 criteria
#'
#' Applies the Lyon Consensus 2.0 three-tier classification to BRAVO
#' wireless pH capsule studies (typically 96 hours / 4 days):
#'
#' \describe{
#'   \item{Conclusive GORD}{AET > 6\% on \strong{at least 2} monitored days.}
#'   \item{Inconclusive}{AET between 4\% and 6\% on the predominant day, or
#'     mixed days without meeting either conclusive threshold.}
#'   \item{GORD excluded}{AET < 4\% on \strong{all} available monitored days.}
#' }
#'
#' Two output columns are added:
#' \describe{
#'   \item{\code{AcidRefluxBRAVO}}{Binary: 1 = Conclusive GORD, 0 = not.}
#'   \item{\code{AcidRefluxBRAVO_Lyon}}{Factor with three levels:
#'     \code{"Conclusive GORD"}, \code{"Inconclusive"}, \code{"GORD excluded"}.}
#'   \item{\code{AcidRefluxBRAVOTotalOnly}}{Binary: 1 = all days < 4\%
#'     (conclusive negative), 0 = otherwise.}
#' }
#'
#' @param df A labelled BRAVO data frame.  Must contain \code{bravoDay1} …
#'   \code{bravoDay\{n\}} columns (created by \code{\link{dataBRAVODayLabeller}}).
#' @param pathological_aet AET threshold (\%) for a \emph{positive} day.
#'   Lyon Consensus 2.0 specifies \strong{6\%} (default).
#' @param normal_aet AET threshold (\%) for a \emph{normal} day.
#'   Lyon Consensus 2.0 specifies \strong{4\%} (default).
#' @param min_positive_days Minimum number of days with AET > \code{pathological_aet}
#'   required to classify as Conclusive GORD.  Default is \strong{2} per Lyon
#'   Consensus 2.0.
#'
#' @return \code{df} with three additional classification columns.
#'
#' @references
#' Gyawali CP, Kahrilas PJ, Savarino E, et al. (2021).
#' Modern diagnosis of GERD: the Lyon Consensus.
#' \emph{Gut}, 70(7), 1351–1362. \doi{10.1136/gutjnl-2020-323270}
#'
#' @examples
#' # Minimal reproducible example with synthetic data
#' df <- data.frame(
#'   HospNum_Id = c("A001", "A002", "A003", "A004"),
#'   bravoDay1  = c(7.2,  3.1,  5.0,  6.5),
#'   bravoDay2  = c(6.8,  2.9,  4.5,  5.8),
#'   bravoDay3  = c(7.5,  3.5,  4.8,  NA ),
#'   bravoDay4  = c(6.1,  3.0,  5.2,  NA ),
#'   bravoNDays = c(4,    4,    4,    2  )
#' )
#' classified <- GORD_AcidBRAVO_Lyon(df)
#' classified[, c("HospNum_Id", "AcidRefluxBRAVO_Lyon", "AcidRefluxBRAVO")]
#'
#' @export
GORD_AcidBRAVO_Lyon <- function(df,
                                 pathological_aet  = 6,
                                 normal_aet        = 4,
                                 min_positive_days = 2) {

  # Identify which bravoDay* columns are present
  day_cols <- grep("^bravoDay\\d+$", names(df), value = TRUE)

  if (length(day_cols) == 0L) {
    stop(
      "No 'bravoDay*' columns found. ",
      "Run dataBRAVODayLabeller() before calling this function."
    )
  }

  mat <- as.matrix(df[day_cols])
  mode(mat) <- "numeric"

  n_available <- rowSums(!is.na(mat))
  n_positive  <- rowSums(mat > pathological_aet, na.rm = TRUE)
  n_normal    <- rowSums(mat < normal_aet,        na.rm = TRUE)

  lyon <- dplyr::case_when(
    n_positive >= min_positive_days              ~ "Conclusive GORD",
    n_available > 0 & n_normal == n_available   ~ "GORD excluded",
    TRUE                                         ~ "Inconclusive"
  )

  df$AcidRefluxBRAVO_Lyon     <- factor(lyon,
                                         levels = c("GORD excluded",
                                                    "Inconclusive",
                                                    "Conclusive GORD"))
  df$AcidRefluxBRAVO          <- as.integer(lyon == "Conclusive GORD")
  df$AcidRefluxBRAVOTotalOnly <- as.integer(
    n_available > 0 & n_normal == n_available
  )

  # Convenience: average-day analysis positive (mean AET > 6%)
  df$AcidRefluxBRAVOAv <- as.integer(
    rowMeans(mat, na.rm = TRUE) > pathological_aet
  )

  df
}


#' Classify ambulatory pH-impedance studies using Lyon Consensus 2.0
#'
#' Determines whether a catheter-based 24-hour pH-impedance study is positive
#' for pathological acid exposure, and optionally evaluates adjunctive metrics
#' when the AET is in the inconclusive (borderline) range.
#'
#' Classification tiers:
#' \describe{
#'   \item{Conclusive GORD}{AET > 6\%.}
#'   \item{Inconclusive (borderline)}{AET 4–6\%.  Adjunctive metrics are
#'     evaluated if provided: MNBI < 2292 Ω or PSPW index < 61\% upgrade the
#'     classification to \emph{Conclusive GORD (adjunctive)}.}
#'   \item{GORD excluded}{AET < 4\%.}
#' }
#'
#' @param df A data frame containing at least the AET column.
#' @param aet_col Name of the total-AET column (default
#'   \code{"MainAcidExpTotalClearanceChannelPercentTime"}).
#' @param pathological_aet Upper threshold for pathological AET (\%).
#'   Default \strong{6}.
#' @param normal_aet Lower threshold for normal AET (\%).  Default \strong{4}.
#' @param mnbi_col Optional: name of the Mean Nocturnal Baseline Impedance
#'   column.  If present, values < 2292 Ω support GORD in borderline cases.
#' @param pspw_col Optional: name of the PSPW (post-reflux swallow-induced
#'   peristaltic wave) index column.  Values < 61\% support GORD.
#' @param total_episodes_col Optional: name of the total reflux episode count
#'   column. > 80 episodes supports GORD as a symptom burden metric.
#'
#' @return \code{df} with columns:
#' \describe{
#'   \item{\code{AcidReflux_Imp}}{Binary: 1 = AET > 6\%, 0 = otherwise.}
#'   \item{\code{AcidReflux_Lyon}}{Factor: \code{"Conclusive GORD"},
#'     \code{"Conclusive GORD (adjunctive)"}, \code{"Inconclusive"},
#'     \code{"GORD excluded"}.}
#' }
#'
#' @references
#' Gyawali CP, Kahrilas PJ, Savarino E, et al. (2021).
#' Modern diagnosis of GERD: the Lyon Consensus.
#' \emph{Gut}, 70(7), 1351–1362. \doi{10.1136/gutjnl-2020-323270}
#'
#' @examples
#' df <- data.frame(
#'   HospNum_Id = c("B001", "B002", "B003", "B004"),
#'   MainAcidExpTotalClearanceChannelPercentTime = c(8.2, 5.1, 2.8, 4.9),
#'   mnbi         = c(1800, 2500, 3100, 2000),
#'   pspw_index   = c(45,   70,   80,   55)
#' )
#' out <- GORD_AcidImp_Lyon(df,
#'                          mnbi_col = "mnbi",
#'                          pspw_col = "pspw_index")
#' out[, c("HospNum_Id", "AcidReflux_Lyon")]
#'
#' @export
GORD_AcidImp_Lyon <- function(df,
                               aet_col            = "MainAcidExpTotalClearanceChannelPercentTime",
                               pathological_aet   = 6,
                               normal_aet         = 4,
                               mnbi_col           = NULL,
                               pspw_col           = NULL,
                               total_episodes_col = NULL) {

  check_cols(df, aet_col)
  aet <- to_numeric(df[[aet_col]])

  lyon <- dplyr::case_when(
    aet > pathological_aet ~ "Conclusive GORD",
    aet < normal_aet       ~ "GORD excluded",
    TRUE                   ~ "Inconclusive"
  )

  # Upgrade borderline cases with adjunctive metrics
  borderline <- lyon == "Inconclusive"
  if (any(borderline, na.rm = TRUE)) {
    adjunctive_positive <- rep(FALSE, nrow(df))

    if (!is.null(mnbi_col) && mnbi_col %in% names(df)) {
      mnbi <- to_numeric(df[[mnbi_col]])
      adjunctive_positive <- adjunctive_positive | (!is.na(mnbi) & mnbi < 2292)
    }
    if (!is.null(pspw_col) && pspw_col %in% names(df)) {
      pspw <- to_numeric(df[[pspw_col]])
      adjunctive_positive <- adjunctive_positive | (!is.na(pspw) & pspw < 61)
    }
    if (!is.null(total_episodes_col) && total_episodes_col %in% names(df)) {
      eps <- to_numeric(df[[total_episodes_col]])
      adjunctive_positive <- adjunctive_positive | (!is.na(eps) & eps > 80)
    }

    lyon[borderline & adjunctive_positive] <- "Conclusive GORD (adjunctive)"
  }

  df$AcidReflux_Lyon <- factor(
    lyon,
    levels = c("GORD excluded", "Inconclusive",
               "Conclusive GORD (adjunctive)", "Conclusive GORD")
  )
  df$AcidReflux_Imp  <- as.integer(
    grepl("Conclusive GORD", lyon)
  )
  df
}


#' Classify pH-impedance studies using legacy threshold (AET > 4.2%)
#'
#' This function applies the older pre-Lyon threshold of AET > 4.2\% as the
#' cut-off for a positive impedance study.  It is provided for backward
#' compatibility and historical comparison.  New analyses should use
#' \code{\link{GORD_AcidImp_Lyon}}.
#'
#' @param df A data frame with an AET column.
#' @param aet_col Name of the AET column (default
#'   \code{"MainAcidExpTotalClearanceChannelPercentTime"}).
#' @param threshold Legacy AET threshold.  Default \strong{4.2\%}.
#'
#' @return \code{df} with column \code{AcidReflux_Imp} (1 = positive).
#'
#' @seealso \code{\link{GORD_AcidImp_Lyon}}
#'
#' @examples
#' df <- data.frame(
#'   MainAcidExpTotalClearanceChannelPercentTime = c(5.0, 3.0, 4.5)
#' )
#' GORD_AcidImp(df)$AcidReflux_Imp
#' # [1] 1 0 1
#'
#' @export
GORD_AcidImp <- function(df,
                          aet_col   = "MainAcidExpTotalClearanceChannelPercentTime",
                          threshold = 4.2) {
  check_cols(df, aet_col)
  aet <- to_numeric(df[[aet_col]])
  df$AcidReflux_Imp <- as.integer(!is.na(aet) & aet > threshold)
  df
}
