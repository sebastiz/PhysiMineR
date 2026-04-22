# ─────────────────────────────────────────────────────────────────────────────
# Cross-test merging utilities
# ─────────────────────────────────────────────────────────────────────────────

#' Merge two physiology datasets selecting the closest-in-time test per patient
#'
#' A common challenge in oesophageal physiology research is that patients may
#' have undergone multiple tests of the same type.  This function joins two
#' data frames by patient identifier, computes the absolute date difference
#' between the two visit dates, and retains only the pair with the smallest
#' difference per patient.
#'
#' @param df_left  Left-side data frame (e.g. HRM).  Must contain \code{id_col}
#'   and \code{date_col_left}.
#' @param df_right Right-side data frame (e.g. BRAVO).  Must contain
#'   \code{id_col} and \code{date_col_right}.
#' @param id_col         Name of the shared patient identifier (default
#'   \code{"HospNum_Id"}).
#' @param date_col_left  Name of the visit-date column in \code{df_left}
#'   (default \code{"VisitDate.x"}).
#' @param date_col_right Name of the visit-date column in \code{df_right}
#'   (default \code{"VisitDate.y"}).
#' @param join_type Type of join: \code{"inner"} (default), \code{"left"},
#'   \code{"right"}, or \code{"full"}.
#' @param max_days_apart Optional integer.  If supplied, pairs whose test dates
#'   are more than this many days apart are dropped.  Useful for enforcing a
#'   maximum window between tests (e.g. 365 days).
#'
#' @return A data frame with one row per patient (the test pair closest in
#'   time), plus a column \code{Date_ABS_Diff} (absolute difference in days).
#'
#' @examples
#' hrm   <- data.frame(HospNum_Id = c("A", "A", "B"),
#'                     VisitDate.x = as.Date(c("2020-01-01","2021-06-01","2020-03-01")),
#'                     IRP = c(8, 9, 12))
#' bravo <- data.frame(HospNum_Id = c("A", "B"),
#'                     VisitDate.y = as.Date(c("2020-01-15","2020-03-10")),
#'                     AET_day1 = c(7.2, 3.1))
#' testMerge(hrm, bravo)
#'
#' @export
testMerge <- function(df_left,
                       df_right,
                       id_col         = "HospNum_Id",
                       date_col_left  = "VisitDate.x",
                       date_col_right = "VisitDate.y",
                       join_type      = "inner",
                       max_days_apart = NULL) {

  check_cols(df_left,  c(id_col, date_col_left))
  check_cols(df_right, c(id_col, date_col_right))

  df_left[[date_col_left]]   <- to_date(df_left[[date_col_left]])
  df_right[[date_col_right]] <- to_date(df_right[[date_col_right]])

  merged <- switch(
    join_type,
    inner = dplyr::inner_join(df_left, df_right, by = id_col),
    left  = dplyr::left_join(df_left,  df_right, by = id_col),
    right = dplyr::right_join(df_left, df_right, by = id_col),
    full  = dplyr::full_join(df_left,  df_right, by = id_col),
    stop("join_type must be one of: 'inner', 'left', 'right', 'full'")
  )

  merged$Date_ABS_Diff <- as.numeric(
    abs(merged[[date_col_left]] - merged[[date_col_right]])
  )

  # Keep only the closest pair per patient
  merged <- merged %>%    dplyr::arrange(.data[[id_col]], .data$Date_ABS_Diff) %>%    dplyr::group_by(.data[[id_col]]) %>%    dplyr::slice(1L) %>%    dplyr::ungroup()

  if (!is.null(max_days_apart)) {
    n_before <- nrow(merged)
    merged   <- merged[!is.na(merged$Date_ABS_Diff) &
                         merged$Date_ABS_Diff <= max_days_apart, ]
    n_dropped <- n_before - nrow(merged)
    if (n_dropped > 0L) {
      message(n_dropped, " patient(s) dropped: tests more than ",
              max_days_apart, " days apart.")
    }
  }

  merged
}


#' Merge three physiological datasets (triple-test merge)
#'
#' A convenience wrapper that applies \code{\link{testMerge}} sequentially to
#' combine three data frames (e.g. HRM, impedance, and BRAVO).
#'
#' The function first merges \code{df_a} and \code{df_b} using the closest-
#' visit-date strategy, then merges the result with \code{df_c}.
#'
#' @param df_a First data frame (e.g. HRM).
#' @param df_b Second data frame (e.g. impedance).
#' @param df_c Third data frame (e.g. BRAVO).
#' @param id_col Shared patient identifier column name.
#' @param max_days_apart Optional maximum days between any two tests.
#' @param join_type Join type passed to each \code{\link{testMerge}} call.
#'
#' @return A data frame with one row per patient, the closest-pair from each
#'   merge step selected.
#'
#' @examples
#' \dontrun{
#' combined <- tripleTestMerge(AllHRM, ImpAll, AllBravo,
#'                             max_days_apart = 365)
#' nrow(combined)
#' }
#'
#' @export
tripleTestMerge <- function(df_a,
                             df_b,
                             df_c,
                             id_col         = "HospNum_Id",
                             max_days_apart = NULL,
                             join_type      = "inner") {
  ab <- testMerge(df_a, df_b,
                  id_col         = id_col,
                  join_type      = join_type,
                  max_days_apart = max_days_apart)

  # After the first merge dplyr appends .x / .y suffixes to date columns.
  # Detect the left-hand date column from the merged result rather than
  # hard-coding "VisitDate.x".
  left_date <- grep("^VisitDate", names(ab), value = TRUE)[1]
  if (is.na(left_date)) left_date <- "VisitDate.x"

  testMerge(ab, df_c,
            id_col         = id_col,
            date_col_left  = left_date,
            date_col_right = "VisitDate.y",
            join_type      = join_type,
            max_days_apart = max_days_apart)
}


#' Filter rows based on data completeness thresholds
#'
#' Removes columns where the proportion of non-missing values falls below
#' a specified threshold, then optionally removes rows with too many missing
#' values.  Useful as a final quality-control step before modelling.
#'
#' @param df A data frame.
#' @param col_complete_threshold Minimum proportion of non-missing values
#'   required to \emph{retain} a column.  Default \strong{0.9} (90\% complete).
#' @param row_complete_threshold Minimum proportion of non-missing values
#'   required to \emph{retain} a row.  Default \strong{0} (keep all rows).
#' @param verbose If \code{TRUE}, prints a message listing dropped columns.
#'
#' @return A data frame with low-completeness columns (and optionally rows)
#'   removed.
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, NA, 4),
#'   b = c(NA, NA, NA, 4),  # only 25% complete
#'   c = c(1, 2, 3, 4)
#' )
#' filterByCompleteness(df, col_complete_threshold = 0.5)
#' #   a c
#' # 1 1 1
#' # ...
#'
#' @export
filterByCompleteness <- function(df,
                                  col_complete_threshold = 0.9,
                                  row_complete_threshold = 0,
                                  verbose = TRUE) {
  col_completeness <- colMeans(!is.na(df))
  keep_cols        <- col_completeness >= col_complete_threshold
  dropped          <- names(df)[!keep_cols]

  if (verbose && length(dropped) > 0L) {
    message("Dropping ", length(dropped), " column(s) with < ",
            col_complete_threshold * 100, "% complete data:\n  ",
            paste(dropped, collapse = "\n  "))
  }

  df <- df[, keep_cols, drop = FALSE]

  if (row_complete_threshold > 0) {
    row_completeness <- rowMeans(!is.na(df))
    df <- df[row_completeness >= row_complete_threshold, ]
  }

  df
}
