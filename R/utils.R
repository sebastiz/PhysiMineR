#' @keywords internal
#' @importFrom dplyr %>%
#' @importFrom rlang .data
"_PACKAGE"

# ─────────────────────────────────────────────────────────────────────────────
# Internal helper utilities
# ─────────────────────────────────────────────────────────────────────────────

#' Coerce a column to numeric, silently
#'
#' Converts a vector to numeric after stripping leading/trailing whitespace.
#' Any value that cannot be coerced becomes \code{NA} without a warning.
#'
#' @param x A vector to convert.
#' @return A numeric vector of the same length as \code{x}.
#' @keywords internal
to_numeric <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}


#' Coerce a column to Date, silently
#'
#' Tries several common date formats in order. Returns \code{NA} for any
#' value that cannot be parsed.
#'
#' @param x A character or Date vector.
#' @param formats A character vector of \code{\link[base]{strptime}} format
#'   strings to try, in order.
#' @return A \code{Date} vector.
#' @keywords internal
to_date <- function(x,
                    formats = c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y",
                                "%Y/%m/%d", "%d %b %Y")) {
  if (inherits(x, "Date")) return(x)
  out <- as.Date(rep(NA_character_, length(x)))
  for (fmt in formats) {
    missing_idx <- is.na(out)
    if (!any(missing_idx)) break
    parsed <- suppressWarnings(as.Date(as.character(x[missing_idx]), format = fmt))
    out[missing_idx] <- parsed
  }
  out
}


#' Check that required columns exist in a data frame
#'
#' Stops with an informative message if any expected column is absent.
#'
#' @param df A data frame.
#' @param cols Character vector of required column names.
#' @param call_env Used to improve error tracing (usually left as default).
#' @return \code{df} invisibly (called for its side-effect).
#' @keywords internal
check_cols <- function(df, cols, call_env = parent.frame()) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) {
    stop(
      "The following required column(s) are missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(df)
}


#' Replace empty strings and literal "null" with NA
#'
#' @param df A data frame.
#' @return The same data frame with empty strings and "null" replaced by NA.
#' @keywords internal
null_to_na <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      col[col == "" | tolower(col) == "null"] <- NA_character_
    }
    col
  })
  df
}
