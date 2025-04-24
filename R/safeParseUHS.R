#' @title Safely parse UHS column names for (POE, Tn)
#'
#' @description
#' Given a character vector of column names (e.g. \code{"0.09516~PGA"}),
#' returns two numeric vectors \code{(POE, Tn)}. If something doesn't parse,
#' those entries are \code{NA}.
#'
#' @param col_labels Character vector of measure column names from meltdown
#'
#' @return A list of two numeric vectors: \code{(POE, Tn)}
#'
#' @keywords internal
safeParseUHS <- function(col_labels) {
  n <- length(col_labels)
  POE  <- numeric(n)
  Tn   <- numeric(n)

  # Initialize everything to NA
  POE[] <- NA_real_
  Tn[]  <- NA_real_

  for (i in seq_len(n)) {
    clb <- col_labels[i]
    # split on '~'
    parts <- strsplit(clb, "~", fixed=TRUE)[[1]]
    if (length(parts) < 2) {
      # no parse => skip
      next
    }
    # prefix => numeric POE
    prefix <- suppressWarnings(as.numeric(parts[1]))
    if (is.na(prefix) || prefix >= 1) {
      # invalid or POE>=1 => skip
      next
    }
    # suffix => e.g. "PGA" or "SA(0.2)"
    suffix <- parts[2]
    # parse Tn
    tn_val <- NA_real_
    if (grepl("PGA", suffix, ignore.case=TRUE)) {
      tn_val <- 0
    } else {
      # look for SA(...)
      mt <- regexpr("SA\\(([0-9\\.]+)\\)", suffix)
      if (mt > 0) {
        val2 <- regmatches(suffix, mt)
        # e.g. "SA(0.2)"
        val2 <- sub("^SA\\(", "", val2)
        val2 <- sub("\\)$", "", val2)
        tn_val <- suppressWarnings(as.numeric(val2))
      }
    }
    # If parse succeeded => store
    if (!is.na(tn_val)) {
      POE[i] <- prefix
      Tn[i]  <- tn_val
    }
  }
  list(POE, Tn)
}
