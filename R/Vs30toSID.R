#' @title Convert Vs30 (m/s) to Site Class Identifier
#'
#' @description
#' Given one or more Vs30 values (in m/s), returns a character vector of site
#' class designations (\code{"A", "B", "BC", "C", "CD", "D", "DE", "E"}). The
#' classification thresholds are:
#' \itemize{
#'   \item A: Vs30 >= 1500
#'   \item B: 900 <= Vs30 < 1500
#'   \item BC: 640 <= Vs30 < 900
#'   \item C: 440 <= Vs30 < 640
#'   \item CD: 300 <= Vs30 < 440
#'   \item D: 210 <= Vs30 < 300
#'   \item DE: 150 <= Vs30 < 210
#'   \item E: 0 <= Vs30 < 150
#' }
#'
#' @param Vs30 Numeric vector. One or more Vs30 values in m/s.
#'
#' @return A character vector of the same length as \code{Vs30},
#'         with site class designations.
#' @export
#'
#' @examples
#' Vs30toSID(1500)  # returns "A"
#' Vs30toSID(c(120, 790, 3000, 455))
Vs30toSID <- function(Vs30) {
  if (!is.numeric(Vs30)) {
    stop("`Vs30` must be numeric.")
  }
  sapply(Vs30, .Vs30toSID_char)
}

# Internal helper that does the classification for a single numeric Vs30
.Vs30toSID_char <- function(v) {
  if (v >= 1500) {
    return("A")
  } else if (v >= 900) {
    return("B")
  } else if (v >= 640) {
    return("BC")
  } else if (v >= 440) {
    return("C")
  } else if (v >= 300) {
    return("CD")
  } else if (v >= 210) {
    return("D")
  } else if (v >= 150) {
    return("DE")
  } else if (v >= 0) {
    return("E")
  } else {
    stop("Vs30 < 0 is invalid. Value = ", v)
  }
}
