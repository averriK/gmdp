# helpers.R
# Minimal set of internal helper functions for reading OpenQuake headers
# and parsing Tn from hazard or UHS columns.

#' @title Extract Quantile or Mean from OpenQuake Header
#'
#' @description
#' Parses a header line (e.g. from an OpenQuake hazard/quantile CSV) to detect
#' \code{kind='quantile-0.xxx'} or \code{kind='mean'}.
#'
#' @param line Character. The first line from a CSV, e.g.
#'   \code{"..., kind='quantile-0.16', ..."} or \code{"..., kind='mean', ..."}.
#'
#' @return Numeric quantile (e.g. 0.16) or the character "mean". \code{NA}
#'   if not found.
#'
#' @keywords internal
.extractQuantileFromHeader <- function(line) {
  if (grepl("kind='mean'", line)) {
    return("mean")
  }
  mt <- regexpr("kind='quantile-([0-9\\.]+)'", line)
  if (mt > 0) {
    val  <- regmatches(line, mt)
    qval <- sub("kind='quantile-", "", val)
    qval <- sub("'", "", qval)
    return(suppressWarnings(as.numeric(qval)))
  }
  return(NA)
}

#' @title Extract Investigation Time from OpenQuake Header
#'
#' @description
#' Finds \code{investigation_time=XX.xx} in a header line, returns
#' it as numeric.
#'
#' @param line Character. E.g. \code{"..., investigation_time=50.0, ..."}.
#'
#' @return Numeric \code{ITo} if found, else \code{NA_real_}.
#'
#' @keywords internal
.extractInvestigationTime <- function(line) {
  mt <- regexpr("investigation_time=([0-9\\.]+)", line)
  if (mt > 0) {
    val <- regmatches(line, mt)
    num <- sub("investigation_time=", "", val)
    return(as.numeric(num))
  }
  return(NA_real_)
}

#' @title Extract Tn from OpenQuake Header
#'
#' @description
#' Detects \code{imt='PGA'}, \code{imt='PGV'}, or \code{imt='SA(...)'} in a
#' header line. Sets Tn=0 for PGA, Tn=-1 for PGV, numeric if SA(...).
#'
#' @param line Character. E.g.
#'   \code{"..., imt='SA(0.2)', ..."} or \code{"..., imt='PGA', ..."}.
#'
#' @return Numeric Tn if found, 0 for PGA, -1 for PGV, else \code{NA_real_}.
#'
#' @keywords internal
.extractTnFromHeader <- function(line) {
  if (grepl("imt='PGA'", line)) {
    return(0)
  }
  if (grepl("imt='PGV'", line)) {
    return(-1)
  }
  mt <- regexpr("imt='SA\\(([0-9\\.]+)\\)'", line)
  if (mt > 0) {
    val <- regmatches(line, mt)
    # e.g. "imt='SA(0.12)'"
    num <- sub("imt='SA\\(", "", val)
    num <- sub("\\)'", "", num)
    return(as.numeric(num))
  }
  return(NA_real_)
}

#' @title Parse UHS Column Name for Tn (Minimal)
#'
#' @description
#' Takes a column name like \code{"0.09516~SA(0.2)"} or \code{"0.00995~PGA"}
#' and returns the numeric Tn. Tn=0 if \code{PGA}, or numeric if \code{SA(...)}.
#' Ignores the numeric prefix before the tilde.
#'
#' @param col_label Character. E.g. \code{"0.09516~SA(0.2)"}.
#'
#' @return Numeric Tn, 0 if PGA, \code{NA_real_} if not recognized.
#'
#' @keywords internal
.parseUHS_TnOnly <- function(col_label) {
  parts <- strsplit(col_label, "~")[[1]]
  if (length(parts) < 2) {
    return(NA_real_)
  }
  spec_str <- parts[2] # e.g. "PGA" or "SA(0.2)"

  # if "PGA" => Tn=0
  if (grepl("PGA", spec_str, ignore.case=TRUE)) {
    return(0)
  }
  # if "SA(0.xx)" => parse numeric
  mt <- regexpr("SA\\(([0-9\\.]+)\\)", spec_str)
  if (mt > 0) {
    val <- regmatches(spec_str, mt)
    tn_str <- sub("SA\\(", "", val)
    tn_str <- sub("\\)", "", tn_str)
    return(as.numeric(tn_str))
  }
  return(NA_real_)
}
