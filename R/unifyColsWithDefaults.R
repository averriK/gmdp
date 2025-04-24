#' @title Force a data.table to have a fixed set of columns, filling defaults
#'
#' @description
#' For each desired column in `colNames`, if it's missing in `dt`, creates it
#' with a default value if specified in `defaults`, or NA otherwise. Also drops
#' any extra columns not in `colNames`. Reorders columns to match `colNames`.
#'
#' @param dt A data.table to unify
#' @param colNames Character vector of the final columns you want
#' @param defaults A named list of default values for specific columns, e.g.
#'   `list(AF=1, sdLnAF=NA_real_)`. If a column in `colNames` is missing, we
#'   fill it with `defaults[[col]]` if present, or NA if not.
#'
#' @return The same dt, now guaranteed to have columns in `colNames` with
#'   defaults as specified.
#' @export
unifyColsWithDefaults <- function(dt, colNames, defaults=list()) {
  # 1) For each col in colNames:
  for (cN in colNames) {
    if (!cN %in% names(dt)) {
      # if cN in defaults, use that value; else NA
      val <- if (cN %in% names(defaults)) defaults[[cN]] else NA
      dt[, (cN) := val]
    }
  }
  # 2) Drop extra columns not in colNames
  extraCols <- setdiff(names(dt), colNames)
  if (length(extraCols)) {
    dt[, (extraCols) := NULL]
  }
  # 3) Reorder
  data.table::setcolorder(dt, colNames)
  return(dt)
}
