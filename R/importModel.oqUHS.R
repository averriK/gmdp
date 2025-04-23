#' @title Import UHS Curves from OpenQuake Outputs
#'
#' @description
#' Reads one or more `uhs-csv.zip` archives in the specified folder, unzips them,
#' and parses any `uhs*.csv` files containing direct Uniform Hazard Spectrum data.
#' This yields columns like:
#' \itemize{
#'   \item \strong{lon, lat, depth} (if depth is in the CSV)
#'   \item \strong{rate} (the numeric value extracted from e.g. "0.095160~...")
#'   \item \strong{Tn} (period in seconds, 0 for PGA)
#'   \item \strong{Sa} (spectral acceleration in g)
#'   \item \strong{p} (quantile, if present in header)
#'   \item \strong{ITo} (investigation_time from header)
#' }
#'
#' @param path Character. Path to the folder containing \code{*uhs-csv.zip} files.
#'
#' @return A \code{data.table} with the columns listed above. If no valid files are found,
#'   returns an empty data.table.
#'
#' @details
#' The code creates a fresh subdirectory \code{.temp_oqUHS_<timestamp>} under \code{path},
#' unzips all matching \code{uhs-csv.zip} there, parses the resulting CSV files, and then
#' deletes the subdirectory.
#'
#' @examples
#' \dontrun{
#' UHS_dt <- importModel.oqUHS("path/to/oq/output")
#' }
#' @import data.table
#' @export
importModel.oqUHS <- function(path) {
  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  # Create a fresh local subdir for unzipping
  tmp_dir <- file.path(path, paste0(".temp_oqUHS_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
  }
  dir.create(tmp_dir, showWarnings = FALSE)

  # Identify all *uhs-csv.zip in `path`
  zip_files <- list.files(path, pattern = "uhs-csv\\.zip$", full.names = TRUE)
  if (length(zip_files) == 0) {
    message("No *uhs-csv.zip files found in: ", path)
    return(data.table())
  }

  # Unzip each file into tmp_dir
  for (zf in zip_files) {
    utils::unzip(zipfile = zf, exdir = tmp_dir, junkpaths = TRUE)
  }

  # Now find the resulting CSVs (uhs.*.csv)
  uhs_files <- list.files(tmp_dir, pattern = "uhs.*\\.csv$", full.names = TRUE)
  if (length(uhs_files) == 0) {
    unlink(tmp_dir, recursive = TRUE, force=TRUE)
    message("No UHS CSV files found after unzipping in: ", path)
    return(data.table())
  }

  result_list <- vector("list", length(uhs_files))

  for (i in seq_along(uhs_files)) {
    file_i <- uhs_files[i]
    header_line <- tryCatch(
      readLines(file_i, n = 1L),
      error = function(e) ""
    )
    p_val <- .extractQuantileFromHeader(header_line)
    ITo   <- .extractInvestigationTime(header_line)

    dt_raw <- data.table::fread(file_i, skip=1, header=TRUE)

    # Identify ID columns (lon, lat, maybe depth)
    id_cols <- c("lon", "lat")
    if ("depth" %in% names(dt_raw)) {
      id_cols <- c(id_cols, "depth")
    }
    measure_cols <- setdiff(names(dt_raw), id_cols)

    # Ensure we have columns with '~' (like "0.095160~PGA")
    if (!any(grepl("~", measure_cols))) {
      warning(sprintf(
        "File '%s' has no columns with '~'. Not recognized as UHS data. Skipping.",
        basename(file_i)
      ))
      next
    }

    # Reshape wide -> long
    dt_long <- melt(
      dt_raw,
      id.vars = id_cols,
      measure.vars = measure_cols,
      variable.name = "exceed_tag",
      value.name    = "Sa"
    )

    # parse "exceed_tag" => (rate, Tn)
    dt_long[, c("rate", "Tn") := .parseUHScolumn(exceed_tag)]
    dt_long[, exceed_tag := NULL]

    dt_long[, p := p_val]
    dt_long[, ITo := ITo]

    result_list[[i]] <- dt_long
  }

  # Combine
  DT <- rbindlist(result_list, use.names=TRUE, fill=TRUE)
  # Clean up
  unlink(tmp_dir, recursive=TRUE, force=TRUE)

  # reorder columns for consistency
  final_cols <- c("lon", "lat", "depth", "rate", "Tn", "Sa", "p", "ITo")
  col_in_both <- intersect(final_cols, names(DT))
  data.table::setcolorder(DT, col_in_both)

  return(DT[])
}

