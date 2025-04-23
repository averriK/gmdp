#' @title Import AEP (Hazard Curves) from OpenQuake Outputs
#'
#' @description
#' Reads one or more `*.zip` archives in \code{path}, unzips them into a temporary
#' subfolder, looks for files named \code{"hazard_curve*"} or \code{"quantile_curve*"},
#' parses, and returns a data.table of `(lon, lat, depth, Tn, p, Sa, POE, AEP, TR)`.
#'
#' @param path Character. Path to the folder containing the .zip files.
#' @param vref Numeric. Reference Vs30 in m/s (not mandatory for reading, just stored or checked).
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item \strong{lon, lat, depth} (if depth is in the CSV)
#'     \item \strong{Tn} (period in seconds; 0 for PGA, -1 for PGV if present)
#'     \item \strong{p} (quantile or "mean")
#'     \item \strong{Sa} (spectral acceleration level in g, from "poe-...")
#'     \item \strong{POE} (probability of exceedance)
#'     \item \strong{AEP} (annual exceedance probability = -log(1-POE)/ITo)
#'     \item \strong{TR} (return period = -ITo/log(1-POE))
#'     \item \strong{ITo} (investigation time extracted from header)
#'   }
#'
#' @import data.table
#' @export
importModel.oqAEP <- function(path, vref) {

  temp_subdir <- file.path(path, paste0(".temp_oqAEP_", as.integer(Sys.time())))
  if (dir.exists(temp_subdir)) {
    # remove old leftover if it exists
    unlink(temp_subdir, recursive = TRUE, force = TRUE)
  }
  dir.create(temp_subdir, showWarnings = FALSE)

  # 2) Unzip all .zip files in `path` into that temp_subdir
  zip_files <- list.files(path, pattern = "\\.zip$", full.names = TRUE)
  if (length(zip_files) == 0) {
    stop("No .zip files found in '", path, "'.")
  }
  for (zf in zip_files) {
    utils::unzip(zipfile = zf, exdir = temp_subdir, junkpaths = TRUE)
  }

  # 3) Identify hazard/quantile curve CSVs
  hazard_files   <- list.files(temp_subdir, pattern = "hazard_curve.*\\.csv$",   full.names = TRUE)
  quantile_files <- list.files(temp_subdir, pattern = "quantile_curve.*\\.csv$", full.names = TRUE)
  all_files      <- c(hazard_files, quantile_files)

  if (length(all_files) == 0) {
    # Clean up and stop
    unlink(temp_subdir, recursive = TRUE, force = TRUE)
    stop("No hazard_curve/quantile_curve CSV found in: ", path)
  }

  result_list <- vector("list", length(all_files))

  # 4) For each file, parse the header, read data, reshape wide->long
  for (i in seq_along(all_files)) {
    file_i <- all_files[i]
    header_line <- tryCatch(
      readLines(file_i, n = 1L),
      error = function(e) ""
    )
    p_val <- .extractQuantileFromHeader(header_line)
    ITo   <- .extractInvestigationTime(header_line)
    Tn    <- .extractTnFromHeader(header_line)

    dt_raw <- data.table::fread(file = file_i, skip = 1, header = TRUE)
    measure_cols <- grep("^poe-[0-9\\.]+$", names(dt_raw), value = TRUE)

    if (length(measure_cols) == 0) {
      stop("File '", basename(file_i), "' has no poe-* columns. Not a hazard/quantile curve?")
    }

    # Might or might not have 'depth'
    id_cols <- c("lon", "lat")
    if ("depth" %in% names(dt_raw)) id_cols <- c(id_cols, "depth")

    dt_long <- melt(
      dt_raw,
      id.vars = id_cols,
      measure.vars = measure_cols,
      variable.name = "poe_label",
      value.name    = "POE"
    )
    dt_long[, Sa := as.numeric(sub("poe-", "", poe_label))]
    dt_long[, poe_label := NULL]

    # Filter out POE=1 if present
    dt_long <- dt_long[POE < 1]

    # Compute AEP & TR
    if (!is.na(ITo)) {
      dt_long[, AEP := -log(1 - POE)/ITo]
      dt_long[, TR  := -ITo / log(1 - POE)]
    } else {
      dt_long[, `:=`(AEP = NA_real_, TR = NA_real_)]
    }

    dt_long[, `:=`(ITo = ITo, Tn = Tn, p = p_val)]
    result_list[[i]] <- dt_long
  }

  # 5) Combine all into one data.table
  DT <- rbindlist(result_list, use.names = TRUE, fill = TRUE)

  # 6) Clean up
  unlink(temp_subdir, recursive = TRUE, force = TRUE)

  # reorder columns
  col_order <- c("lon", "lat", "depth", "Tn", "p", "Sa", "POE", "AEP", "TR", "ITo")
  col_order <- intersect(col_order, names(DT))
  data.table::setcolorder(DT, col_order)
  return(DT[])
}
