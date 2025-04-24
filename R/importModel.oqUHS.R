#' @title Import UHS (Minimal) from OpenQuake Outputs (Fixed)
#'
#' @description
#' Reads one or more `uhs.*.csv` (or `uhs-csv.zip`) in `path`, skipping line #1
#' (metadata => parse \code{p,ITo}), meltdown columns except `(lon, lat[, depth])`,
#' parse Tn from something like \code{"0.095160~SA(0.2)"} (ignoring the numeric prefix),
#' and returns columns `(lon, lat, depth, Tn, Sa, p, ITo)`.
#'
#' @param path Character. Folder with `uhs*.csv` or `uhs-csv.zip`.
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item \strong{lon, lat, depth} (if depth is found)
#'     \item \strong{Tn} (0 if "PGA", numeric if "SA(...)" parsed)
#'     \item \strong{Sa} (spectral acceleration from meltdown cell)
#'     \item \strong{p} (quantile or mean from the first-line metadata)
#'     \item \strong{ITo} (investigation_time from metadata)
#'   }
#'
#' @import data.table
#' @keywords internal
importModel.oqUHS <- function(path) {
  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  # Create a fresh temp folder if .zip is found
  tmp_dir <- file.path(path, paste0(".temp_oqUHS_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
  }
  dir.create(tmp_dir, showWarnings=FALSE)

  # Unzip any `uhs-csv.zip`
  zip_files <- list.files(path, pattern="uhs-csv\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zipfile=zf, exdir=tmp_dir, junkpaths=TRUE)
    }
  }
  # If no zips, read directly from `path`
  search_dir <- if (length(zip_files)) tmp_dir else path

  # Find `uhs.*.csv`
  uhs_files <- list.files(search_dir, pattern="uhs.*\\.csv$", full.names=TRUE)
  if (!length(uhs_files)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("No UHS CSV files found in: ", path)
    return(data.table())
  }

  out_list <- vector("list", length(uhs_files))
  iCount   <- 0

  for (f_ in uhs_files) {
    # 1) read the first line => parse p, ITo from metadata
    header_line <- tryCatch(readLines(f_, n=1L), error=function(e) "")
    p_val <- .extractQuantileFromHeader(header_line)
    ITo   <- .extractInvestigationTime(header_line)

    # 2) read data skipping line #1 => second line is col names
    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    # Possibly we have depth
    id_cols <- c("lon","lat")
    if ("depth" %in% col_names) {
      id_cols <- c(id_cols, "depth")
    }
    measure_cols <- setdiff(col_names, id_cols)
    if (!length(measure_cols)) next

    # 3) Melt wide->long, forcing the 'variable' to be character
    dt_long <- data.table::melt(
      dt_raw,
      id.vars         = id_cols,
      measure.vars    = measure_cols,
      variable.name   = "uhs_label",
      value.name      = "Sa",
      variable.factor = FALSE  # ensures 'uhs_label' is character, not factor
    )

    # Convert 'Sa' to numeric in case it's read as char
    dt_long[, Sa := as.numeric(Sa)]

    # 4) parse Tn from "uhs_label", ensuring it's character
    # (variable.factor=FALSE typically keeps it character, but we double-check)
    dt_long[, uhs_label := as.character(uhs_label)]
    dt_long[, Tn := .parseUHS_TnOnly(uhs_label)]
    dt_long[, uhs_label := NULL]

    # 5) attach p, ITo
    dt_long[, `:=`(p=p_val, ITo=ITo)]

    out_list[[ iCount <- iCount+1 ]] <- dt_long
  }

  # cleanup
  unlink(tmp_dir, recursive=TRUE, force=TRUE)
  DT <- data.table::rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(DT)) return(DT)

  # reorder columns
  final_cols <- c("lon","lat","depth","Tn","Sa","p","ITo")
  keepC <- intersect(final_cols, names(DT))
  data.table::setcolorder(DT, keepC)

  return(DT[])
}
