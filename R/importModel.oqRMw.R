#' @title Import Disaggregation (R-Mw-p) from OpenQuake
#'
#' @description
#' Reads one or more `.zip` archives in \code{path}, unzips them, and then looks
#' for files named like \code{"Mag_Dist"} (but not \code{"TRT"}). If a "-mean" file exists,
#' that one is used. The data is returned with columns:
#' \itemize{
#'   \item \strong{Mw, R, POE}
#'   \item \strong{AEP, TR}
#'   \item \strong{Tn} (parsed from \code{imt}, e.g. "Sa(0.0)"),
#'   \item \strong{p} (renamed from "mean" or "rlz")
#' }
#'
#' @param path Character. Path to the folder containing .zip with disagg outputs.
#' @param ITo Numeric. Investigation time, e.g. 50.
#' @param vref Numeric. Reference Vs30 in m/s (not required, just for logging).
#'
#' @return \code{data.table} or \code{NULL} if no valid files are found.
#'
#' @import data.table
#' @importFrom stringr str_extract
#' @export
importModel.oqRMw <- function(path, ITo, vref) {
  if (!dir.exists(path)) {
    stop("`path` must be an existing directory.")
  }

  # Create a local temp subdir
  tmp_dir <- file.path(path, paste0(".temp_oqRMw_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
  }
  dir.create(tmp_dir, showWarnings=FALSE)

  # Unzip all .zip in path
  zip_files <- list.files(path, pattern="\\.zip$", full.names=TRUE)
  if (length(zip_files)==0) {
    message("No .zip files found in: ", path)
    return(NULL)
  }
  for (zf in zip_files) {
    utils::unzip(zf, exdir=tmp_dir, junkpaths=TRUE)
  }

  # Now find "Mag_Dist" files, ignoring "TRT"
  all_files <- list.files(tmp_dir, pattern="Mag_Dist", full.names=TRUE)
  all_files <- all_files[!grepl("TRT", all_files)]
  if (length(all_files)==0) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("No 'Mag_Dist' files found in: ", path)
    return(NULL)
  }

  # Possibly pick "-mean" version if present
  mean_files <- grep("Mag_Dist-mean", all_files, value=TRUE)
  if (length(mean_files)>0) {
    all_files <- mean_files
  }

  DHT <- data.table()

  for (file_i in all_files) {
    # read lines, skip 1 row
    dt_raw <- tryCatch(
      data.table::fread(file_i, skip=1, header=TRUE, blank.lines.skip=TRUE),
      error=function(e) NULL
    )
    if (is.null(dt_raw)) {
      warning("Failed reading file: ", basename(file_i), ". Skipping.")
      next
    }

    # check required cols
    required_cols <- c("mag","dist","poe","imt")
    missing_cols  <- setdiff(required_cols, names(dt_raw))
    if (length(missing_cols)>0) {
      warning("File '", basename(file_i), "' missing: ", paste(missing_cols, collapse=", "), ". Skipping.")
      next
    }

    setnames(dt_raw, old=c("mag","dist","poe"), new=c("Mw","R","POE"))
    if ("iml" %in% names(dt_raw)) {
      dt_raw[, iml:=NULL]
    }

    # rename "rlz" or "mean" -> p
    rlz_col <- grep("rlz|mean", names(dt_raw), value=TRUE)
    if (length(rlz_col)==1) {
      setnames(dt_raw, old=rlz_col, new="p")
    } else if (length(rlz_col)>1) {
      warning("Multiple 'rlz|mean' columns in file '", basename(file_i), "'. Skipping.")
      next
    } else {
      warning("No 'rlz' or 'mean' col in file '", basename(file_i), "'. Skipping.")
      next
    }

    # Convert "imt", e.g. "PGA" -> "Sa(0.0)"
    dt_raw[imt=="PGA", imt:="Sa(0.0)"]
    dt_raw[, Tn := 0]
    dt_raw[, Tn := as.numeric(stringr::str_extract(imt, "(?<=\\()\\d+\\.*\\d*(?=\\))"))]
    dt_raw[is.na(Tn), Tn := 0]

    # compute AEP, TR
    dt_raw[, IT  := ITo]
    dt_raw[, AEP := POE / IT]
    dt_raw[, TR  := 1/AEP]

    DHT <- rbind(DHT, dt_raw, fill=TRUE, use.names=TRUE)
  }

  # clean up
  unlink(tmp_dir, recursive=TRUE, force=TRUE)

  if (nrow(DHT)==0) {
    message("No valid disagg data found after unzipping in: ", path)
    return(NULL)
  }
  return(DHT[])
}
