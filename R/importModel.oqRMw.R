#' @title Import Disaggregation (R-Mw-p) from OpenQuake
#'
#' @description
#' Reads files named "Mag_Dist" (not "TRT") from the .zip or folder. If a
#' "Mag_Dist-mean" is present, we pick that first. Skips the first line
#' (metadata), treats second line as column headers, renames (mag,dist,poe)->(Mw,R,POE),
#' and (rlz|mean)->p. Then calculates AEP=POE/ITo, TR=1/AEP, Tn from "imt" if present.
#'
#' @param path Character. Folder with `.zip` or direct `Mag_Dist*` CSV
#' @param ITo Numeric. Investigation time
#' @param vref Numeric. Vs30 for reference (not mandatory)
#'
#' @return data.table or NULL if no valid data found. Columns:
#' \itemize{
#'   \item Mw, R, POE, p, Tn, AEP, TR, IT
#' }
#' @import data.table
#' @export
importModel.oqRMw <- function(path, ITo, vref) {
  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  tmp_dir <- file.path(path, paste0(".temp_oqRMw_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
  }
  dir.create(tmp_dir, showWarnings=FALSE)

  zip_files <- list.files(path, pattern="\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zf, exdir=tmp_dir, junkpaths=TRUE)
    }
  }
  search_dir <- if (length(zip_files)) tmp_dir else path

  # find Mag_Dist (not TRT)
  all_files <- list.files(search_dir, pattern="Mag_Dist", full.names=TRUE)
  all_files <- all_files[!grepl("TRT", all_files)]
  if (!length(all_files)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("No 'Mag_Dist' files found in: ", path)
    return(NULL)
  }
  mean_files <- grep("Mag_Dist-mean", all_files, value=TRUE)
  if (length(mean_files)) {
    all_files <- mean_files
  }

  DHT <- data.table()
  for (f_ in all_files) {
    meta_line <- tryCatch(readLines(f_, n=1L), error=function(e) "")
    # if you want lat/lon from meta_line, parse here

    dt_raw <- tryCatch(
      data.table::fread(f_, skip=1, header=TRUE, blank.lines.skip=TRUE),
      error=function(e) NULL
    )
    if (is.null(dt_raw) || !nrow(dt_raw)) {
      next
    }

    required_cols <- c("mag","dist","poe","imt")
    missing_cols  <- setdiff(required_cols, names(dt_raw))
    if (length(missing_cols)) {
      next
    }

    setnames(dt_raw, old=c("mag","dist","poe"), new=c("Mw","R","POE"), skip_absent=TRUE)
    if ("iml" %in% names(dt_raw)) {
      dt_raw[, iml := NULL]
    }

    # rename (rlz|mean)->p
    rlz_col <- grep("rlz|mean", names(dt_raw), value=TRUE)
    if (length(rlz_col)==1) {
      setnames(dt_raw, old=rlz_col, new="p")
    } else {
      # skip file if no single p col
      next
    }

    # convert "imt" => Tn
    dt_raw[imt=="PGA", imt:="Sa(0.0)"]
    dt_raw[, Tn := stringr::str_extract(imt, "(?<=\\()\\d+\\.*\\d*(?=\\))")]
    dt_raw[, Tn := as.numeric(Tn)]
    dt_raw[is.na(Tn), Tn:=0]

    dt_raw[, IT:= ITo]
    dt_raw[, `:=`(AEP=POE/IT, TR=1/(POE/IT))]
    DHT <- rbind(DHT, dt_raw, fill=TRUE)
  }

  unlink(tmp_dir, recursive=TRUE, force=TRUE)
  if (!nrow(DHT)) {
    message("No valid disagg data found.")
    return(NULL)
  }
  # remove duplicates if desired
  DHT <- unique(DHT)
  return(DHT[])
}
