#' @title Import AEP (Hazard Curves) from OpenQuake (Fixed)
#'
#' @description
#' Reads hazard-curve or quantile-curve CSVs from \code{path} (or .zip) using a
#' minimal meltdown approach. For each file:
#' \enumerate{
#'   \item Parses the first line for \code{p} (quantile or mean) and
#'         \code{ITo} (investigation_time). Possibly \code{Tn} from
#'         \code{imt='...'}. If multiple Tn, each file has its own Tn.
#'   \item Skips that line, reads data with \code{header=FALSE} so the second
#'         line is columns.
#'   \item Identifies columns matching \code{poe-xxx}, melts them to long
#'         format => \code{POE} (must be converted to numeric).
#'   \item Filters out \code{POE>=1}, if any.
#'   \item If \code{ITo>0}, computes \code{AEP=-log(1-POE)/ITo} and
#'         \code{TR=-ITo/log(1-POE)}.
#'   \item Stores \code{(lon, lat, depth, Tn, p, Sa, POE, AEP, TR, ITo)}.
#' }
#'
#' @param path Character. Folder with hazard_curve*, quantile_curve* CSV/zip.
#' @param vref Numeric. Vs30 reference if needed (unused in meltdown).
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item \strong{lon, lat, depth} (if depth col is in CSV)
#'     \item \strong{Tn} (0 for PGA, -1 for PGV, numeric if SA(...))
#'     \item \strong{p} (quantile or mean from the header)
#'     \item \strong{Sa} (spectral acceleration from \code{poe-xxx})
#'     \item \strong{POE} (probability of exceedance, numeric)
#'     \item \strong{AEP} (=-log(1-POE)/ITo), \strong{TR} (=-ITo/log(1-POE))
#'     \item \strong{ITo} (investigation_time)
#'   }
#' @import data.table
#' @export
importModel.oqAEP <- function(path, vref) {
  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  temp_subdir <- file.path(path, paste0(".temp_oqAEP_", as.integer(Sys.time())))
  if (dir.exists(temp_subdir)) {
    unlink(temp_subdir, recursive=TRUE, force=TRUE)
  }
  dir.create(temp_subdir, showWarnings=FALSE)

  zip_files <- list.files(path, pattern="\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zipfile=zf, exdir=temp_subdir, junkpaths=TRUE)
    }
  }
  search_dir <- if (length(zip_files)) temp_subdir else path

  files_curves <- list.files(
    search_dir,
    pattern="(hazard_curve|quantile_curve).*\\.csv$",
    full.names=TRUE
  )
  if (!length(files_curves)) {
    unlink(temp_subdir, recursive=TRUE, force=TRUE)
    stop("No hazard_curve/quantile_curve CSV found in: ", path)
  }

  out_list <- list()
  iCount   <- 0

  for (f_ in files_curves) {
    # 1) Read first line => parse p, ITo, Tn
    line1 <- tryCatch(readLines(f_, n=1L), error=function(e)"")
    p_val <- .extractQuantileFromHeader(line1)
    ITo   <- .extractInvestigationTime(line1)
    TnVal <- .extractTnFromHeader(line1)

    # 2) Read data skipping line #1 => second line is col names
    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    # Possibly we have "depth"
    id_cols <- c("lon","lat")
    if ("depth" %in% col_names) {
      id_cols <- c(id_cols, "depth")
    }

    # 3) measure columns => poe-xxx
    measure_cols <- grep("^poe-[0-9\\.]+$", col_names, value=TRUE)
    if (!length(measure_cols)) {
      # skip file if no poe-* columns
      next
    }

    # 4) Melt wide->long
    dt_long <- melt(
      dt_raw,
      id.vars       = id_cols,
      measure.vars  = measure_cols,
      variable.name = "poe_label",
      value.name    = "POE"
    )

    # Convert POE to numeric
    dt_long[, POE := as.numeric(POE)]

    # parse Sa from the "poe-xxx" part
    dt_long[, Sa := as.numeric(sub("poe-", "", poe_label))]
    dt_long[, poe_label := NULL]

    # filter out POE>=1
    dt_long <- dt_long[!is.na(POE) & POE < 1]

    # 5) If ITo>0 => compute AEP,TR
    if (!is.na(ITo) && ITo > 0) {
      dt_long[, AEP := -log(1 - POE) / ITo]
      dt_long[, TR  := -ITo / log(1 - POE)]
    } else {
      dt_long[, `:=`(AEP=NA_real_, TR=NA_real_)]
    }

    # store Tn, p, ITo
    dt_long[, `:=`(Tn=TnVal, p=p_val, ITo=ITo)]

    out_list[[ iCount <- iCount + 1 ]] <- dt_long
  }

  unlink(temp_subdir, recursive=TRUE, force=TRUE)
  DT <- data.table::rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(DT)) return(DT)

  # reorder columns
  final_cols <- c("lon","lat","depth","Tn","p","Sa","POE","AEP","TR","ITo")
  keepC <- intersect(final_cols, names(DT))
  setcolorder(DT, keepC)

  return(DT[])
}
