#' @title Import AEP (Hazard Curves) from OpenQuake
#'
#' @description
#' Reads hazard-curve or quantile-curve CSV files (or .zip) from \code{path},
#' skipping the first line (metadata) and treating the second line as column
#' headers (to match old code). Melts columns like "poe-0.001" => (Sa=0.001, POE).
#' Then computes AEP=-log(1-POE)/ITo and TR=-ITo/log(1-POE).
#'
#' It returns a table with potential duplicates if the same site is repeated
#' multiple times. We also do `unique()` so each row is unique.
#'
#' @param path Character. Directory containing hazard_curve*.csv or .zip
#' @param vref Numeric. Reference Vs30 in m/s (not strictly required, stored or logged)
#'
#' @return data.table with columns:
#' \itemize{
#'   \item \strong{lon, lat, depth} if present
#'   \item \strong{Sa, POE, AEP, TR, Tn, p, ITo}
#' }
#' @export
importModel.oqAEP <- function(path, vref) {
  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  # 1) Possibly unzip .zip => temp folder
  temp_subdir <- file.path(path, paste0(".temp_oqAEP_", as.integer(Sys.time())))
  if (dir.exists(temp_subdir)) {
    unlink(temp_subdir, recursive=TRUE, force=TRUE)
  }
  dir.create(temp_subdir, showWarnings=FALSE)
  zip_files <- list.files(path, pattern="\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zf, exdir=temp_subdir, junkpaths=TRUE)
    }
  }
  search_dir <- if (length(zip_files)) temp_subdir else path

  # 2) Find hazard or quantile CSV
  files_curves <- list.files(search_dir, pattern="(hazard_curve|quantile_curve).*\\.csv$", full.names=TRUE)
  if (!length(files_curves)) {
    unlink(temp_subdir, recursive=TRUE, force=TRUE)
    stop("No hazard_curve/quantile_curve CSV found in: ", path)
  }

  out_list <- vector("list", length(files_curves))
  iCount <- 0

  for (f_ in files_curves) {
    # read first line => parse p, ITo, Tn
    line1 <- tryCatch(readLines(f_, n=1L), error=function(e)"")
    p_val <- .extractQuantileFromHeader(line1)
    ITo   <- .extractInvestigationTime(line1)
    Tn    <- .extractTnFromHeader(line1)

    # read the data skipping line1, no header
    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    # 1st row => column names
    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]  # remove that row

    # identify id cols
    id_cols <- c("lon","lat")
    if ("depth" %in% col_names) id_cols <- c(id_cols, "depth")
    # measure => "poe-xxx"
    measure_cols <- grep("^poe-[0-9\\.]+$", col_names, value=TRUE)
    if (!length(measure_cols)) next

    dt_long <- melt(
      dt_raw,
      id.vars=id_cols,
      measure.vars=measure_cols,
      variable.name="poe_label",
      value.name="POE"
    )
    dt_long[, POE := as.numeric(POE)]
    dt_long[, Sa  := as.numeric(sub("poe-","",poe_label))]
    dt_long[, poe_label := NULL]

    # filter out POE>=1
    dt_long <- dt_long[POE < 1]

    if (!is.na(ITo) && ITo>0) {
      dt_long[, `:=`(
        AEP = -log(1-POE)/ITo,
        TR  = -ITo/log(1-POE)
      )]
    } else {
      dt_long[, `:=`(AEP=NA_real_, TR=NA_real_)]
    }

    # store Tn, p
    if (!is.na(Tn)) {
      dt_long[, Tn := Tn]
    } else {
      # might be multiple Tn in one file
      # we keep as NA unless you want advanced logic
      dt_long[, Tn := NA_real_]
    }
    dt_long[, `:=`(p=p_val, ITo=ITo)]
    out_list[[ iCount <- iCount+1 ]] <- dt_long
  }

  unlink(temp_subdir, recursive=TRUE, force=TRUE)
  out <- data.table::rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(out)) {
    stop("No hazard data found after reading files in: ", path)
  }
  # remove duplicates
  out <- unique(out)

  # reorder
  final_cols <- c("lon","lat","depth","Tn","p","Sa","POE","AEP","TR","ITo")
  keepC <- intersect(final_cols, names(out))
  data.table::setcolorder(out, keepC)
  return(out[])
}
