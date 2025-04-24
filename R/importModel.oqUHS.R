#' @title Import UHS from OpenQuake (Old-Style, Safe Loop)
#'
#' @description
#' Reads \code{uhs*.csv} or \code{uhs-csv.zip} from \code{path}, skipping line #1
#' (metadata => parse \code{p,ITo}). Melts wide->long on `(lon, lat[, depth])`.
#' Then, for each measure column name (like `"0.09516~SA(0.2)"`),
#' we parse prefix => POE, suffix => Tn via a row-by-row safe loop (\code{safeParseUHS}).
#'
#' If \code{POE<1 && ITo>0}, we compute \code{AEP=-log(1-POE)/ITo},
#' \code{TR=-ITo/log(1-POE)}. The final columns are
#' \code{(lon, lat, depth, Tn, p, POE, AEP, TR, ITo, Sa)}. Any row that fails
#' parse or \code{prefix>=1} is dropped. This avoids substring errors in data.table.
#'
#' @param path Character. Directory with `uhs-csv.zip` or `uhs*.csv`.
#'
#' @return A data.table with columns:
#'   \itemize{
#'     \item \strong{lon, lat, depth} if present
#'     \item \strong{Tn} numeric if "SA(0.xx)", 0 if "PGA"
#'     \item \strong{p} from the metadata
#'     \item \strong{POE, AEP, TR, ITo} if \code{ITo>0}
#'     \item \strong{Sa} meltdown cell
#'   }
#' @import data.table
#' @keywords internal
importModel.oqUHS <- function(path) {
  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  # 1) Possibly unzip .zip => temp
  tmp_dir <- file.path(path, paste0(".temp_oqUHS_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
  }
  dir.create(tmp_dir, showWarnings=FALSE)

  zip_files <- list.files(path, pattern="uhs-csv\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zf, exdir=tmp_dir, junkpaths=TRUE)
    }
  }
  search_dir <- if (length(zip_files)) tmp_dir else path

  # 2) find "uhs*.csv"
  uhs_files <- list.files(search_dir, pattern="uhs.*\\.csv$", full.names=TRUE)
  if (!length(uhs_files)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("No UHS CSV files found in: ", path)
    return(data.table())
  }

  out_list <- vector("list", length(uhs_files))
  iCount   <- 0

  for (f_ in uhs_files) {
    # parse line #1 => p, ITo
    header_line <- tryCatch(readLines(f_, n=1L), error=function(e)"")
    p_val <- .extractQuantileFromHeader(header_line)
    ITo   <- .extractInvestigationTime(header_line)

    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    # ID columns
    id_cols <- c("lon","lat")
    if ("depth" %in% col_names) {
      id_cols <- c(id_cols, "depth")
    }
    measure_cols <- setdiff(col_names, id_cols)
    if (!length(measure_cols)) next

    # meltdown
    dt_long <- melt(
      dt_raw,
      id.vars         = id_cols,
      measure.vars    = measure_cols,
      variable.name   = "col_label",
      value.name      = "Sa",
      variable.factor = FALSE
    )
    dt_long[, Sa := as.numeric(Sa)]

    if (!nrow(dt_long)) next

    # 3) parse prefix => POE, suffix => Tn safely row-by-row
    parsed <- safeParseUHS(dt_long$col_label)
    dt_long[, POE := parsed[[1]] ]
    dt_long[, Tn  := parsed[[2]] ]
    # drop rows that failed parse => POE or Tn => NA
    dt_long <- dt_long[!is.na(POE) & !is.na(Tn)]

    # if POE >=1 => skip
    dt_long <- dt_long[POE < 1]

    # compute AEP, TR
    if (!is.na(ITo) && ITo>0) {
      dt_long[, AEP := -log(1 - POE)/ITo]
      dt_long[, TR  := -ITo / log(1 - POE)]
    } else {
      dt_long[, `:=`(AEP=NA_real_, TR=NA_real_)]
    }

    dt_long[, `:=`(p=p_val, ITo=ITo)]
dt_long[,col_label:=NULL]
    out_list[[ iCount <- iCount+1 ]] <- dt_long
  }

  # cleanup
  unlink(tmp_dir, recursive=TRUE, force=TRUE)
  DT <- data.table::rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(DT)) return(DT)

  # reorder columns
  final_cols <- c("lon","lat","depth","Tn","p","POE","AEP","TR","ITo","Sa")
  keepC <- intersect(final_cols, names(DT))
  setcolorder(DT, keepC)

  return(DT[])
}
