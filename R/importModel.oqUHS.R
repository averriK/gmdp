#' @title Import UHS Curves from OpenQuake Outputs
#'
#' @description
#' Reads "uhs*.csv" or "uhs-csv.zip" from \code{path}, skipping the first line
#' (metadata) and treating the second line as the column header. Then each
#' column name like "0.095160~PGA" is melted, with "exceed_tag" parsed by
#' `.parseUHScolumn()` => `(rate, Tn)`.
#'
#' @param path Character. Directory with UHS csv or zip
#'
#' @return A data.table with columns:
#' \itemize{
#'   \item \strong{lon, lat, depth} (if depth is found)
#'   \item \strong{rate, Tn, Sa, p, ITo}
#' }
#' @import data.table
#' @export
importModel.oqUHS <- function(path) {
  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  tmp_dir <- file.path(path, paste0(".temp_oqUHS_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
  }
  dir.create(tmp_dir, showWarnings=FALSE)

  # unzip if needed
  zip_files <- list.files(path, pattern="uhs-csv\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zf, exdir=tmp_dir, junkpaths=TRUE)
    }
  }
  search_dir <- if (length(zip_files)) tmp_dir else path

  # find uhs.*.csv
  uhs_files <- list.files(search_dir, pattern="uhs.*\\.csv$", full.names=TRUE)
  if (!length(uhs_files)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("No UHS CSV files found in: ", path)
    return(data.table())
  }

  out_list <- vector("list", length(uhs_files))
  iCount   <- 0

  for (f_ in uhs_files) {
    line1 <- tryCatch(readLines(f_, n=1L), error=function(e)"")
    p_val <- .extractQuantileFromHeader(line1)
    ITo   <- .extractInvestigationTime(line1)

    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    id_cols <- c("lon","lat")
    if ("depth" %in% col_names) {
      id_cols <- c(id_cols, "depth")
    }
    measure_cols <- setdiff(col_names, id_cols)
    # ensure we have ~
    if (!any(grepl("~", measure_cols))) {
      next
    }

    dt_long <- melt(
      dt_raw,
      id.vars=id_cols,
      measure.vars=measure_cols,
      variable.name="exceed_tag",
      value.name="Sa"
    )
    dt_long[, exceed_tag := as.character(exceed_tag)]
    # parse "exceed_tag"
    dt_long[, c("rate","Tn") := .parseUHScolumn(exceed_tag)]
    dt_long[, exceed_tag := NULL]

    dt_long[, `:=`(p=p_val, ITo=ITo)]
    dt_long[, Sa := as.numeric(Sa)]
    out_list[[ iCount <- iCount+1 ]] <- dt_long
  }

  unlink(tmp_dir, recursive=TRUE, force=TRUE)
  out <- data.table::rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(out)) return(out)

  # make unique
  out <- unique(out)

  final_cols <- c("lon","lat","depth","rate","Tn","Sa","p","ITo")
  keepC <- intersect(final_cols, names(out))
  data.table::setcolorder(out, keepC)
  return(out[])
}
