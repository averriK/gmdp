#' @title Import UHS from OpenQuake (Prefix=POE, Suffix=Tn) - Vectorized
#'
#' @description
#' Reads one or more `uhs*.csv` or `uhs-csv.zip` from `path`, skipping line #1
#' (metadata => parse \code{p,ITo}). Melts wide->long on `(lon, lat[, depth])`.
#'
#' **Interprets** the **numeric prefix** before `"~"` in each column name as
#' \code{POE} (Probability of Exceedance), and the **suffix** as either
#' \code{"PGA"} => Tn=0 or \code{"SA(0.2)"} => Tn=0.2, etc. Then the meltdown cell
#' is \code{Sa}. If \code{POE<1 && ITo>0}, we compute \code{AEP=-log(1-POE)/ITo}
#' and \code{TR=-ITo/log(1-POE)}. This matches your old code's logic that stores
#' `(TR, AEP, POE, Tn, Sa)`.
#'
#' @param path Character. Folder with `uhs*.csv` or `uhs-csv.zip`.
#'
#' @return A `data.table` with columns:
#'   \itemize{
#'     \item \strong{lon, lat, depth} if depth was in the CSV
#'     \item \strong{Tn} numeric if "SA(0.2)", 0 if "PGA"
#'     \item \strong{p} quantile or "mean" from the first-line metadata
#'     \item \strong{POE, AEP, TR, ITo} (as computed if \code{POE<1 && ITo>0})
#'     \item \strong{Sa} spectral acceleration from meltdown
#'   }
#'
#' @import data.table
#' @keywords internal
importModel.oqUHS <- function(path) {
  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  # 1) Possibly unzip `uhs-csv.zip` -> temp folder
  tmp_dir <- file.path(path, paste0(".temp_oqUHS_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
  }
  dir.create(tmp_dir, showWarnings=FALSE)

  zip_files <- list.files(path, pattern="uhs-csv\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zipfile=zf, exdir=tmp_dir, junkpaths=TRUE)
    }
  }
  search_dir <- if (length(zip_files)) tmp_dir else path

  # 2) find `uhs*.csv`
  uhs_files <- list.files(search_dir, pattern="uhs.*\\.csv$", full.names=TRUE)
  if (!length(uhs_files)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("No UHS CSV files found in: ", path)
    return(data.table())
  }

  out_list <- vector("list", length(uhs_files))
  iCount   <- 0

  for (f_ in uhs_files) {
    # 3) read the first line => parse p, ITo
    header_line <- tryCatch(readLines(f_, n=1L), error=function(e)"")
    p_val <- .extractQuantileFromHeader(header_line)
    ITo   <- .extractInvestigationTime(header_line)

    # 4) read data skipping line #1 => line #2 is col names
    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    # identify ID columns (lon, lat[, depth])
    id_cols <- c("lon","lat")
    if ("depth" %in% col_names) {
      id_cols <- c(id_cols, "depth")
    }
    measure_cols <- setdiff(col_names, id_cols)
    if (!length(measure_cols)) next

    # meltdown wide->long
    dt_long <- melt(
      dt_raw,
      id.vars         = id_cols,
      measure.vars    = measure_cols,
      variable.name   = "uhs_col",
      value.name      = "Sa",
      variable.factor = FALSE
    )
    dt_long[, Sa := as.numeric(Sa)]

    # 5) parse prefix => POE, suffix => Tn
    # e.g. "0.048770~SA(0.2)" => prefix=0.048770 => POE=0.04877, suffix=SA(0.2)
    # no "by=..." => we do a vector approach w tstrsplit:
    dt_long[, c("prefix","suffix") := tstrsplit(uhs_col, "~", fixed=TRUE)]
    dt_long[, prefix := as.numeric(prefix)]  # POE
    # parse Tn
    dt_long[, Tn := {
      out_tn <- rep(NA_real_, .N)
      is_pga <- grepl("PGA", suffix, ignore.case=TRUE)
      out_tn[is_pga] <- 0
      # for SA(...) => parse numeric
      idx_sa <- which(!is_pga)
      if (length(idx_sa)) {
        val_sa <- regexpr("SA\\(([0-9\\.]+)\\)", suffix[idx_sa])
        # those w no match => remain NA
        has_sa <- which(val_sa > 0)
        if (length(has_sa)) {
          matched_idx <- idx_sa[has_sa]
          val2 <- regmatches(suffix[matched_idx], val_sa[has_sa])
          # e.g. "SA(0.2)"
          val2 <- sub("SA\\(", "", val2)
          val2 <- sub("\\)", "", val2)
          out_tn[matched_idx] <- as.numeric(val2)
        }
      }
      out_tn
    }]

    dt_long[, c("uhs_col","suffix") := NULL]
    setnames(dt_long, "prefix", "POE")

    # remove POE>=1 or NA
    dt_long <- dt_long[!is.na(POE) & POE < 1]

    # 6) compute AEP, TR if ITo>0
    if (!is.na(ITo) && ITo>0) {
      dt_long[, AEP := -log(1 - POE)/ITo]
      dt_long[, TR  := -ITo / log(1 - POE)]
    } else {
      dt_long[, `:=`(AEP=NA_real_, TR=NA_real_)]
    }

    # store p, ITo
    dt_long[, `:=`(p=p_val, ITo=ITo)]

    out_list[[ iCount <- iCount+1 ]] <- dt_long
  }

  # cleanup
  unlink(tmp_dir, recursive=TRUE, force=TRUE)
  DT <- rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(DT)) return(DT)

  # reorder final columns
  final_cols <- c("lon","lat","depth","Tn","p","POE","AEP","TR","ITo","Sa")
  keepC <- intersect(final_cols, names(DT))
  setcolorder(DT, keepC)
  return(DT[])
}
