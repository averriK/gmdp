#' Build Ground-Motion Design Parameters (GMDP)
#'
#' Assembles an annual-exceedance-probability (AEP) table, uniform-hazard-spectrum
#' (UHS) table, and—where available—a magnitude–distance disaggregation table,
#' using OpenQuake or user-supplied hazard files.
#'
#' @param IDo    Character. Identifier for the GMDP. Default `"gmdp"`.
#' @param path   Character. Directory with hazard data files.
#' @param engine Character. `"openquake"` (default) or `"user"`.
#' @param vref   Numeric. Reference Vs30 (m/s). Default 760.
#' @param TRo    Numeric vector. Target return periods (years).
#'
#' @import data.table

#' @return A named list with `AEPTable`, `UHSTable`, and `RMwTable`
#'   (`NULL` if disaggregation is unavailable).
#'
#' @export
buildGMDP <- function(path,
                      IDo = "GEM",
                      engine = "openquake",
                      vref = 760,
                      TRo = c(475, 975, 1975, 2475, 4975, 9975)) {
    OUT <- importAEPTable(path = path, engine = engine, vref = vref)
    AEPTable <- OUT$AEPTable
    ITo <- unique(AEPTable$ITo)[1]
    RMwTable <- NULL

    if (engine == "openquake") {
        message("> Building Disaggregation Hazard Table...")
        RMwTable <- tryCatch(
            importModel.oqRMw(path = path, ITo = ITo, vref = vref),
            error = function(e) {
                message(">> Skipping disagg (", e$message, ")")
                NULL
            }
        )
        if (!is.null(RMwTable)) {
            RMwTable <- RMwTable[, `:=`(SID = Vs30toSID(vref), Vs30 = vref, SM = engine, IT = ITo)]
        } else {
            message("> Disaggregation data not available.")
        }
    }

    message("> Re-mesh hazard data on a uniform TR grid ...")
    COLS <- setdiff(names(AEPTable), c("Sa", "POE", "AEP", "TR"))
    UHSTable <- AEPTable[Tn != -1, remeshGroup(.SD, TRo), by = COLS]
    UHSTable[, `:=`(Vref = vref, Vs30 = vref, AF = 1, SaF = Sa)]
    UHSTable[, ID := IDo]
    AEPTable[, ID := IDo]

    list(
        AEPTable = AEPTable,
        UHSTable = UHSTable,
        RMwTable = RMwTable
    )
}

#' Import OpenQuake or user AEP Data (Internal)
#'
#' @keywords internal
#' @noRd
importAEPTable <- function(path, engine = "openquake", vref = 760) {
    message("> Build AEP Table...")
    AEPTable <- NULL

    if (engine == "openquake") {
        message("> Unzip OQ data ...")

        FILES <- list.files(path, pattern = "\\.zip$", full.names = TRUE)
        if (!length(FILES)) {
            stop("No .zip files found in ", path)
        }

        for (z in FILES) {
            message("> Import AEP data from: ", z)
            tryCatch(
                {
                    temp_dir <- tempdir()
                    unlink(temp_dir, recursive = TRUE)
                    dir.create(temp_dir, showWarnings = FALSE)
                    utils::unzip(z, junkpaths = TRUE, exdir = temp_dir)

                    AUX <- importModel.oqAEP(path = temp_dir, vref = vref)
                    AEPTable <- if (is.null(AEPTable)) {
                        AUX
                    } else {
                        data.table::rbindlist(list(AEPTable, AUX), use.names = TRUE, fill = TRUE)
                    }
                },
                error = function(e) {
                    message(">> Skipping file: ", z, " (", e$message, ")")
                }
            )
        }
    } else if (engine == "user") {
        message("> Import user-supplied AEP.xlsx ...")
        AEPTable <- tryCatch(
            importModel.userAEP(path, filename = "AEP.xlsx"),
            error = function(e) stop("Error in user AEP data: ", e$message)
        )
    } else {
        stop("Unknown engine: ", engine)
    }

    if (is.null(AEPTable) || !nrow(AEPTable)) {
        stop("No valid AEP data found in path: ", path)
    }
    AEPTable[, `:=`(Vref = vref, Vs30 = vref)]
    return(list(AEPTable = AEPTable[]))
}

#' Import OpenQuake AEP File (Internal)
#'
#' @keywords internal
#' @noRd
importModel.oqAEP <- function(path, vref) {
    DT <- data.table::data.table()

    FILES <- c(
        list.files(path, pattern = "quantile_curve"),
        list.files(path, pattern = "hazard_curve"),
        list.files(path, pattern = "hcurves-csv")
    )
    if (!length(FILES)) {
        stop("No hazard_curve, quantile_curve, or hcurves-csv files found in ", path)
    }

    for (.files in FILES) {
        HEADER <- readLines(file.path(path, .files), n = 1)
        p <- .extractQuantileFromHeader(HEADER)
        ITo <- .extractInvestigationTime(HEADER)
        Tn <- .extractTnFromHeader(HEADER)

        if (is.na(p) || is.na(ITo)) {
            stop("Malformed header in file: ", file.path(path, .files))
        }

        AUX <- data.table::fread(file.path(path, .files), skip = 1, header = FALSE, blank.lines.skip = TRUE)
        if (!nrow(AUX)) {
            warning("Empty data in file: ", file.path(path, .files))
            next
        }

        COLS <- unlist(AUX[1]) |> as.vector()
        data.table::setnames(AUX, COLS)
        AUX <- AUX[-1]

        poeCols <- grep("poe-", COLS, value = TRUE)
        if (!length(poeCols)) {
            stop("No 'poe-' columns in file: ", file.path(path, .files))
        }

        DATA <- data.table::melt(
            data = AUX,
            id.vars = c("lon", "lat", "depth"),
            measure.vars = poeCols,
            variable.name = "Sa",
            value.name = "POE"
        )
        DATA[, POE := as.numeric(POE)]
        DATA[, Sa := as.numeric(sub("^poe-", "", Sa))]
        DATA[, AEP := -log(1 - POE) / ITo]
        DATA[, TR := -ITo / log(1 - POE)]
        DATA[, `:=`(Tn = Tn, p = p, ITo = ITo)]

        DT <- data.table::rbindlist(list(DT, DATA), use.names = TRUE)
    }

    # remove infinite or negative TR
    DT <- DT[is.finite(TR) & TR > 0 & !is.na(TR)]

    if (!nrow(DT)) {
        stop("No valid hazard data in: ", path)
    }

    return(DT[])
}

#' Import OpenQuake Mag-Dist Disaggregation (Internal)
#'
#' @keywords internal
#' @noRd
importModel.oqRMw <- function(path, ITo, vref) {
    if (!dir.exists(path)) {
        stop("Path does not exist: ", path)
    }

    tmp_dir <- file.path(path, paste0(".temp_oqRMw_", as.integer(Sys.time())))
    if (dir.exists(tmp_dir)) {
        unlink(tmp_dir, recursive = TRUE, force = TRUE)
    }
    dir.create(tmp_dir, showWarnings = FALSE)

    FILES <- list.files(path, pattern = "\\.zip$", full.names = TRUE)
    if (length(FILES)) {
        for (.files in FILES) {
            utils::unzip(.files, exdir = tmp_dir, junkpaths = TRUE)
        }
    }
    search_dir <- if (length(FILES)) tmp_dir else path

    all_files <- list.files(search_dir, pattern = "Mag_Dist", full.names = TRUE)
    all_files <- all_files[!grepl("TRT", all_files)]
    if (!length(all_files)) {
        unlink(tmp_dir, recursive = TRUE, force = TRUE)
        message("> No 'Mag_Dist' files found in path. Skipping...")
        return(NULL)
    }
    mean_files <- grep("Mag_Dist-mean", all_files, value = TRUE)
    if (length(mean_files)) {
        all_files <- mean_files
    }

    DHT <- data.table::data.table()
    for (.files in all_files) {
        meta_line <- tryCatch(readLines(.files, n = 1L), error = function(e) "")
        dt_raw <- tryCatch(data.table::fread(.files, skip = 1, header = TRUE, blank.lines.skip = TRUE),
            error = function(e) NULL
        )
        if (is.null(dt_raw) || !nrow(dt_raw)) {
            next
        }

        required_cols <- c("mag", "dist", "poe", "imt")
        missing_cols <- setdiff(required_cols, names(dt_raw))
        if (length(missing_cols)) {
            next
        }

        data.table::setnames(dt_raw, old = c("mag", "dist", "poe"), new = c("Mw", "R", "POE"), skip_absent = TRUE)
        if ("iml" %in% names(dt_raw)) dt_raw[, iml := NULL]

        rlz_col <- grep("rlz|mean", names(dt_raw), value = TRUE)
        if (length(rlz_col) == 1) {
            data.table::setnames(dt_raw, old = rlz_col, new = "p")
        } else {
            next
        }

        dt_raw[imt == "PGA", imt := "Sa(0.0)"]
        dt_raw[, Tn := stringr::str_extract(imt, "(?<=\\()\\d+\\.*\\d*(?=\\))")]
        dt_raw[, Tn := as.numeric(Tn)]
        dt_raw[is.na(Tn), Tn := 0]
        dt_raw[, IT := ITo]
        dt_raw[, `:=`(AEP = POE / IT, TR = 1 / (POE / IT))]

        DHT <- data.table::rbindlist(list(DHT, dt_raw), fill = TRUE)
    }

    unlink(tmp_dir, recursive = TRUE, force = TRUE)
    if (!nrow(DHT)) {
        message("No valid disagg data found.")
        return(NULL)
    }
    return(unique(DHT))
}

#' Internal: Extract Quantile from OpenQuake Header
#' @keywords internal
#' @noRd
.extractQuantileFromHeader <- function(line) {
    if (grepl("kind='mean'", line)) {
        return("mean")
    }
    mt <- regexpr("kind='quantile-([0-9\\.]+)'", line)
    if (mt > 0) {
        val <- regmatches(line, mt)
        qval <- sub("kind='quantile-", "", val)
        qval <- sub("'", "", qval)
        return(as.numeric(qval))
    }
    return(NA)
}

#' Internal: Extract Investigation Time from OpenQuake Header
#' @keywords internal
#' @noRd
.extractInvestigationTime <- function(line) {
    mt <- regexpr("investigation_time=([0-9\\.]+)", line)
    if (mt > 0) {
        val <- regmatches(line, mt)
        num <- sub("investigation_time=", "", val)
        return(as.numeric(num))
    }
    return(NA_real_)
}

#' Internal: Extract Tn from OpenQuake Header
#' @keywords internal
#' @noRd
.extractTnFromHeader <- function(line) {
    if (grepl("imt='PGA'", line)) {
        return(0)
    }
    if (grepl("imt='PGV'", line)) {
        return(-1)
    }
    mt <- regexpr("imt='SA\\(([0-9\\.]+)\\)'", line)
    if (mt > 0) {
        val <- regmatches(line, mt)
        num <- sub("imt='SA\\(", "", val)
        num <- sub("\\)'", "", num)
        return(as.numeric(num))
    }
    return(NA_real_)
}

#' Internal: Interpolate Hazard Curve to Uniform TR Grid
#' @keywords internal
#' @noRd
# ---------------------------------------------------------------------------
#  Patched: reMeshCurve()
#  • Same name, same 3‑argument signature.
#  • Replaces “flat‑tail” fallback with log‑log extrapolation, so each
#    quantile keeps its own slope and cannot collapse into a single value.
# ---------------------------------------------------------------------------
reMeshCurve <- function(TRi, Sai, TRo) {

  if (length(TRi) < 2L)
    stop("reMeshCurve: need at least two (TR, Sa) points for interpolation.")

  # Pre‑compute logs once – faster and clearer
  log_TRi <- log(TRi)
  log_Sai <- log(Sai)

  Sa_star <- numeric(length(TRo))

  for (j in seq_along(TRo)) {

    trT <- TRo[j]

    # ----------------- 1.  Below first point → extrapolate ------------------
    if (trT <= TRi[1]) {
      frac <- (log(trT) - log_TRi[1]) / (log_TRi[2] - log_TRi[1])
      Sa_star[j] <- exp(log_Sai[1] + frac * (log_Sai[2] - log_Sai[1]))
      next
    }

    # ----------------- 2.  Above last point → extrapolate -------------------
    if (trT >= TRi[length(TRi)]) {
      n <- length(TRi)
      frac <- (log(trT) - log_TRi[n - 1]) / (log_TRi[n] - log_TRi[n - 1])
      Sa_star[j] <- exp(log_Sai[n - 1] + frac * (log_Sai[n] - log_Sai[n - 1]))
      next
    }

    # ----------------- 3.  Inside the curve → interpolate -------------------
    if (trT %in% TRi) {                            # exact node
      Sa_star[j] <- Sai[match(trT, TRi)]
    } else {                                       # between two nodes
      idx_high <- which(TRi > trT)[1]
      idx_low  <- idx_high - 1
      frac <- (log(trT) - log_TRi[idx_low]) /
        (log_TRi[idx_high] - log_TRi[idx_low])
      Sa_star[j] <- exp(log_Sai[idx_low] +
                          frac * (log_Sai[idx_high] - log_Sai[idx_low]))
    }
  }

  Sa_star
}


#' Internal: Remesh a Group to Uniform TR Grid
#' @keywords internal
#' @noRd
remeshGroup <- function(.SD, TRo) {
    if (!all(c("TR", "Sa") %in% colnames(.SD))) {
        stop("'.SD' must contain columns 'TR' and 'Sa'.")
    }

    data.table::setorder(.SD, TR)
    .SD <- .SD[is.finite(TR) & TR > 0]
    if (nrow(.SD) < 2) {
        return(NULL)
    }

    TRi <- .SD$TR
    Sai <- .SD$Sa
    Sa_star <- reMeshCurve(TRi, Sai, TRo)
    data.table(TR = TRo, Sa = Sa_star)
}

#' Internal: Remesh a Group to Uniform TR Grid
#' @keywords internal
#' @noRd
importModel.userAEP <- function(path = NULL, filename = "AEP.xlsx") {
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`: must be a directory.")
  }

  file_xlsx <- file.path(path, filename)
  if (!file.exists(file_xlsx)) {
    stop("File not found: ", file_xlsx)
  }

  sheets_all <- readxl::excel_sheets(file_xlsx)
  sheets_p <- grep(pattern = "^p=", sheets_all, value = TRUE)
  if (length(sheets_p) == 0) {
    stop("No sheets named 'p=...' found in: ", file_xlsx)
  }

  AT <- data.table()
  for (SHEET in sheets_p) {
    dt_sheet <- data.table::as.data.table(readxl::read_xlsx(file_xlsx, sheet = SHEET))
    if (!("Tn" %in% names(dt_sheet))) {
      stop("Missing column 'Tn' in sheet '", SHEET, "'.")
    }

    id_var <- "Tn"
    measure_vars <- setdiff(names(dt_sheet), id_var)
    if (length(measure_vars) == 0) {
      warning("No measure columns in sheet '", SHEET, "'. Skipping.")
      next
    }

    aux <- melt(
      dt_sheet,
      id.vars = id_var,
      measure.vars = measure_vars,
      variable.name = "Sa",
      value.name = "AEP",
      variable.factor = FALSE
    )
    aux[, Sa := as.character(Sa)]

    po <- stringr::str_remove(SHEET, "p=")
    if (po != "mean") {
      po_num <- suppressWarnings(as.numeric(po))
      if (!is.na(po_num)) {
        po <- po_num
      }
    }

    aux <- aux[AEP > 0]
    if (!is.numeric(aux$Sa)) {
      aux[, Sa := suppressWarnings(as.numeric(as.character(Sa)))]
    }

    aux[, p := po]
    aux[, IT := 50]
    aux[, POE := 1 - exp(-IT * AEP)]
    aux[, TR := 1 / AEP]

    AT <- rbind(AT, aux, use.names = TRUE, fill = TRUE)
  }

  col_order <- c("Tn", "Sa", "AEP", "p", "POE", "TR", "IT")
  col_order <- intersect(col_order, names(AT))
  setcolorder(AT, col_order)

  return(AT[])
}
