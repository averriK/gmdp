
# Helpers:

importModel.oqUHS <- function(path) {
  if (!dir.exists(path)) {
    stop("[ERROR] Path does not exist: ", path)
  }

  # Define the dictionary from POE => TR
  poe_to_tr_map <- c(
    "0.0999" = 475,
    "0.0800" = 600,
    "0.0500" = 975,
    "0.0400" = 1225,
    "0.0250" = 1975,
    "0.0200" = 2475,
    "0.0100" = 4975,
    "0.0080" = 6225,
    "0.0050" = 9975,
    "0.0040" = 12475,
    "0.0025" = 19975
  )

  tmp_dir <- file.path(path, paste0(".temp_oqUHS_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
  }
  dir.create(tmp_dir, showWarnings=FALSE)

  # Unzip any "uhs-csv.zip"
  zip_files <- list.files(path, pattern="uhs-csv\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zf, exdir=tmp_dir, junkpaths=TRUE)
    }
  }
  search_dir <- if (length(zip_files)) tmp_dir else path

  # Find "uhs.*.csv" files
  uhs_files <- list.files(search_dir, pattern="uhs.*\\.csv$", full.names=TRUE)
  if (!length(uhs_files)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("[INFO] No UHS CSV files in: ", path)
    return(data.table())
  }

  out_list <- list()
  iCount <- 0
  for (f_ in uhs_files) {
    header_line <- tryCatch(readLines(f_, n=1L), error=function(e) "")
    p_val <- .extractQuantileFromHeader(header_line)
    ITval <- .extractInvestigationTime(header_line)

    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    # The first row of dt_raw has col names
    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    # Possibly drop lat/lon/depth columns
    dropCols <- intersect(c("lat","lon","depth"), col_names)
    dt_raw <- dt_raw[, !col_names %in% dropCols, with=FALSE]

    # Identify measure cols like "0.099900~PGA", "0.050000~SA(0.1)", etc.
    measure_cols <- grep("^[0-9\\.]+~", names(dt_raw), value=TRUE)
    id_cols      <- setdiff(names(dt_raw), measure_cols)

    # Convert wide->long so each row is one combination of POE, Tn, Sa
    dt_long <- melt(
      dt_raw,
      id.vars        = id_cols,
      measure.vars   = measure_cols,
      variable.name  = "col_label",
      value.name     = "Sa",
      variable.factor=FALSE
    )
    dt_long[, Sa := as.numeric(Sa)]

    # We parse out (POE, Tn_m) from e.g. "0.099900~SA(0.05)" or "~PGA"
    parsed <- safeParseUHS(dt_long$col_label)
    dt_long[, `:=`(POE = parsed[[1]], Tn_m = parsed[[2]])]
    dt_long[, col_label := NULL]

    # Keep only valid rows
    dt_long <- dt_long[!is.na(POE) & !is.na(Tn_m) & POE < 1]

    # If meltdown doesn't define Tn, rename Tn_m -> Tn
    if (!"Tn" %in% names(dt_long)) {
      setnames(dt_long, "Tn_m", "Tn")
    } else {
      dt_long[, Tn_m := NULL]
    }

    # If meltdown doesn't define fractile p, use p_val
    if (!"p" %in% names(dt_long)) {
      dt_long[, p := p_val]
    }

    # If meltdown doesn't define TR or AEP, we fill them from the dictionary
    #   1) string-match POE to the dictionary
    #   2) fallback if no match
    dt_long[, POE_str := format(POE, nsmall=4)]
    dt_long[, TR_dict := poe_to_tr_map[POE_str]]

    # If meltdown gave a TR column, we keep it
    # else we set TR from TR_dict
    if (!"TR" %in% names(dt_long)) {
      dt_long[, TR := TR_dict]
    } else {
      # meltdown has a TR => we do not overwrite
      # but let's ensure it's numeric
      dt_long[, TR := as.numeric(TR)]
    }

    # If meltdown gave an AEP column, keep it
    # else define AEP=1/TR if TR is known
    if (!"AEP" %in% names(dt_long)) {
      dt_long[, AEP := ifelse(!is.na(TR), 1/TR, NA_real_)]
    } else {
      dt_long[, AEP := as.numeric(AEP)]
    }

    # If meltdown didn't have TR or we don't have a dictionary match => TR=NA => AEP=NA
    # That might happen if OQ used a different POE, e.g. 0.07 => no dictionary entry

    # drop helper columns
    dt_long[, c("TR_dict","POE_str") := NULL]

    # store the investigation time
    dt_long[, IT := ITval]

    out_list[[ iCount <- iCount + 1 ]] <- dt_long
  }

  unlink(tmp_dir, recursive=TRUE, force=TRUE)

  # Combine into one DT
  DT <- rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(DT)) return(DT)

  # Reorder columns
  final_cols <- c("p","Tn","POE","AEP","TR","IT","Sa")
  keepC <- intersect(final_cols, names(DT))
  setcolorder(DT, keepC)

  return(DT[])
}




importModel.oqAEP <- function(path, vref) {
  if (!dir.exists(path)) stop("Path does not exist: ", path)

  tmp_dir <- file.path(path, paste0(".temp_oqAEP_", as.integer(Sys.time())))
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

  files_curves <- list.files(
    search_dir,
    pattern="(hazard_curve|quantile_curve).*\\.csv$",
    full.names=TRUE
  )
  if (!length(files_curves)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("No hazard_curve/quantile_curve CSV found in: ", path)
    return(data.table())
  }

  out_list <- list()
  iCount   <- 0

  for (f_ in files_curves) {
    line1 <- tryCatch(readLines(f_, n=1L), error=function(e)"")
    p_val <- .extractQuantileFromHeader(line1)
    ITo   <- .extractInvestigationTime(line1)
    TnVal <- .extractTnFromHeader(line1)

    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    dropCols <- intersect(c("lat","lon","depth"), col_names)
    keepCols <- setdiff(col_names, dropCols)
    dt_raw <- dt_raw[, ..keepCols]

    measure_cols <- grep("^poe-[0-9\\.]+$", keepCols, value=TRUE)
    id_cols <- setdiff(keepCols, measure_cols)

    dt_long <- melt(
      dt_raw,
      id.vars       = id_cols,
      measure.vars  = measure_cols,
      variable.name = "poe_label",
      value.name    = "POE",
      variable.factor=FALSE
    )
    dt_long[, POE := as.numeric(POE)]
    dt_long[, Sa  := as.numeric(sub("poe-", "", poe_label))]
    dt_long[, poe_label := NULL]

    dt_long <- dt_long[!is.na(POE) & POE<1]

    if (!is.na(ITo) && ITo>0) {
      dt_long[, AEP := -log(1 - POE)/ITo]
      dt_long[, TR  := -ITo / log(1 - POE)]
    } else {
      dt_long[, `:=`(AEP=NA_real_, TR=NA_real_)]
    }

    if (!"Tn" %in% names(dt_long)) {
      dt_long[, Tn := TnVal]
    }
    if (!"p" %in% names(dt_long)) {
      dt_long[, p := p_val]
    }
    dt_long[, ITo := ITo]

    out_list[[ iCount <- iCount+1 ]] <- dt_long
  }

  unlink(tmp_dir, recursive=TRUE, force=TRUE)
  DT <- data.table::rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(DT)) return(DT)

  final_cols <- c("p","Tn","Sa","POE","AEP","TR","ITo")
  keepC <- intersect(final_cols, names(DT))
  setcolorder(DT, keepC)

  return(DT[])
}


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
    dt_raw <- tryCatch(data.table::fread(f_, skip=1, header=TRUE, blank.lines.skip=TRUE),
                       error=function(e) NULL)
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
    rlz_col <- grep("rlz|mean", names(dt_raw), value=TRUE)
    if (length(rlz_col)==1) {
      setnames(dt_raw, old=rlz_col, new="p")
    } else {
      next
    }
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
  DHT <- unique(DHT)
  return(DHT[])
}


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
      value.name    = "AEP",
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

  col_order <- c("Tn","Sa","AEP","p","POE","TR","IT")
  col_order <- intersect(col_order, names(AT))
  setcolorder(AT, col_order)

  return(AT[])
}



