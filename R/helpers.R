# helpers.R
# =========
# Minimal internal helper functions for reading, parsing, param expansions,
# and site amplification. This replicates your old logic as closely as possible.

##############################################################################
# 1) Parse OpenQuake Header Lines
##############################################################################

.extractQuantileFromHeader <- function(line) {
  if (grepl("kind='mean'", line)) {
    return("mean")
  }
  mt <- regexpr("kind='quantile-([0-9\\.]+)'", line)
  if (mt > 0) {
    val  <- regmatches(line, mt)
    qval <- sub("kind='quantile-", "", val)
    qval <- sub("'", "", qval)
    return(suppressWarnings(as.numeric(qval)))
  }
  return(NA)
}

.extractInvestigationTime <- function(line) {
  mt <- regexpr("investigation_time=([0-9\\.]+)", line)
  if (mt > 0) {
    val <- regmatches(line, mt)
    num <- sub("investigation_time=", "", val)
    return(as.numeric(num))
  }
  return(NA_real_)
}

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

##############################################################################
# 2) Safely parse "0.00995~PGA" => (POE, Tn)
##############################################################################
safeParseUHS <- function(col_labels) {
  n <- length(col_labels)
  POE  <- numeric(n)
  Tn   <- numeric(n)
  POE[] <- NA_real_
  Tn[]  <- NA_real_

  for (i in seq_len(n)) {
    clb <- col_labels[i]
    parts <- strsplit(clb, "~", fixed=TRUE)[[1]]
    if (length(parts) < 2) next
    prefix <- suppressWarnings(as.numeric(parts[1]))
    if (is.na(prefix) || prefix >= 1) next
    suffix <- parts[2]
    tn_val <- NA_real_
    if (grepl("PGA", suffix, ignore.case=TRUE)) {
      tn_val <- 0
    } else {
      mt <- regexpr("SA\\(([0-9\\.]+)\\)", suffix)
      if (mt > 0) {
        val2 <- regmatches(suffix, mt)
        val2 <- sub("^SA\\(", "", val2)
        val2 <- sub("\\)$", "", val2)
        tn_val <- suppressWarnings(as.numeric(val2))
      }
    }
    if (!is.na(tn_val)) {
      POE[i] <- prefix
      Tn[i]  <- tn_val
    }
  }
  list(POE, Tn)
}

##############################################################################
# 3) Import UHS from OQ CSV/ZIP
##############################################################################
importModel.oqUHS <- function(path) {
  if (!dir.exists(path)) stop("Path does not exist: ", path)

  tmp_dir <- file.path(path, paste0(".temp_oqUHS_", as.integer(Sys.time())))
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive=TRUE, force=TRUE)
  dir.create(tmp_dir, showWarnings=FALSE)

  zip_files <- list.files(path, pattern="uhs-csv\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zf, exdir=tmp_dir, junkpaths=TRUE)
    }
  }
  search_dir <- if (length(zip_files)) tmp_dir else path
  uhs_files <- list.files(search_dir, pattern="uhs.*\\.csv$", full.names=TRUE)
  if (!length(uhs_files)) {
    unlink(tmp_dir, recursive=TRUE, force=TRUE)
    message("No UHS CSV files in: ", path)
    return(data.table())
  }

  out_list <- list()
  iCount   <- 0

  for (f_ in uhs_files) {
    header_line <- tryCatch(readLines(f_, n=1L), error=function(e)"")
    p_val <- .extractQuantileFromHeader(header_line)
    ITval <- .extractInvestigationTime(header_line)

    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    # remove lat/lon/depth if they exist
    dropCols <- intersect(c("lat","lon","depth"), col_names)
    keepCols <- setdiff(col_names, dropCols)
    dt_raw <- dt_raw[, ..keepCols]

    measure_cols <- setdiff(keepCols, dropCols) # but we might refine
    # Actually we want everything except 'p' or 'IT' columns? We'll just meltdown all but measure
    # Let's just meltdown everything except measure. We'll do a safer approach:

    # meltdown
    id_cols <- c()  # no lat/lon/depth
    # if you do have some "p" column in the CSV? Usually not. We'll just meltdown all columns
    measure_cols <- keepCols  # meltdown everything

    dt_long <- melt(
      dt_raw,
      measure.vars=measure_cols,
      variable.name="col_label",
      value.name="Sa",
      variable.factor=FALSE      # ensures col_label is character
    )
    dt_long[, col_label := as.character(col_label)]
    dt_long[, Sa:=as.numeric(Sa)]

    # parse (POE,Tn)
    parsed <- safeParseUHS(dt_long$col_label)
    dt_long[, `:=`(POE=parsed[[1]], Tn=parsed[[2]])]
    dt_long <- dt_long[!is.na(POE) & !is.na(Tn) & POE<1]
    dt_long[, col_label:=NULL]

    if (!is.na(ITval) && ITval>0) {
      dt_long[, AEP := -log(1 - POE)/ITval]
      dt_long[, TR  := -ITval / log(1 - POE)]
    } else {
      dt_long[, `:=`(AEP=NA_real_, TR=NA_real_)]
    }
    dt_long[, `:=`(p=p_val, IT=ITval)]
    out_list[[ iCount <- iCount+1 ]] <- dt_long
  }

  unlink(tmp_dir, recursive=TRUE, force=TRUE)
  DT <- data.table::rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(DT)) return(DT)

  final_cols <- c("p","Tn","POE","AEP","TR","IT","Sa")
  keepC <- intersect(final_cols, names(DT))
  setcolorder(DT, keepC)
  return(DT[])
}


##############################################################################
# 4) Import AEP from OQ
##############################################################################
importModel.oqAEP <- function(path, vref) {
  if (!dir.exists(path)) stop("Path does not exist: ", path)

  temp_subdir <- file.path(path, paste0(".temp_oqAEP_", as.integer(Sys.time())))
  if (dir.exists(temp_subdir)) unlink(temp_subdir, recursive=TRUE, force=TRUE)
  dir.create(temp_subdir, showWarnings=FALSE)

  zip_files <- list.files(path, pattern="\\.zip$", full.names=TRUE)
  if (length(zip_files)) {
    for (zf in zip_files) {
      utils::unzip(zf, exdir=temp_subdir, junkpaths=TRUE)
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
    message("No hazard_curve/quantile_curve CSV found in: ", path)
    return(data.table())
  }

  out_list <- list()
  iCount   <- 0

  for (f_ in files_curves) {
    # Parse header => p, ITo, Tn
    line1 <- tryCatch(readLines(f_, n=1L), error=function(e)"")
    p_val <- .extractQuantileFromHeader(line1)
    ITo   <- .extractInvestigationTime(line1)
    TnVal <- .extractTnFromHeader(line1)

    # skip line #1 => read line #2 as col names
    dt_raw <- data.table::fread(f_, skip=1, header=FALSE, blank.lines.skip=TRUE)
    if (!nrow(dt_raw)) next

    col_names <- unlist(dt_raw[1,], use.names=FALSE)
    setnames(dt_raw, col_names)
    dt_raw <- dt_raw[-1]

    # Possibly remove lat/lon/depth columns right away
    dropCols <- c("lat","lon","depth")
    keepCols <- setdiff(col_names, dropCols)
    dt_raw <- dt_raw[, ..keepCols]

    # measure columns => poe-xxx
    measure_cols <- grep("^poe-[0-9\\.]+$", keepCols, value=TRUE)
    if (!length(measure_cols)) next

    # meltdown
    id_cols <- setdiff(keepCols, measure_cols)
    dt_long <- melt(
      dt_raw,
      id.vars       = id_cols,
      measure.vars  = measure_cols,
      variable.name = "poe_label",
      value.name    = "POE"
    )
    dt_long[, poe_label := as.character(poe_label)]

    dt_long[, POE := as.numeric(POE)]
    dt_long[, Sa  := as.numeric(sub("poe-", "", poe_label))]
    dt_long[, poe_label := NULL]

    dt_long <- dt_long[!is.na(POE) & POE < 1]
    if (!is.na(ITo) && ITo>0) {
      dt_long[, AEP := -log(1 - POE)/ITo]
      dt_long[, TR  := -ITo / log(1 - POE)]
    } else {
      dt_long[, `:=`(AEP=NA_real_, TR=NA_real_)]
    }
    dt_long[, `:=`(p=p_val, Tn=TnVal, ITo=ITo)]
    out_list[[ iCount <- iCount+1 ]] <- dt_long
  }

  unlink(temp_subdir, recursive=TRUE, force=TRUE)
  DT <- data.table::rbindlist(out_list, fill=TRUE, use.names=TRUE)
  if (!nrow(DT)) return(DT)

  # reorder columns => no lat/lon/depth
  final_cols <- c("p","Tn","Sa","POE","AEP","TR","ITo")
  keepC <- intersect(final_cols, names(DT))
  setcolorder(DT, keepC)

  return(DT[])
}


##############################################################################
# 4) Import  from OQ
##############################################################################


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

##############################################################################
# 4) Import  from user
##############################################################################

importModel.userAEP <- function(path = NULL, filename = "AEP.xlsx") {
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`: must be a directory.")
  }

  file_xlsx <- file.path(path, filename)
  if (!file.exists(file_xlsx)) {
    stop("File not found: ", file_xlsx)
  }

  sheets_all <- readxl::excel_sheets(file_xlsx)
  # We expect sheet names like "p=0.16", "p=mean", etc.
  sheets_p <- grep(pattern = "^p=", sheets_all, value = TRUE)
  if (length(sheets_p) == 0) {
    stop("No sheets named 'p=...' found in: ", file_xlsx)
  }

  AT <- data.table()
  for (SHEET in sheets_p) {
    dt_sheet <- data.table::as.data.table(
      readxl::read_xlsx(file_xlsx, sheet = SHEET)
    )
    if (!("Tn" %in% names(dt_sheet))) {
      stop("Missing column 'Tn' in sheet '", SHEET, "'.")
    }

    # We'll assume all other columns besides Tn are "ground motion" columns
    id_var <- "Tn"
    measure_vars <- setdiff(names(dt_sheet), id_var)
    if (length(measure_vars) == 0) {
      warning("No measure columns in sheet '", SHEET, "'. Skipping.")
      next
    }

    # Reshape from wide to long
    aux <- melt(
      dt_sheet,
      id.vars = id_var,
      measure.vars = measure_vars,
      variable.name = "Sa",
      value.name    = "AEP",
      variable.factor = FALSE
    )
    aux[, Sa := as.character(Sa)]

    # parse the "p" from the sheet name, e.g. "p=0.16" -> 0.16
    po <- stringr::str_remove(SHEET, "p=")
    if (po != "mean") {
      po_num <- suppressWarnings(as.numeric(po))
      if (!is.na(po_num)) {
        po <- po_num
      }
      # else it might remain a character if it's not numeric
    }

    # Filter out non-positive AEP
    aux <- aux[AEP > 0]
    # Convert Sa factor or string to numeric
    if (!is.numeric(aux$Sa)) {
      # attempt to parse numeric from the factor/character
      aux[, Sa := suppressWarnings(as.numeric(as.character(Sa)))]
    }

    aux[, p := po]
    aux[, IT := 50]   # default
    aux[, POE := 1 - exp(-IT * AEP)] # exact relationship
    aux[, TR := 1 / AEP]

    AT <- rbind(AT, aux, use.names = TRUE, fill = TRUE)
  }

  # reorder columns
  col_order <- c("Tn","Sa","AEP","p","POE","TR","IT")
  col_order <- intersect(col_order, names(AT))
  setcolorder(AT, col_order)

  return(AT[])
}

##############################################################################
# 5) Param expansions (unchanged public names)
##############################################################################
buildParamHaz <- function(fitDT, AEP_in) {
  ITo <- if ("ITo" %in% names(AEP_in)) unique(AEP_in$ITo)[1] else 50
  TRseq <- seq(100,10000,25)

  fitDT[
    ,
    {
      Sa_calc    <- exp(a + b*log(TRseq) + c*(1/TRseq))
      AEP_approx <- 1 / TRseq
      POE_approx <- 1 - exp(-ITo*(1/TRseq))
      data.table(
        TR  = TRseq,
        Sa  = Sa_calc,
        AEP = AEP_approx,
        POE = POE_approx,
        ITo = ITo
      )
    },
    by=.(lat,lon,depth,p,Tn,a,b,c,sdLnA,R2,MSE,RMSE,fit)
  ]
}

buildParamUHS <- function(fitDT, AEP_in) {
  ITo <- if ("ITo" %in% names(AEP_in)) unique(AEP_in$ITo)[1] else 50
  TRo <- unique(c(seq(100,10000,25), 475,975,2475,5000,10000))

  fitDT[
    ,
    {
      Sa_calc    <- exp(a + b*log(TRo) + c*(1/TRo))
      AEP_approx <- 1 / TRo
      POE_approx <- 1 - exp(-ITo*(1/TRo))
      data.table(
        TR  = TRo,
        Sa  = Sa_calc,
        AEP = AEP_approx,
        POE = POE_approx,
        ITo = ITo
      )
    },
    by=.(lat,lon,depth,p,Tn)
  ]
}

##############################################################################
# 6) Site Amp helpers (same names)
##############################################################################
applySiteAmp <- function(dt, vs30, vref, quantile_AF) {
  grouping <- c()
  for (cc in c("ID","lat","lon","depth","p","TR")) {
    if (cc %in% names(dt)) grouping <- c(grouping, cc)
  }

  resAF <- data.table()
  for (Vs in vs30) {
    dt2 <- copy(dt)
    # define PGA from Tn=0 if missing
    dt2[Tn==0 & is.na(PGA) & !is.na(Sa), PGA:=Sa]

    fullG <- unique(c(grouping,"Tn"))
    AFdt <- dt2[
      ,
      fitModel.AF.TR(.SD, pga=PGA, q=quantile_AF, Tn=Tn, vs30=Vs, vref=vref),
      by=fullG
    ]
    resAF <- rbind(resAF, AFdt, fill=TRUE)
  }
  return(resAF)
}

mergeAF <- function(dt, AFdt) {
  joinC <- intersect(names(dt), names(AFdt))
  dt2 <- AFdt[dt, on=joinC]
  if ("AF" %in% names(dt2)) {
    if ("Sa"  %in% names(dt2)) dt2[, Sa  := Sa  * AF]
    if ("PGA" %in% names(dt2)) dt2[, PGA := PGA * AF]
  }
  return(dt2[])
}

