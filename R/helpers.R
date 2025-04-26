
#' @noRd

.find <- function(V, X) {
  # Ensure V is sorted
  V <- sort(unique(V))
  X <- sort(unique(X))
  # Initialize the result vector
  results <- numeric(length(X))

  # Process each element in X
  for (i in seq_along(X)) {
    x <- X[i]

    # Check if x is exactly in V
    if (x %in% V) {
      results[i] <- x
    } else {
      # Find the closest indices before and after x
      idx_before <- max(which(V < x), na.rm = TRUE)
      idx_after <- min(which(V > x), na.rm = TRUE)

      # Handle edge cases when x is outside the range of V
      if (length(idx_before) == 0) { # x is less than all elements in V
        idx_before <- idx_after
      }
      if (length(idx_after) == 0) { # x is more than all elements in V
        idx_after <- idx_before
      }

      # Fetch the closest values
      # Choose the closer one or both if x is equally distant from V[idx_before] and V[idx_after]
      if (abs(V[idx_before] - x) < abs(V[idx_after] - x)) {
        results[i] <- V[idx_before]
      } else if (abs(V[idx_before] - x) > abs(V[idx_after] - x)) {
        results[i] <- V[idx_after]
      } else {
        # If both are equally close, return the average or any other logic you want
        results[i] <- mean(c(V[idx_before], V[idx_after]))
      }
    }
  }

  return(results)
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

