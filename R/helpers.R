#' Find closest values in a vector
#'
#' @description
#' Internal function to find the closest values in a sorted vector V for each element in X.
#'
#' @param V Numeric vector. The reference vector to search in.
#' @param X Numeric vector. The values to find closest matches for.
#'
#' @return Numeric vector of the same length as X containing the closest values from V.
#'
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

#' Import OpenQuake Magnitude-Distance Disaggregation Data
#'
#' @description
#' Imports magnitude-distance disaggregation data from OpenQuake output files.
#'
#' @param path Character. Path to the directory containing OpenQuake output files.
#' @param ITo Numeric. Investigation time in years.
#' @param vref Numeric. Reference Vs30 value.
#'
#' @return A data.table containing the disaggregation data, or NULL if no valid data is found.
#'
#' @importFrom data.table data.table fread setnames rbind
#' @importFrom utils unzip
#' @importFrom stringr str_extract
#'
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

    zip_files <- list.files(path, pattern = "\\.zip$", full.names = TRUE)
    if (length(zip_files)) {
        for (zf in zip_files) {
            utils::unzip(zf, exdir = tmp_dir, junkpaths = TRUE)
        }
    }
    search_dir <- if (length(zip_files)) tmp_dir else path

    all_files <- list.files(search_dir, pattern = "Mag_Dist", full.names = TRUE)
    all_files <- all_files[!grepl("TRT", all_files)]
    if (!length(all_files)) {
        unlink(tmp_dir, recursive = TRUE, force = TRUE)
        message("No 'Mag_Dist' files found in: ", path)
        return(NULL)
    }
    mean_files <- grep("Mag_Dist-mean", all_files, value = TRUE)
    if (length(mean_files)) {
        all_files <- mean_files
    }

    DHT <- data.table()
    for (f_ in all_files) {
        meta_line <- tryCatch(readLines(f_, n = 1L), error = function(e) "")
        dt_raw <- tryCatch(data.table::fread(f_, skip = 1, header = TRUE, blank.lines.skip = TRUE),
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

        setnames(dt_raw, old = c("mag", "dist", "poe"), new = c("Mw", "R", "POE"), skip_absent = TRUE)
        if ("iml" %in% names(dt_raw)) {
            dt_raw[, iml := NULL]
        }
        rlz_col <- grep("rlz|mean", names(dt_raw), value = TRUE)
        if (length(rlz_col) == 1) {
            setnames(dt_raw, old = rlz_col, new = "p")
        } else {
            next
        }
        dt_raw[imt == "PGA", imt := "Sa(0.0)"]
        dt_raw[, Tn := stringr::str_extract(imt, "(?<=\\()\\d+\\.*\\d*(?=\\))")]
        dt_raw[, Tn := as.numeric(Tn)]
        dt_raw[is.na(Tn), Tn := 0]

        dt_raw[, IT := ITo]
        dt_raw[, `:=`(AEP = POE / IT, TR = 1 / (POE / IT))]
        DHT <- rbind(DHT, dt_raw, fill = TRUE)
    }

    unlink(tmp_dir, recursive = TRUE, force = TRUE)
    if (!nrow(DHT)) {
        message("No valid disagg data found.")
        return(NULL)
    }
    DHT <- unique(DHT)
    return(DHT[])
}

#' Import User-Provided Annual Exceedance Probability Data
#'
#' @description
#' Imports annual exceedance probability data from a user-provided Excel file.
#'
#' @param path Character. Path to the directory containing the Excel file.
#' @param filename Character. Name of the Excel file. Default is "AEP.xlsx".
#'
#' @return A data.table containing the annual exceedance probability data.
#'
#' @importFrom data.table data.table as.data.table melt setcolorder
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom stringr str_remove
#'
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

#' Import OpenQuake Annual Exceedance Probability Data
#'
#' @description
#' Imports annual exceedance probability data from OpenQuake output files.
#'
#' @param path Character. Path to the directory containing OpenQuake output files.
#' @param vref Numeric. Reference Vs30 value.
#'
#' @return A data.table containing the annual exceedance probability data.
#'
#' @importFrom data.table data.table fread setnames rbind
#' @importFrom utils unzip
#' @importFrom stringr str_extract
#'
#' @noRd
importModel.oqAEP <- function(path, vref) {
    . <- NULL
    DT <- data.table()

    FILES <- c(
        list.files(path, pattern = "quantile_curve"),
        list.files(path, pattern = "hazard_curve")
    )
    if (length(FILES) == 0) {
        stop("No hazard_curve or quantile_curve files found in ", path)
    }
    DATAPATH <- data.table(
        name     = FILES,
        datapath = file.path(path, FILES)
    )

    foundPGA <- FALSE

    for (k in seq_len(nrow(DATAPATH))) {
        fcsv <- DATAPATH$datapath[k]

        # read the header line
        HEADER <- readLines(fcsv, n = 1)
        p <- .extractQuantileFromHeader(HEADER)
        ITo <- .extractInvestigationTime(HEADER)
        Tn <- .extractTnFromHeader(HEADER) # => Tn=0 if 'imt="PGA"'
        if (Tn == 0) foundPGA <- TRUE

        # read file
        AUX <- data.table::fread(fcsv, skip = 1, header = FALSE, blank.lines.skip = TRUE)
        COLS <- unlist(AUX[1]) |> as.vector()
        setnames(AUX, COLS)
        AUX <- AUX[-1]

        # meltdown
        poeCols <- grep("poe-", COLS, value = TRUE)
        DATA <- melt(
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

        DT <- rbindlist(list(DT, DATA), use.names = TRUE)
    }

    # remove infinite or negative TR
    DT <- DT[is.finite(TR) & TR > 0 & !is.na(TR)]

    if (!foundPGA) {
        warning("No 'imt=PGA' found in openquake files => building synthetic Tn=0 from min Tn>=0.")
        groupCols <- c("lon", "lat", "depth", "p", "ITo")
        FAKES <- DT[Tn >= 0, .SD[which.min(Tn)], by = groupCols]
        FAKES[, Tn := 0]
        DT <- rbindlist(list(DT, FAKES), use.names = TRUE)
    }

    return(DT[])
}

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


reMeshCurve <- function(TRi, Sai, TRo) {
    # TRi : numeric vector of original return periods
    # Sai : numeric vector of spectral acceleration or hazard values
    # TRo : numeric vector of new return periods to interpolate onto

    # We assume TRi and Sai are sorted or will handle sorting outside.
    # (But you can also sort them here if you like.)

    nOut <- length(TRo)
    Sa_star <- numeric(nOut)

    for (j in seq_len(nOut)) {
        trT <- TRo[j]
        if (trT <= TRi[1]) {
            # Below the smallest known period => clamp to first value
            Sa_star[j] <- Sai[1]
        } else if (trT >= TRi[length(TRi)]) {
            # Above the largest known period => clamp to last value
            Sa_star[j] <- Sai[length(Sai)]
        } else {
            # Inside the known range => do log interpolation
            idx_high <- which(TRi >= trT)[1]
            idx_low <- idx_high - 1
            frac <- (trT - TRi[idx_low]) / (TRi[idx_high] - TRi[idx_low])
            val_log <- log(Sai[idx_low]) + frac * (log(Sai[idx_high]) - log(Sai[idx_low]))
            Sa_star[j] <- exp(val_log)
        }
    }
    Sa_star
}

remeshGroup <- function(.SD, TRo) {
    # Verify we have at least these columns:
    if (!all(c("TR", "Sa") %in% colnames(.SD))) {
        stop("'.SD' must contain columns 'TR' and 'Sa'.")
    }

    # Sort by TR
    data.table::setorder(.SD, TR)

    # Filter out invalid or non-positive TR
    .SD <- .SD[is.finite(TR) & TR > 0]
    if (nrow(.SD) < 2) {
        # Not enough points for interpolation
        return(NULL) # or stop() if you prefer
    }

    # Do the interpolation
    TRi <- .SD$TR
    Sai <- .SD$Sa
    Sa_star <- reMeshCurve(TRi, Sai, TRo)

    # Return only the new TR & Sa columns (one row per TRo)
    data.table(TR = TRo, Sa = Sa_star)
}
