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
#' @importFrom data.table data.table fread setnames
#' @importFrom utils unzip
#' @importFrom stringr str_extract
#'
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
