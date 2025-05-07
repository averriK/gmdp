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
