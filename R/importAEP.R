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

    foundPGA <- FALSE

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

        if (is.na(Tn) && grepl("imt='PGA'", HEADER)) {
            Tn <- 0
        }
        if (Tn == 0) foundPGA <- TRUE

        DATA[, AEP := -log(1 - POE) / ITo]
        DATA[, TR := -ITo / log(1 - POE)]
        DATA[, `:=`(Tn = Tn, p = p, ITo = ITo)]

        DT <- data.table::rbindlist(list(DT, DATA), use.names = TRUE)
    }

    # remove infinite or negative TR
    DT <- DT[is.finite(TR) & TR > 0 & !is.na(TR)]

    if (!foundPGA && nrow(DT[Tn >= 0])) {
        warning("No 'imt=PGA' found => building synthetic Tn=0 from min Tn>=0.")
        groupCols <- c("lon", "lat", "depth", "p", "ITo")
        FAKES <- DT[Tn >= 0, .SD[which.min(Tn)], by = groupCols]
        FAKES[, Tn := 0]
        DT <- data.table::rbindlist(list(DT, FAKES), use.names = TRUE)
    }

    if (!nrow(DT)) {
        stop("No valid hazard data in: ", path)
    }

    return(DT[])
}
