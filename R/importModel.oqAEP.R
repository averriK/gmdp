importModel.oqAEP <- function(path, vref) {
  . <- NULL
  DT <- data.table()

  # find hazard files
  FILES <- c(
    list.files(path, pattern="quantile_curve"),
    list.files(path, pattern="hazard_curve")
  )
  if (length(FILES) == 0) {
    stop("No hazard_curve or quantile_curve files found in ", path)
  }

  DATAPATH <- data.table(
    name = FILES,
    datapath = file.path(path, FILES)
  )

  for (k in seq_len(nrow(DATAPATH))) {
    fcsv <- DATAPATH$datapath[k]

    # read header line
    HEADER <- readLines(fcsv, n=1)

    p   <- .extractQuantileFromHeader(HEADER)
    ITo <- .extractInvestigationTime(HEADER)
    Tn  <- .extractTnFromHeader(HEADER)  # 0 if 'PGA', -1 if 'PGV', numeric if 'SA(x)'

    # read the file skipping first line
    AUX <- data.table::fread(fcsv, skip=1, header=FALSE, blank.lines.skip=TRUE)
    COLS <- unlist(AUX[1]) |> as.vector()
    data.table::setnames(AUX, COLS)
    AUX <- AUX[-1]

    # meltdown on "poe-xxx"
    poeCols <- grep("poe-", COLS, value=TRUE)
    DATA <- data.table::melt(
      data = AUX,
      id.vars = c("lon", "lat", "depth"),
      measure.vars = poeCols,
      variable.name = "SaCol",
      value.name    = "POE"
    )
    # parse numeric Sa from 'poe-xxx'
    DATA[, Sa := as.numeric(gsub("^poe-", "", SaCol))]
    DATA[, POE := as.numeric(POE)]

    # define AEP / TR
    DATA[, AEP := -log(1-POE)/ITo]
    DATA[, TR  := -ITo/log(1-POE)]

    DATA[, `:=`(ITo=ITo, Tn=Tn, p=p)]

    DT <- data.table::rbindlist(list(DT, DATA), use.names=TRUE)
  }

  # AFTER building the table, do final filtering as old code:
  # remove infinite or NA TR, remove Tn=-1 if ignoring PGV
  # (or maybe you used to do it inside the function.)
  DT <- DT[ is.finite(TR) & TR>0 & !is.na(TR) ]
  # If ignoring PGV lines:
  DT <- DT[Tn != -1]

  return(DT[])
}
