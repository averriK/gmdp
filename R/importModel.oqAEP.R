# file: importModel.oqAEP.R

importModel.oqAEP <- function(path, vref) {
  . <- NULL
  DT <- data.table()

  FILES <- c(
    list.files(path, pattern="quantile_curve"),
    list.files(path, pattern="hazard_curve")
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
    HEADER <- readLines(fcsv, n=1)
    p   <- .extractQuantileFromHeader(HEADER)
    ITo <- .extractInvestigationTime(HEADER)
    Tn  <- .extractTnFromHeader(HEADER)  # => Tn=0 if 'imt="PGA"'
    if (Tn == 0) foundPGA <- TRUE

    # read file
    AUX <- data.table::fread(fcsv, skip=1, header=FALSE, blank.lines.skip=TRUE)
    COLS <- unlist(AUX[1]) |> as.vector()
    setnames(AUX, COLS)
    AUX <- AUX[-1]

    # meltdown
    poeCols <- grep("poe-", COLS, value=TRUE)
    DATA <- melt(
      data        = AUX,
      id.vars     = c("lon","lat","depth"),
      measure.vars= poeCols,
      variable.name = "Sa",
      value.name    = "POE"
    )

    DATA[, POE := as.numeric(POE)]
    DATA[, Sa  := as.numeric(sub("^poe-", "", Sa))]

    # define AEP & TR
    DATA[, AEP := -log(1-POE)/ITo]
    DATA[, TR  := -ITo/log(1-POE)]

    DATA[, `:=`(Tn=Tn, p=p, ITo=ITo)]

    DT <- rbindlist(list(DT, DATA), use.names=TRUE)
  }

  # remove infinite or negative TR
  DT <- DT[is.finite(TR) & TR>0 & !is.na(TR)]

  # If no Tn=0 found, define a synthetic Tn=0 from min Tn≥0
  if (!foundPGA) {
    warning("No 'imt=PGA' found in openquake files => building synthetic Tn=0 from min Tn≥0.")
    groupCols <- c("lon","lat","depth","p","ITo")
    FAKES <- DT[Tn>=0, .SD[which.min(Tn)], by=groupCols]
    FAKES[, Tn:=0]
    DT <- rbindlist(list(DT, FAKES), use.names=TRUE)
  }

  return(DT[])
}

.extractQuantileFromHeader <- function(line) {
  if (grepl("kind='mean'", line)) return("mean")
  mt <- regexpr("kind='quantile-([0-9\\.]+)'", line)
  if (mt>0) {
    val <- regmatches(line, mt)
    qval <- sub("kind='quantile-", "", val)
    qval <- sub("'", "", qval)
    return(as.numeric(qval))
  }
  return(NA)
}
.extractInvestigationTime <- function(line) {
  mt <- regexpr("investigation_time=([0-9\\.]+)", line)
  if (mt>0) {
    val <- regmatches(line, mt)
    num <- sub("investigation_time=", "", val)
    return(as.numeric(num))
  }
  return(NA_real_)
}
.extractTnFromHeader <- function(line) {
  if (grepl("imt='PGA'", line)) return(0)
  if (grepl("imt='PGV'", line)) return(-1)
  mt <- regexpr("imt='SA\\(([0-9\\.]+)\\)'", line)
  if (mt>0) {
    val <- regmatches(line, mt)
    num <- sub("imt='SA\\(", "", val)
    num <- sub("\\)'", "", num)
    return(as.numeric(num))
  }
  return(NA_real_)
}
