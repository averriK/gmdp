
#' Import Annual Exceedance Probabiliies (AEP) from openquake zip files.
#'
#' @param path character. Path to the folder containing the hazard and quantile curves
#' @param ITo numeric
#'
#' @return A data.table with the following columns:
#' @export importModel.oqAEP
#'
#' @examples
#' @import data.table
#' @importFrom stringr str_remove
#' @importFrom stringr str_split
#'
importModel.oqAEP <- function(path,ITo) {
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  . <- NULL




  AT <- data.table::data.table()
  PATH <- path
  FILES <- list.files(path = PATH, pattern = "quantile_curve")
  FILES <- c(FILES, list.files(path = PATH, pattern = "hazard_curve"))
  DATAPATH <- data.table::data.table(name = FILES, datapath = file.path(PATH, FILES))
  NR <- nrow(DATAPATH)
  if (NR == 0) {
    stop("No files found")
    return(NULL)
  }

  # Read job.ini (if exists)
  IT <- ITo# gmdp.ini$IT

  for (k in seq_len(NR)) {
    FILE <- DATAPATH$name[k]
    HEADER <- data.table::fread(file = DATAPATH$datapath[k], nrows = 1, header = FALSE)
    HEADER <- HEADER[[ncol(HEADER)]]

    DT <- data.table::fread(file = DATAPATH$datapath[k], skip = 1, header = FALSE, blank.lines.skip = TRUE)
    DT[1, X := "SN"]
    DT[2:.N, X := paste0("V", .I + 1)]
    if (all(DT[1, .(V1, V2, V3)] %in% c("lon", "lat", "depth"))) {
      # ST <- DT[,list(V1,V2,V3,X)]
      DT <- DT[, -c("V1", "V2", "V3", "X")] |>
        t() |>
        data.table::as.data.table()
    } else if (all(DT[1, .(V1, V2)] %in% c("lon", "lat"))) {
      # ST <- DT[,list(V1,V2,X)]
      DT <- DT[, -c("V1", "V2", "X")] |>
        t() |>
        data.table::as.data.table()
    } else {
      stop()
    }

    # data.table::setnames(DT, old = "V1", new = "AUX")
    DT[, V1 := stringr::str_remove(string = V1, pattern = "poe-") |> as.numeric()]
    DT[, V2 := as.numeric(V2)]
    data.table::setnames(DT, old = "V1", new = "Sa")
    data.table::setnames(DT, old = "V2", new = "POE")

    # Check quantiles ----
    p <- NULL

    if(is.null(p)){
      AUX <- regmatches(HEADER, regexpr(pattern = "'quantile-([0-9]+(\\.[0-9]+)?)'", HEADER)) |> stringr::str_extract(pattern = "([0-9]+(\\.[0-9]+)?)")
      if ( length(AUX) >0) {
        p <- as.numeric(AUX)
      }
    }
    if (is.null(p)) {
      AUX <- regmatches(HEADER, regexpr(pattern = "'mean'", HEADER))
      if ( length(AUX) >0) {
        p <-"mean"
      }
    }
    if(is.null(p)){
      stop("Internal error: Unknown header:\n %s",HEADER)
    }
    DT[, p := p]
    # Check Period
    Tn <- NULL
    AUX <- regmatches(HEADER, regexpr(pattern = "'SA\\(([0-9]+(\\.[0-9]+)?)\\)'", HEADER)) |> stringr::str_extract(pattern = "([0-9]+(\\.[0-9]+)?)")
    if (length(AUX) >0) {
      Tn <- as.numeric(AUX)
    }

    AUX <- regmatches(HEADER, regexpr(pattern = "'PGA'", HEADER))
    if ( length(AUX) >0) {
      Tn <- 0
    }
    if(is.null(Tn)){
      stop("Internal error: Unknown header:\n %s",HEADER)
    }
    DT[,Tn:=Tn]

    DT[, IT := IT]
    DT[, AEP := -log(1 - POE) / IT] # DT[,AEP:=POE/IT]
    DT[, TR := 1 / AEP]
    AT <- data.table::rbindlist(list(AT, DT))
  }
# SET QUANTILES - OBSOLETO
  # LQ <- AT[p!="mean"][p < 0.5]$p |> unique() |> as.numeric()
  # HQ <- AT[p!="mean"][p > 0.5]$p |> unique() |> as.numeric()
  # Po <- c(LQ, 0.5, HQ)
  #
  # pID <- paste0("+",Po*100,"%")
  # cID <- paste0("±",sort(abs(1-Po)*100),"%")
  # QT <- data.table::data.table(p=Po,pID=pID,cID=cID,q=stats::qnorm(Po))
  # AT <- QT[AT, on = "p"]
  #

  AT <- AT[TR != Inf & AEP != Inf & AEP != 0]
   return(AT[])
}



