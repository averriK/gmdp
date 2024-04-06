
#' Import disaggregation hazard (R-MW-p) from openquake
#'
#' @param path Path to the folder containing the hazard and quantile curves
#' @param gmdp.ini List with the following elements:
#'
#' @return A data.table with the following columns:
#' @export importModel.oqRMw
#'
#' @import data.table
#' @importFrom stringr str_split
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#'
#' @examples
#'
importModel.oqRMw <- function(path, gmdp.ini = NULL) {
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  OK <- !is.null(path)
  stopifnot(OK)
  IT <- gmdp.ini$IT # Investigation Time

  DHT <- data.table::data.table()
  PATH <- path
  LIST <- list.files(path = PATH)
  # FILES <- LIST[grepl(LIST,pattern="rlz")]
  FILES <- LIST[grepl(LIST, pattern = "Mag_Dist")]
  FILES <- FILES[!grepl(FILES, pattern = "TRT")]
  DATAPATH <-  data.table::data.table(name = FILES, datapath = file.path(PATH, FILES))

  # if(!is.null(uploadTable)){
  #   LIST <- uploadTable
  #   FILES <- LIST[grepl(name,pattern="rlz")]
  #   DATAPATH <-  data.table::data.table(name=FILES$name,datapath=FILES$datapath)
  # }
  NR <- nrow(DATAPATH)
  if (NR == 0) {
    return(NULL)
  }
  AUX <- DATAPATH[grepl(name, pattern = "Mag_Dist-mean")]


  if (nrow(AUX) > 0) {
    # openquake V18 reports two files for dissagg. Keep just one
    DATAPATH <- AUX
    NR <- nrow(DATAPATH)
    # ******** THIS SOLUTION DOES NOT WORK FOR MULTI-SITE MODELS
  }

  sid <- double(length = NR)
  for (k in seq_len(NR)) {
    FILE <- DATAPATH$name[k]
    HEADER <- data.table::fread(file = DATAPATH$datapath[k], nrows = 1, header = FALSE, blank.lines.skip = TRUE) |> stringr::str_split(pattern = ",")
    HEADER <- HEADER[[length(HEADER)]]

    # lat <- HEADER[grep(HEADER,pattern=" lat=")] |> stringr::str_remove(pattern = " lat=") |> as.numeric()
    # lon <- HEADER[grep(HEADER,pattern=" lon=")] |> stringr::str_remove(pattern = " lon=") |> as.numeric()
    AUX <- k # (str_split(FILE,pattern = "Mag_Dist-")[[1]][2] |> stringr::str_split(pattern = "_"))[[1]][1]

    SN <- paste0("V", as.integer(AUX) + 2)
    DT <- data.table::fread(file = DATAPATH$datapath[k], skip = 1, header = TRUE, blank.lines.skip = TRUE)
    data.table::setnames(DT, old = c("mag", "dist", "poe"), new = c("Mw", "R", "POE"))
    DT[, IT := IT]
    DT[, AEP := POE / IT]
    DT[, TR := 1 / AEP]
    OK <- FALSE
    COLS <- colnames(DT)
    if (any(COLS == "iml")) {
      # openQuake Version 18. Remove column
      DT[, iml := NULL]
    }
    COLS <- colnames(DT)
    OK <- any(COLS == "imt")
    stopifnot(OK)
    DT[imt == "PGA", imt := "Sa(0.0)"]
    DT[, Tn := stringr::str_extract(string = imt, pattern = "(?<=\\().*(?=\\))") |>  as.numeric()]
    DT[, SN := SN]
    DT[, imt := NULL]
    COLS <- colnames(DT)
    OK <- FALSE
    N1 <- length(grep(COLS, pattern = "rlz"))
    N2 <- length(grep(COLS, pattern = "mean"))
    if (N1 == 0 & N2 == 1) {
      # openQuake Verion 18. Right file
      OK <- TRUE
      oCOL <- colnames(DT) |> grep(pattern = "mean", value = TRUE)
    }


    if (N1 == 1 & N2 == 0) {
      # openQuake Version 16. ok
      OK <- TRUE
      oCOL <- colnames(DT) |> grep(pattern = "rlz", value = TRUE)
    }

    if (N1 > 1 & N2 == 0) {
      # openQuake Version 18. Wrong file. Internal error
      OK <- FALSE
    }
    stopifnot(OK)
    nCOL <- "p"
    data.table::setnames(DT, old = oCOL, new = nCOL)

    # DT <- DT[,.(Mw,R,p,rlz,Tn,AEP,TR,LAT,LON,SN)]
    DHT <- data.table::rbindlist(list(DHT, DT))
  }
  return(DHT)
}
