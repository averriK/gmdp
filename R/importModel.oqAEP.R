
#' Import Annual Exceedance Probabiliies (AEP) from openquake zip files.
#'
#' @param path character. Path to the folder containing the hazard and quantile curves
#' @param vref numeric. Reference Vs30 in m/s
#'
#' @return A data.table with the following columns:
#' @export importModel.oqAEP
#'
#' @examples
#' @import data.table
#' @importFrom stringr str_remove
#' @importFrom stringr str_split
#'
importModel.oqAEP <- function(path,vref) {
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  . <- NULL




  DT <- data.table::data.table()
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
  # IT <- ITo# gmdp.ini$IT

  for (k in seq_len(NR)) {
    FILE <- DATAPATH$name[k]
    #
    AUX <- data.table::fread(file = DATAPATH$datapath[k],  skip = 1, header = FALSE, blank.lines.skip = TRUE)
    COLS <- unlist(AUX[1]) |> as.vector()
    data.table::setnames(AUX,COLS)
    VARS <- setdiff(COLS,grep(COLS,pattern="poe-",value=TRUE))
    DATA <- melt(data=AUX[-1],id.vars = VARS,variable="Sa",value="POE")
    DATA <- DATA[,.(POE=as.numeric(POE),Sa=gsub(x=Sa,pattern="poe-",replacement="") |> as.numeric()),by=VARS]



    # parse HEADER ----
    p <- NULL
    HEADER <- data.table::fread(file = DATAPATH$datapath[k], nrows = 1, header = FALSE) |> str_split(",") |> unlist()
    HEADER <- HEADER[HEADER!="NA" & HEADER!="#"]

    AUX <- grep(HEADER,pattern="kind='quantile-",value=TRUE)
    if(length(AUX)>0){
      p <- str_remove(AUX,pattern="kind='quantile-") |> stringr::str_extract(pattern = "([0-9]+(\\.[0-9]+)?)") |> as.numeric()
    }
    if(any(grepl(HEADER,pattern="kind='mean'") )){p <-"mean"}
    if(is.null(p)){
      stop("Internal error: Unknown quantile \n %s",paste(HEADER,sep=""))
    }


    ITo <- NULL
    AUX <- grep(HEADER,pattern="investigation_time=",value=TRUE)
    if(length(AUX)>0){
      ITo <- str_remove(AUX,pattern="investigation_time=") |> stringr::str_extract(pattern = "([0-9]+(\\.[0-9]+)?)") |> as.numeric()
    }
    if(is.null(ITo)){
      stop("Internal error: Unknown investigation time:\n %s",paste(HEADER,sep=""))
    }


    Tn <- NULL
    AUX <- grep(HEADER,pattern="imt='SA\\(([0-9]+(\\.[0-9]+)?)\\)'",value=TRUE)
    if(length(AUX)>0){
      Tn <- str_remove(AUX,pattern="imt='SA") |> stringr::str_extract(pattern = "([0-9]+(\\.[0-9]+)?)") |> as.numeric()
    }
    if(any(grepl(HEADER,pattern="imt='PGA'") )){Tn <- 0}
    if(any(grepl(HEADER,pattern="imt='PGV'") )){Tn <- -1}
    if(is.null(Tn)){
      stop("Internal error: Unknown Tn:\n %s",paste(HEADER,sep=""))
    }

    DATA <- DATA[,.(lat,lon,depth,ITo=ITo,Tn = Tn, p = p, Sa = Sa, POE = POE,AEP = -log(1 - POE) / ITo,TR=-ITo/log(1-POE))]
    DT <- data.table::rbindlist(list(DATA, DT))
  }
  # browser()
  # ignore PGV
  # DT <- DT[TR != Inf & AEP != Inf & AEP != 0 & Tn!=-1]
  DT <- DT[ Tn!=-1]
  return(DT[])
}



