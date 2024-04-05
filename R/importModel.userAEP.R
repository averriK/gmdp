
#' Import AEP values from user table in Excel
#'
#' @param filename Name of the file
#' @param path Path to the file
#'
#' @return AEPTable
#' @export importModel.userAEP
#'
#' @import data.table
#' @importFrom stringr str_remove
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_xlsx
#' @importFrom stats predict
#'
#' @examples
#'
#'
importModel.userAEP <- function(path = NULL,filename= "AEP.xlsx") {
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  OK <- !is.null(path) && dir.exists(path)
  stopifnot(OK)
  FILE <- file.path(path,filename)
  SHEETS <- readxl::excel_sheets(FILE) |> grep(pattern="p=",value = TRUE)
  SHEET <- SHEETS[1]
  AT <- NULL
  for(SHEET in SHEETS){
    DT <- readxl::read_xlsx(FILE,sheet=SHEET) |> data.table::as.data.table()
    if(!("Tn" %in% colnames(DT))){
      stop("Missing column 'Tn' in sheet '",SHEET,"'")
    }
    IVARS <- c("Tn")
    MVARS <- colnames(DT[, -c("Tn")])
    AUX <- data.table::melt(DT, id.vars = IVARS, measure.vars = MVARS)
    data.table::setnames(AUX, old = c("variable", "value"), new = c("Sa", "AEP"))
    po <- stringr::str_remove(SHEET,pattern = "p=")
    if(po!="mean"){po <- as.double(po)}
    AUX <- AUX[AEP>0]
    AUX[,p :=po]
    AUX[,Sa := as.numeric(levels(Sa))[Sa]]
    AUX[, IT := 50]
    AUX[, POE := (1 - exp(-IT * AEP))] # (exact)
    AUX[, TR := 1 / AEP]
    AT <- data.table::rbindlist(list(AUX,AT))
  }



  # LQ <- AT[p<0.5]$p |> unique()
  # HQ <- AT[p>0.5]$p |> unique()
  # Po <- c(LQ,0.5,HQ)
  # pID <- paste0("+",Po*100,"%")
  # cID <- paste0("±",sort(abs(1-Po)*100),"%")
  # QT <- data.table::data.table(p=Po,pID=pID,cID=cID,q=stats::qnorm(Po))
  # AT <- QT[AT, on="p"]
  #

  return(AT[])
}
