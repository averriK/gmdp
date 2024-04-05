
#' Title
#'
#' @param path Path to GMDP
#' @param ID Output ID
#' @param TRo Return Periods
#' @param Vs30_STEP Vs30 Step
#' @param configFile GMDP config file
#' @param Vs30o Vs30 in m/s
#' @param ITo Invesigation Time. ITo=50 yr
#'
#' @return list
#' @export buildGMDP
#' @import data.table
#'
#' @examples
#'
buildGMDP <- function(path, ID="00000000", TRo = c(100, 200, 475,500, 1000, 2000, 2475,2500, 5000, 10000), ITo=50,Vs30o=760,Vs30_STEP = 25,configFile="gmdp.ini") {
  on.exit(expr = {
    # Clean Data
    if(exists("TEMP")){unlink(TEMP, recursive = TRUE)}
    rm(list = ls())
  }, add = TRUE)



  # ********************************************************************* ----
  # load job.INI
  message(sprintf("> Reading GMDP config file %s ...",configFile))
  FILE <- file.path(path, configFile)
  gmdp.ini <- list()
  gmdp.ini[["outputID"]] <- ID
  if(file.exists(FILE)){

    message(sprintf("> Reading GMDP config file %s...",configFile))

    INI <- ini::read.ini(filepath = FILE)
    gmdp.ini[["projectID"]] <- INI$gmdp$project_ID # ARM2J1G
    gmdp.ini[["gmpeID"]] <- INI$gmdp$gmpe_ID # SCC
    gmdp.ini[["sourceID"]] <- INI$gmdp$source_ID # ZAF
    gmdp.ini[["logicTreeID"]] <- INI$gmdp$logic_tree_ID # 0
    gmdp.ini[["vs30"]] <- INI$gmdp$vs30 |> as.double()
    gmdp.ini[["engine"]] <- INI$gmdp$engine
    gmdp.ini[["IT"]] <- INI$gmdp$IT |> as.double()
  }

  if(!file.exists(FILE)){

    message(sprintf("> GMDP config file not found. Assuming defaul values..."))

    gmdp.ini[["projectID"]] <- substr(ID, 1, nchar(ID)-1)

    gmdp.ini[["gmpeID"]] <- NA # SCC
    gmdp.ini[["sourceID"]] <- NA # ZAF
    gmdp.ini[["logicTreeID"]] <- NA # 0

    gmdp.ini[["vs30"]] <- Vs30o
    gmdp.ini[["engine"]] <- "openquake"
    gmdp.ini[["IT"]] <- ITo # Caution: South American models use IT=1 year. Alway check this value
  }



  # ********************************************************************* ----
  # Build AEPTable

  message(sprintf("> Build AEP Table..."))
  AEPTable <- NULL
  if(gmdp.ini$engine=="openquake"){
    message(sprintf("> Unzip OQ data ..."))
    TEMP <- tempdir()
    if (dir.exists(TEMP)) {
      unlink(TEMP, recursive = TRUE)
    }

    FILES <- list.files(path, pattern = "*.zip", full.names = TRUE)
    for (ZIPFILE in FILES) {
      utils::unzip(zipfile = ZIPFILE, junkpaths = TRUE, exdir = TEMP)
    }

    message(sprintf("> Import AEP data from openquake..."))
    AEPTable <- importModel.oqAEP(path = TEMP,gmdp.ini)

  }

  if(gmdp.ini$engine=="user"){
    message(sprintf("> Unzip USER data ..."))
    message(sprintf("> Import AEP data from openquake..."))
    AEPTable <- importModel.userAEP(path,filename= "AEP.xlsx")
  }
  if(is.null(AEPTable)){
    gmdp.ini[["IT"]] <- AEPTable$IT |> unique()# gmdp.ini$IT
    stopifnot(length(gmdp.ini$IT)==1)

  } else {
    message(sprintf("> AEP data not available."))
  }

  # ********************************************************************* ----
  # Disaggregation

  RMwTable <- NULL
  message(sprintf("> Building Disagregation Hazard Table..."))
  if(gmdp.ini$engine=="openquake"){
    message(sprintf("> Import Disaggregation data from openquake..."))
    RMwTable <- importModel.oqRMw(path = TEMP, gmdp.ini)
  }


  # Tag Site Conditions
  if (!is.null(RMwTable)) {
    RMwTable[, SID := Vs30toSID(gmdp.ini$vs30)]
    RMwTable[, Vs30 := gmdp.ini$vs30]
    RMwTable[, SM := gmdp.ini$engine]
    RMwTable[, SN := ID]
  } else {
    message(sprintf("> Disaggregation data not available."))
  }



  # ********************************************************************* ----
  # Build SaTR model

  message(sprintf("> Fit AEP  modelfrom %s...", path))
  SaTRmodel <- AEPTable[, fitModel.SaTR( x = .SD, TRmin = 100, TRmax = 10000), by = c("Tn", "p")][, .(Tn, p, a, b, c, sdLnA)]


  # ********************************************************************* ----
  # Get UHS ordinates
  message(sprintf("> Set UHS specral ordinates..."))

  IT <- gmdp.ini$IT
  # AUX <- SaTRmodel[p!="mean", .(IT = IT, POE = IT * 1 / TRo, TR = TRo, Sa = exp(a + b * log(TRo) + c * 1 / TRo), AEP = 1 / TRo, q = stats::qnorm(p)), by = .(Tn, p)]

  AUX <- SaTRmodel[, .(IT = IT, POE = IT * 1 / TRo, TR = TRo, Sa = exp(a + b * log(TRo) + c * 1 / TRo), AEP = 1 / TRo), by = .(Tn, p)]

  AEPTable <- data.table::rbindlist(list(AEPTable, AUX), use.names = TRUE)
  # ********************************************************************* ----
  # Get PGA
  Tn_PGA <- AEPTable$Tn |> min()
  PGATable <- AEPTable[Tn == Tn_PGA, .(PGA = Sa), by = .(p, TR)] |> unique()

  COLS <- colnames(AEPTable)[colnames(AEPTable) %in% colnames(PGATable)]
  AEPTable <- PGATable[AEPTable, on = COLS]




  # ********************************************************************* ----
  # Set AF=1
  message(sprintf("> Initialize site conditions for Vref..."))

  AFTRmodel <- AEPTable[, fitModel.AFTR( x = .SD, Vs30 = gmdp.ini$vs30, Vref = gmdp.ini$vs30, Vl = 200, Vu = 2000), by = .(Tn, p, TR), .SDcols = colnames(AEPTable)] # c("PGA","Tn")]



  # ********************************************************************* ----
  # Get AF*Sa for AEP ordinates
  if(gmdp.ini$vs30 %in% c(760,3000)){
    message(sprintf("> Fit Site Response model (Stewart2017) for ASCE site classes..."))
    for (SID in c("A", "B", "BC", "C", "CD", "D", "DE", "E")) {
      message(sprintf("> Building AEP Site Response model for site class %s...", SID))
      AUX <- AEPTable[, fitModel.AFTR( x = .SD, Vs30 = SIDtoVs30(SID), Vref = gmdp.ini$vs30, Vl = 200, Vu = 2000), by = .(Tn, p, TR), .SDcols = colnames(AEPTable)]
      AFTRmodel <- data.table::rbindlist(list(AFTRmodel, AUX), use.names = TRUE)
    }
  }



  # ********************************************************************* ----
  # update AEPTable
  message(sprintf("> Update AEPTable ..."))
  COLS <- colnames(AFTRmodel)[colnames(AFTRmodel) %in% colnames(AEPTable)]
  AEPTable <- AEPTable[AFTRmodel[, .(p, Tn, TR, AF, sdLnAF, Vs30, Vref, SID, SM)], on = COLS]
  AEPTable[, AUX := Sa * AF]
  AEPTable[, Sa := AUX]
  AEPTable[, AUX := NULL]
  AEPTable[, Sa_Unit := "g"]
  AEPTable[, PGA_Unit := "g"]
  AEPTable[, Vs30_Unit := "m/s"]
  AEPTable[, Tn_Unit := "s"]
  AEPTable[, SN := ID]

  # ********************************************************************* ----
  # Get AF*Sa for UHS ordinates ----
  UHSTable <- AEPTable[TR %in% TRo] |> unique(by = c("Tn", "p", "TR", "SID", "Vs30", "SM"))
  if(gmdp.ini$vs30 %in% c(760,3000)){
    message(sprintf("> Refine Site Response model for UHS ..."))
    S1 <- seq(SIDtoVs30("E"), SIDtoVs30("BC"), by = Vs30_STEP)
    S2 <- sapply(c("BC", "C", "CD", "D", "DE", "E"), SIDtoVs30) |> unname()

    Vs30_SET <- S1[!(S1 %in% S2)]
    for (Vs in Vs30_SET) {
      message(sprintf("> Building UHS Site Response model for Vs30 %4.1f m/s...", Vs))
      AUX <- UHSTable[, fitModel.AFTR(x = .SD, Vs30 = Vs, Vref = gmdp.ini$vs30, Vl = 200, Vu = 2000), by = .(Tn, p, TR), .SDcols = colnames(UHSTable)]
      AFTRmodel <- data.table::rbindlist(list(AFTRmodel, AUX), use.names = TRUE)
    }
  }



  AFTRmodel <- unique(AFTRmodel, by = c("Tn", "p", "TR", "Vs30", "Vref", "SID", "SM"))
  # ********************************************************************* ----
  # PGV, Arias Intensity






  # ********************************************************************* ----
  # Newmark Displacements ----

  message(sprintf("> Building Newmark Displacements model"))
  DnTRmodel <- UHSTable[, fitModel.DnTR(x = .SD, Mw = 6.5, xD = 1.3, kymin = 0.005, kymax = 0.5, n = 30), by = .(Tn, TR, Vs30, p), .SDcols = colnames(UHSTable)]

  # ********************************************************************* ----

  DnTRmodel[, SN := ID]
  message(sprintf("> Update UHSTable ..."))
  COLS <- colnames(DnTRmodel)[colnames(DnTRmodel) %in% colnames(UHSTable)]
  UHSTable <- UHSTable[DnTRmodel[, .(SN, p, Tn, Ts, TR, Dn, sdLnD, ky, Vs30, a, b, e)], on = COLS]
  UHSTable[, Dn_Unit := "cm"]
  UHSTable[, Ts_Unit := "s"]

  # ********************************************************************* ----
  # Pseudo-static coefficient ----
  message(sprintf("> Building pseudo-static coefficient model"))
  AUX <- UHSTable[, .(Tn, TR, p, Ts, Vs30, Vref, a, b, e, PGA)] |> unique()
  KmaxTable <- AUX[, fitModel.KmaxTR( x = .SD, n = 20), by = .(Tn, TR, p, Vs30, Vref), .SDcols = colnames(AUX)]

  # ********************************************************************* ----
  DT <- list(AEPTable = AEPTable, UHSTable = UHSTable, AFTRmodel = AFTRmodel, SaTRmodel = SaTRmodel, DnTRmodel = DnTRmodel, RMwTable = RMwTable, PGATable = PGATable, KmaxTable = KmaxTable, job.ini = gmdp.ini)
  return(DT)
}
