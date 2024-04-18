
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
buildGMDP <- function(path, ID="00000000", TRo = c(100, 200, 475,500, 1000, 2000, 2475,2500, 5000, 10000), ITo,Vs30o=760,Vs30_STEP = 25,configFile="gmdp.ini") {
  on.exit(expr = {
    # Clean Data
    if(exists("TEMP")){unlink(TEMP, recursive = TRUE)}
    rm(list = ls())
  }, add = TRUE)

  . <- NULL


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
  # Build UHSTable
  TRo <- seq(100,10000,by=25)
  UHSTable <- SaTRmodel[, .(IT = IT, POE = IT * 1 / TRo, TR = TRo, Sa = exp(a + b * log(TRo) + c * 1 / TRo), AEP = 1 / TRo), by = .(Tn, p)]
# no NAs
  # ********************************************************************* ----
  # Get PGA at Vref
  Tn0 <- UHSTable$Tn |> min()
  # browser()
  PGATable <- UHSTable[Tn == Tn0, .(PGAref = Sa), by = .(p, TR)] |> unique()
  COLS <- colnames(UHSTable)[colnames(UHSTable) %in% colnames(PGATable)]
  UHSTable <- PGATable[UHSTable, on = COLS]

  # ********************************************************************* ----
  # Get AF*Sa for AEP ordinates
  Vref <- gmdp.ini$vs30
  AFTRmodel <- data.table()
  if(Vref %in% c(760,3000)){
    message(sprintf("> Fit Site Response model (Stewart2017) for ASCE site classes..."))
    S1 <- seq(SIDtoVs30("E"), SIDtoVs30("BC"), by = Vs30_STEP)
    S2 <- sapply(c("BC", "C", "CD", "D", "DE", "E"), SIDtoVs30) |> unname()
    Vs30_SET <- c(S1,S2,Vref) |> unique() |> sort()
    for (Vs in Vs30_SET) {
      message(sprintf("> Building AEP Site Response model for Vs30 %4.1f m/s...", Vs))
      AUX <- UHSTable[, fitModel.AFTR(.x=.SD,p=p,Tn=Tn, Vs30 = Vs, Vref = Vref), by = .(p,Tn)]
      AFTRmodel <- data.table::rbindlist(list(AFTRmodel, AUX), use.names = TRUE)
    }
  }

  # ********************************************************************* ----
  # update UHSTable
  message(sprintf("> Update UHSTable ..."))
  COLS <- colnames(AFTRmodel)[colnames(AFTRmodel) %in% colnames(UHSTable)]
  UHSTable <- AFTRmodel[UHSTable, on = COLS]

  # ********************************************************************* ----
  # Update AEPTable

  browser()
  UHSTable[,`:=`(Sa=Sa*AF,PGA=PGAref*AF,SN=ID,PGA_Unit="g",Sa_Unit="g",Vs30_Unit="m/s",Tn_Unit="s")]

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
rm(AUX)
  # ********************************************************************* ----
  return(list(AEPTable = AEPTable, UHSTable = UHSTable, AFTRmodel = AFTRmodel, SaTRmodel = SaTRmodel, DnTRmodel = DnTRmodel, RMwTable = RMwTable, PGATable = PGATable, KmaxTable = KmaxTable, job.ini = gmdp.ini))
}
