
#' Title
#'
#' @param path Path to GMDP
#' @param IDo Output IDo
#' @param Vs30_STEP Vs30 Step
#' @param Vref Vs30 in m/s
#' @param ITo Invesigation Time. ITo=50 yr
#' @param engine c("openquake","user")
#'
#' @return list
#' @export buildGMDP
#' @import data.table
#'
#' @examples
#'
buildGMDP <- function(path, IDo="00000000", ITo,Vref=760,engine="openquake",Vs30_STEP = 25) {
  on.exit(expr = {rm(list = ls())}, add = TRUE)

  . <- NULL

  # ********************************************************************* ----
  # Build AEPTable

  message(sprintf("> Build AEP Table..."))
  AEPTable <- NULL
  if(engine=="openquake"){
    message(sprintf("> Unzip OQ data ..."))
    TEMP <- tempdir()
    if (dir.exists(TEMP)) {
      unlink(TEMP, recursive = TRUE)
      TEMP <- tempdir()
    }

    FILES <- list.files(path, pattern = "*.zip", full.names = TRUE)
    for (ZIPFILE in FILES) {
      utils::unzip(zipfile = ZIPFILE, junkpaths = TRUE, exdir = TEMP)
    }

    message(sprintf("> Import AEP data from openquake..."))
    AEPTable <- importModel.oqAEP(path = TEMP,ITo)

  }

  if(engine=="user"){
    message(sprintf("> Unzip USER data ..."))
    message(sprintf("> Import AEP data from openquake..."))
    AEPTable <- importModel.userAEP(path,filename= "AEP.xlsx")
  }
  # if(is.null(AEPTable)){
  #   gmdp.ini[["IT"]] <- AEPTable$IT |> unique()# gmdp.ini$IT
  #   stopifnot(length(gmdp.ini$IT)==1)
  #
  # } else {
  #   message(sprintf("> AEP data not available."))
  # }

  # ********************************************************************* ----
  # Disaggregation

  RMwTable <- NULL
  message(sprintf("> Building Disagregation Hazard Table..."))
  if(engine=="openquake"){
    message(sprintf("> Import Disaggregation data from openquake..."))
    RMwTable <- importModel.oqRMw(path = TEMP, ITo)
  }


  # Tag Site Conditions
  if (!is.null(RMwTable)) {
    RMwTable[, SID := Vs30toSID(Vref)]
    RMwTable[, Vs30 := Vref]
    RMwTable[, SM := engine]
    RMwTable[, ID := IDo]
    RMwTable[, IT := ITo]
  } else {
    message(sprintf("> Disaggregation data not available."))
  }



  # ********************************************************************* ----
  # Build SaTR model

  message(sprintf("> Fit AEP  modelfrom %s...", path))
  SaTRmodel <- AEPTable[, fitModel.Sa.TR( x = .SD, TRmin = 100, TRmax = 10000), by = c("Tn", "p")][, .(Tn, p, a, b, c, sdLnA)]


  # ********************************************************************* ----
  # Get UHS ordinates
  message(sprintf("> Set UHS specral ordinates..."))
  # AUX <- SaTRmodel[, .(IT = ITo, POE = IT * 1 / TRo, TR = TRo, Sa = exp(a + b * log(TRo) + c * 1 / TRo), AEP = 1 / TRo), by = .(Tn, p)]
  #
  # AEPTable <- data.table::rbindlist(list(AEPTable, AUX), use.names = TRUE)

  # ********************************************************************* ----
  # Build UHSTable
  S1 <- seq(100,10000,by=25)
  S2 <- c(500,1000,2000,2500,5000,10000)
  TRo <- c(S1,S2) |> unique() |> sort()
  UHSTable <- SaTRmodel[, .(IT = ITo, POE = ITo * 1 / TRo, TR = TRo, Sa = exp(a + b * log(TRo) + c * 1 / TRo), AEP = 1 / TRo), by = .(Tn, p)]
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
  # Vref <- gmdp.ini$vs30
  AFTRmodel <- data.table()
  if(Vref %in% c(760,3000)){
    message(sprintf("> Fit Site Response model (Stewart2017) for ASCE site classes..."))
    S1 <- seq(SIDtoVs30("E"), SIDtoVs30("BC"), by = Vs30_STEP)
    S2 <- sapply(c("BC", "C", "CD", "D", "DE", "E"), SIDtoVs30) |> unname()
    Vs30_SET <- c(S1,S2,Vref) |> unique() |> sort()
    for (Vs in Vs30_SET) {
      message(sprintf("> Building AEP Site Response model for Vs30 %4.1f m/s...", Vs))
      # AF estimated only as mean value. Ignoring quantiles from Sa(Tn). Setting p=0.50
      # Each (p, Tn) set results in a data.table .x with TR rows
      AUX <- UHSTable[, fitModel.AF.TR(.x=.SD,q=0.50,Tn=Tn, Vs30 = Vs, Vref = Vref), by = .(p,Tn)]
      AFTRmodel <- data.table::rbindlist(list(AFTRmodel, AUX), use.names = TRUE)
    }
  }

  # ********************************************************************* ----
  # update UHSTable
  message(sprintf("> Update UHSTable ..."))
  # browser()
  COLS <- colnames(AFTRmodel)[colnames(AFTRmodel) %in% colnames(UHSTable)]
  UHSTable <- AFTRmodel[UHSTable, on = COLS]

  # ********************************************************************* ----
  # Update AEPTable
  UHSTable[,`:=`(Sa=Sa*AF,PGA=PGAref*AF,ID=IDo,PGA_Unit="g",Sa_Unit="g",TR_Unit="yr",Vs30_Unit="m/s",Vref_Unit="m/s")]

  AFTRmodel <- unique(AFTRmodel, by = c("Tn", "p", "TR", "Vs30", "Vref", "SID", "SM"))
  # ********************************************************************* ----
  # PGV, Arias Intensity






  # ********************************************************************* ----
  return(list(AEPTable = AEPTable, UHSTable = UHSTable, AFTRmodel = AFTRmodel, SaTRmodel = SaTRmodel, RMwTable = RMwTable, PGATable = PGATable))
}
