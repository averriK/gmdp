
#' Title
#'
#' @param path character
#' @param IDo character Output IDo
#' @param Vs30 numeric Vs30 Step
#' @param Vref Vs30 in m/s
#' @param engine character c("openquake","user")
#' @param siteResponse boolean
#'
#' @return list
#' @export
#' @import data.table
#'
#' @examples
#'
buildGMDP <- function(path, IDo="00000000",engine="openquake",Vs30=NULL,Vref) {
  on.exit(expr = {rm(list = ls())}, add = TRUE)

  . <- NULL

  # ********************************************************************* ----
  # Build AEPTable
  # THIS VERSION DO NOT REQUIRES THE INVESTIGATION TIME AS INPUT.
  # RUNNING AS A BATCH PROCESS, ITo CAN BE OBTAINED FROM HEADERS

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
    AEPTable <- importModel.oqAEP(path = TEMP)

  }

  if(engine=="user"){
    message(sprintf("> Unzip USER data ..."))
    message(sprintf("> Import AEP data from openquake..."))
    AEPTable <- importModel.userAEP(path,filename= "AEP.xlsx")
  }
  ITo <- unique(AEPTable$ITo)[1]
  # ********************************************************************* ----
  # Disaggregation

  RMwTable <- NULL
  message(sprintf("> Building Disagregation Hazard Table..."))
  if(engine=="openquake"){
    message(sprintf("> Import Disaggregation data from openquake..."))
    RMwTable <- importModel.oqRMw(path = TEMP, ITo=ITo)
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

#



  # ********************************************************************* ----
  # Build SaTR model
  message(sprintf("> Fit AEP  modelfrom %s...", path))
  Tn_PGA <- AEPTable[Tn>=0]$Tn |> min()
  # browser()
  Tn_PGV <- NULL
  if(nrow(AEPTable[Tn<0])>0){
    # Exclude PGV case
    Tn_PGV <- AEPTable[Tn<0]$Tn |> min()
    SaTRmodel <- AEPTable[Tn!=Tn_PGV, fitModel.Sa.TR( x = .SD, TRmin = 100, TRmax = 10000), by = c("lat","lon","depth","Tn", "p")][, .(lat,lon,depth,Tn, p, a, b, c, sdLnA)]

    # Include PGV case. Assume same AF than PGA case
    SaTRmodel <- rbind(SaTRmodel,SaTRmodel[Tn==Tn_PGA][,Tn:=Tn_PGV])
  } else {
    SaTRmodel <- AEPTable[, fitModel.Sa.TR( x = .SD, TRmin = 100, TRmax = 10000), by = c("lat","lon","depth","Tn", "p")][, .(lat,lon,depth,Tn, p, a, b, c, sdLnA)]
  }







  # ********************************************************************* ----
  # Get UHS ordinates
  message(sprintf("> Set UHS spectral ordinates..."))
 # Build UHSTable
  S1 <- seq(100,10000,by=25)
  S2 <- c(475,500,975,1000,2000,2475,2500,5000,10000)
  TRo <- c(S1,S2) |> unique() |> sort()

  UHSTable <- SaTRmodel[, .(IT = ITo, POE = ITo * 1 / TRo, TR = TRo, Sa = exp(a + b * log(TRo) + c * 1 / TRo), AEP = 1 / TRo), by = .(lat,lon,depth,Tn, p)]
# no NAs
  # ********************************************************************* ----
  # Get PGA at Vref
  if(!is.null(Tn_PGV)){
    AUX1 <- UHSTable[Tn == Tn_PGA,.(PGA=Sa,Vs30=Vref),by=.(lat,lon,depth,p,TR)]
    AUX2 <- UHSTable[Tn == Tn_PGV,.(PGV=Sa,Vs30=Vref),by=.(lat,lon,depth,p,TR)]
    COLS <- colnames(AUX1)[colnames(AUX1) %in% colnames(AUX2)]
    PeakTable <- AUX1[AUX2,on=COLS]
  } else {
    PeakTable <- UHSTable[Tn == Tn_PGA,.(PGA=Sa,PGV=NA,Vs30=Vref),by=.(lat,lon,depth,p,TR)]
  }


  COLS <- colnames(UHSTable)[colnames(UHSTable) %in% colnames(PeakTable)]
  UHSTable <- PeakTable[UHSTable, on = COLS]

  if(!is.null(Tn_PGV)){
    # Remove Tn==-1
    UHSTable <- UHSTable[Tn!=Tn_PGV]
  }

  # ********************************************************************* ----
  # Get AF*Sa for AEP ordinates

  AFTRmodel <- data.table()
  if(Vref %in% c(760,3000) &  !is.null(Vs30)){

    message(sprintf("> Fit Site Response model (Stewart2017) for ASCE site classes..."))

    for (Vs in Vs30) {
      message(sprintf("> Building AEP Site Response model for Vs30 %4.1f m/s...", Vs))
      # AF estimated only as mean value. Ignoring quantiles from Sa(Tn). Setting p=0.50
      # Each (p, Tn) set results in a data.table .x with TR rows

      AUX <- UHSTable[, fitModel.AF.TR(.x=.SD,q=0.50,Tn=Tn, Vs30 = Vs,Vref=Vref), by = .(lat,lon,depth,p,Tn)]
      AFTRmodel <- data.table::rbindlist(list(AFTRmodel, AUX), use.names = TRUE)
    }
    # update UHSTable
    message(sprintf("> Update UHSTable ..."))
    # browser()
    AUX <- AFTRmodel[,.(lat,lon,depth,p,TR,Tn,AF,SID,SM)]
    COLS <- colnames(AUX)[colnames(AUX) %in% colnames(UHSTable)]
    UHSTable <- AFTRmodel[UHSTable, on = COLS][,.(lat,lon,depth,p,TR,Tn,IT,POE,AEP,Sa=Sa*AF,PGA=PGA*AF,PGV=PGV*AF,ID=IDo,Vref=Vref,Vs30=Vs,AF,SID,SM,PGA_Unit="g",Sa_Unit="g",TR_Unit="yr",Vs30_Unit="m/s",Vref_Unit="m/s")]
    AFTRmodel <- unique(AFTRmodel, by = c("lat","lon","depth","Tn", "p", "TR", "Vs30", "Vref", "SID", "SM"))

  } else {

    UHSTable <- UHSTable[,.(lat,lon,depth,p,TR,Tn,IT,POE,AEP,Sa,PGA,PGV,ID=IDo,Vref=Vref,Vs30=Vref,AF=1,SID="",SM="openquake",PGA_Unit="g",Sa_Unit="g",TR_Unit="yr",Vs30_Unit="m/s",Vref_Unit="m/s")]

  }

  # ********************************************************************* ----
  return(list(AEPTable = AEPTable, UHSTable = UHSTable, AFTRmodel = AFTRmodel, SaTRmodel = SaTRmodel, RMwTable = RMwTable, PeakTable = PeakTable))
}
