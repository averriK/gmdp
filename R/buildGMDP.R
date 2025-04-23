
#' Title
#'
#' @param path character
#' @param IDo character Output IDo
#' @param vs30 numeric Vs30 Step
#' @param vref Vs30 in m/s
#' @param engine character c("openquake","user")
#' @param quantile_AF character c("mean",0.16,0.50,0.84)
#'
#' @return list
#' @export
#' @import data.table
#' @import dsra

#'
#' @examples
#'
buildGMDP <- function(path, IDo="00000000",engine="openquake",vs30=NULL,vref,quantile_AF="mean") {
on.exit(expr = {rm(list = ls())}, add = TRUE)
  . <- NULL
  AF_q_TARGET <- quantile_AF
  # *********************************************************************
  # Build AEPTable ----

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
    AEPTable <- importModel.oqAEP(path = TEMP,vref=vref)

  }

  if(engine=="user"){
    message(sprintf("> Unzip USER data ..."))
    message(sprintf("> Import AEP data from openquake..."))
    AEPTable <- importModel.userAEP(path,filename= "AEP.xlsx")
  }
  ITo <- unique(AEPTable$ITo)[1]
  # *********************************************************************
  # Build Disaggregation Table ----

  RMwTable <- NULL
  message(sprintf("> Building Disagregation Hazard Table..."))
  if(engine=="openquake"){
    message(sprintf("> Import Disaggregation data from openquake..."))
    RMwTable <- importModel.oqRMw(path = TEMP, ITo=ITo,vref=vref)
  }


  # Tag Site Conditions
  if (!is.null(RMwTable)) {
    RMwTable[, SID := Vs30toSID(vref)]
    RMwTable[, Vs30 := vref]
    RMwTable[, SM := engine]
    RMwTable[, ID := IDo]
    RMwTable[, IT := ITo]
  } else {
    message(sprintf("> Disaggregation data not available."))
  }


  # *********************************************************************
  # Build SaTR model ----
  message(sprintf("> Fit AEP  model from %s...", path))
  Tn_PGA <- AEPTable[Tn>=0]$Tn |> min()
  SaTRmodel <- AEPTable[, fitModel.Sa.TR( x = .SD, TRmin = 100, TRmax = 10000), by = c("lat","lon","depth","Tn", "p")][, .(lat,lon,depth,Tn, p, a, b, c, sdLnA,Sa_Unit="g")]


  # *********************************************************************
  # Get UHS ordinates ----
  message(sprintf("> Set UHS spectral ordinates..."))
 # Build UHSTable
  S1 <- seq(100,10000,by=25)
  S2 <- c(475,500,975,1000,2000,2475,2500,5000,10000)
  TRo <- c(S1,S2) |> unique() |> sort()

  UHSTable <- SaTRmodel[, .(IT = ITo, POE = ITo * 1 / TRo, TR = TRo, Sa = exp(a + b * log(TRo) + c * 1 / TRo), AEP = 1 / TRo), by = .(lat,lon,depth,Tn, p)]
# no NAs


  # *********************************************************************
  # Get PGA at vref ----
  PeakTable <- UHSTable[Tn == Tn_PGA,.(ID=IDo,PGA=Sa),by=.(lat,lon,depth,p,TR)]
  COLS <- colnames(UHSTable)[colnames(UHSTable) %in% colnames(PeakTable)]
  UHSTable <- PeakTable[UHSTable, on = COLS]
  PeakTable <- AEPTable[Tn==Tn_PGA,.(ID=IDo,PGA=Sa),by=.(p,Sa)] |> unique()
  COLS <- colnames(AEPTable)[colnames(AEPTable) %in% colnames(PeakTable)]
  AEPTable <- PeakTable[AEPTable, on = COLS]

  # *********************************************************************
  # Get AF*Sa for UHS ordinates  ----
  AFmodel_UHS <- data.table()
  AFmodel_AEP <- data.table()
  if(!is.null(vs30) & vref %in% c(760,3000)){
    for (Vs in vs30) {
      # AF estimated only as mean value. Ignoring quantiles from Sa(Tn). Setting p=0.50
      # by = .(p,lat,lon,depth,Tn) set results in a data.table .x with TR rows
      message(sprintf("> Fit UHS Site Response model (Stewart2017) for target Vs30 %4.1f m/s...", Vs))
      AUX <- UHSTable[, dsra::fitModel.AF.TR(.x=.SD,pga=PGA,q=AF_q_TARGET,Tn=Tn, vs30 = Vs,vref=vref), by = .(p,lat,lon,depth,Tn)]
      AFmodel_UHS <- data.table::rbindlist(list(AFmodel_UHS, AUX), use.names = TRUE)
      message(sprintf("> Fit AEP Site Response model (Stewart2017) for target Vs30 %4.1f m/s...", Vs))
      AUX <- AEPTable[, dsra::fitModel.AF.TR(.x=.SD,pga=PGA,q=AF_q_TARGET,Tn=Tn, vs30 = Vs,vref=vref), by = .(p,lat,lon,depth,Tn)]
      AFmodel_AEP <- data.table::rbindlist(list(AFmodel_AEP, AUX), use.names = TRUE)
    }
    # update UHSTable
    message(sprintf("> Update UHSTable ..."))
    AUX <- AFmodel_UHS[,.(Vref,Vs30,lat,lon,depth,p,Tn,AF,SID,SM,PGA,sdLnAF)] |> unique()
    COLS <- colnames(AUX)[colnames(AUX) %in% colnames(UHSTable)]
    AFmodel_UHS <- unique(AFmodel_UHS, by = c("lat","lon","depth","Tn", "p", "Vs30", "Vref", "SID", "SM"))
    UHSTable <- AUX[UHSTable, on = COLS][,`:=`(Sa=AF*Sa,PGA=AF*PGA)] |> unique()

    # update AEPTable
    message(sprintf("> Update AEPTable ..."))
    AUX <- AFmodel_AEP[,.(Vref,Vs30,lat,lon,depth,p,Tn,AF,SID,SM,PGA,sdLnAF)] |> unique()
    COLS <- colnames(AUX)[colnames(AUX) %in% colnames(AEPTable)]
    AFmodel_AEP <- unique(AFmodel_AEP, by = c("lat","lon","depth","Tn", "p", "Vs30", "Vref", "SID", "SM"))
    AEPTable <- AUX[AEPTable, on = COLS][,`:=`(Sa=AF*Sa,PGA=AF*PGA)] |> unique()


  }

  if(!is.null(vs30) & !(vref %in% c(760,3000)) ){
    stop("Error: You are trying to obtain spectral ordinates from an openquake model with vref=%4.1f to a target vs30=%4.1f but amplification factors AF are available for vref = 760 and vref=3000 m/s. ",vref,vs30)
  }

  if(is.null(vs30)){ #default case
    UHSTable <- data.table(UHSTable,Vref=vref,Vs30=vref,AF=1,SID=Vs30toSID(vref),SM="openquake",sdLnAF=0)
    AEPTable <- data.table(AEPTable,Vref=vref,Vs30=vref,AF=1,SID=Vs30toSID(vref),SM="openquake",sdLnAF=0)

  }

  return(list(AEPTable = AEPTable, UHSTable = UHSTable, AFmodel_AEP=AFmodel_AEP,AFmodel_UHS=AFmodel_UHS,SaTRmodel = SaTRmodel, RMwTable = RMwTable))
}
