# file: buildGMDP.R

buildGMDP <- function(path,
                      IDo        = "gmdp",
                      engine     = "openquake",
                      vs30       = NULL,
                      vref = 760,
                      TRo = seq(400,10000,by=25),
                      q_AF = "mean") {

  # (1) Import hazard data
  message("> Build AEP Table...")
  AEPTable <- NULL
  if (engine=="openquake") {
    message("> Unzip OQ data ...")
    TEMP <- tempdir()
    if (dir.exists(TEMP)) {
      unlink(TEMP, recursive=TRUE)
      TEMP <- tempdir()
    }
    for (ZIPFILE in list.files(path, pattern="*.zip", full.names=TRUE)) {
      utils::unzip(zipfile=ZIPFILE, junkpaths=TRUE, exdir=TEMP)
    }
    message("> Import AEP data from openquake...")
    AEPTable <- importModel.oqAEP(path=TEMP, vref=vref)
  } else if (engine=="user") {
    message("> Unzip USER data ...")
    message("> Import AEP data from user ...")
    AEPTable <- importModel.userAEP(path,filename="AEP.xlsx")
  } else {
    stop("Unknown engine: ", engine)
  }
  ITo <- unique(AEPTable$ITo)[1]

  # (2) Disagg table if any
  RMwTable <- NULL
  message("> Building Disagregation Hazard Table...")
  if (engine=="openquake") {
    message("> Import Disaggregation data from openquake...")
    RMwTable <- importModel.oqRMw(path=TEMP, ITo=ITo, vref=vref)
  }
  if (!is.null(RMwTable)) {
    RMwTable[, `:=`(SID=Vs30toSID(vref), Vs30=vref, SM=engine, ID=IDo, IT=ITo)]
  } else {
    message("> Disaggregation data not available.")
  }

  # (3) Re-mesh approach: define a dense TRo
  message("> Re-mesh hazard data on a uniform TR grid (no param-fitting)...")

  COLS <- setdiff(colnames(AEPTable),c("Sa","POE","AEP","TR"))
  UHSTable <- AEPTable[Tn!=-1 , remeshGroup(.SD, TRo),       by = COLS    ]


  # (4) Merge Tn=0 => PGA into UHSTable
  message("> Merge Tn=0 => PGA into UHSTable ...")

  COLS <- setdiff(colnames(UHSTable),c("Sa","Tn","AEP","POE"))
  UHSTable[, PGA := Sa[Tn == 0],by = COLS]
  UHSTable[,Vref:=vref]

  AFmodel <- data.table()

  if (!is.null(vs30) && vref %in% c(760,3000)) {
    for (Vs in vs30) {
      message(sprintf("> Fit UHS Site Response for Vs30 %.1f...", Vs))
      COLS <- setdiff(colnames(UHSTable),c("Sa","PGA","AEP","POE"))

      AUX <- UHSTable[,fitModel.AF.TR(.x=.SD, pga=PGA, q=q_AF, Tn=Tn, vs30=Vs, vref=Vref),by=COLS]
      AFmodel <- rbindlist(list(AFmodel, AUX), use.names=TRUE)
      message(sprintf("> Fit AEP Site Response for Vs30 %.1f...", Vs))
    }
  }
  if (is.null(vs30)) {
    UHSTable <- data.table(UHSTable,  Vs30=vref, AF=1,sdLnAF=0)
  }

  message("> Update UHSTable ...")
  COLS <- colnames(UHSTable)[colnames(UHSTable) %in% colnames(AFmodel)]
  UHSTable <- AFmodel[UHSTable, on=COLS][    , `:=`(Sa=AF*Sa, PGA=AF*PGA)  ] |> unique()

  return(list(
    AEPTable    = AEPTable,
    UHSTable    = UHSTable,
    AFmodel = AFmodel,
    RMwTable    = RMwTable
  ))
}
