# file: buildGMDP.R

buildGMDP <- function(path,
                      IDo        = "00000000",
                      engine     = "openquake",
                      vs30       = NULL,
                      vref) {
  on.exit(expr = {rm(list = ls())}, add = TRUE)
  . <- NULL

  AF_q_TARGET <- "mean"

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

  S1 <- seq(100,20000,by=25)
  S2 <- c(1975, 2475, 4975, 6225, 9975,12475,19975)
  TRo <- c(S1,S2) |> unique() |> sort()

  reMeshCurve <- function(TRi, Sai, TRo) {
    # piecewise log interpolation
    Sa_star <- numeric(length(TRo))
    nIn <- length(TRi)
    for (j in seq_along(TRo)) {
      trT <- TRo[j]
      if (trT <= TRi[1]) {
        Sa_star[j] <- Sai[1]
      } else if (trT >= TRi[nIn]) {
        Sa_star[j] <- Sai[nIn]
      } else {
        idx_high <- which(TRi >= trT)[1]
        idx_low  <- idx_high - 1
        frac <- (trT - TRi[idx_low])/(TRi[idx_high] - TRi[idx_low])
        val_log <- log(Sai[idx_low]) + frac*(log(Sai[idx_high]) - log(Sai[idx_low]))
        Sa_star[j] <- exp(val_log)
      }
    }
    Sa_star
  }

  remeshGroup <- function(.SD, TRo, ITo) {
    # sort, filter
    DF <- .SD[order(TR)]
    DF <- DF[is.finite(TR) & TR>0]
    if (nrow(DF)<2) {
      stop(sprintf("No enough hazard points for lat=%.4f lon=%.4f p=%s Tn=%.3f",
                   as.numeric(.SD$lat[1]), as.numeric(.SD$lon[1]),
                   as.character(.SD$p[1]), as.numeric(.SD$Tn[1])
      ))
    }

    TRi <- DF$TR
    Sai <- DF$Sa
    Sa_star <- reMeshCurve(TRi, Sai, TRo)

    outDT <- data.table(
      lat   = DF$lat[1],
      lon   = DF$lon[1],
      depth = DF$depth[1],
      p     = DF$p[1],
      Tn    = DF$Tn[1],
      TR    = TRo,
      Sa    = Sa_star,
      POE   = ITo*(1/TRo), # if you want that approx
      AEP   = 1/TRo,
      IT    = ITo
    )
    return(outDT)
  }

  # build UHSTable by grouping
  message("> Build UHSTable (re-meshed) ...")
  UHSTable <- AEPTable[
    Tn!=-1,
    remeshGroup(.SD, TRo, ITo),
    by=.(lat,lon,depth,p,Tn)
  ]
  UHSTable[, `:=`(
    ID   = IDo,
    SM   = engine,
    Vs30 = vref,
    SID  = Vs30toSID(vref)
  )]

  # (4) Merge Tn=0 => PGA into UHSTable
  message("> Merge Tn=0 => PGA into UHSTable ...")
  PeakTable <- UHSTable[Tn==0, .(lat,lon,depth,p,TR,PGA=Sa)]
  UHSTable <- PeakTable[UHSTable, on=c("lat","lon","depth","p","TR")]

  # (5) Also define PGA in AEPTable if Tn=0 is present for same TR
  message("> Merge Tn=0 => PGA into AEPTable ...")
  AEPTable[ , PGA := Sa[Tn == 0],           by = .(lat, lon, depth, p, TR)]
  # (6) Site Amplification
  AFmodel <- data.table()
  # AFmodel_AEP <- data.table()

  if (!is.null(vs30) && vref %in% c(760,3000)) {
    for (Vs in vs30) {
      message(sprintf("> Fit UHS Site Response for Vs30 %.1f...", Vs))
      AUX <- UHSTable[
        ,
        dsra::fitModel.AF.TR(.x=.SD, pga=PGA, q="mean", Tn=Tn, vs30=Vs, vref=vref),
        by=.(p,lat,lon,depth,Tn)
      ]
      AFmodel <- rbindlist(list(AFmodel, AUX), use.names=TRUE)

      message(sprintf("> Fit AEP Site Response for Vs30 %.1f...", Vs))

    }

    message("> Update UHSTable ...")
    AUX <- AFmodel[, .(Vref, Vs30, lat, lon, depth, p, Tn, AF, SID, SM, PGA, sdLnAF)] |> unique()
    COLS <- colnames(UHSTable)[colnames(UHSTable) %in% colnames(AUX)]
    AFmodel <- unique(AFmodel, by=c("lat","lon","depth","Tn","p","Vs30","Vref","SID","SM"))
    UHSTable <- AUX[UHSTable, on=COLS][
      , `:=`(Sa=AF*Sa, PGA=AF*PGA)
    ] |> unique()


  }
  if (is.null(vs30)) {
    UHSTable <- data.table(UHSTable, Vref=vref, Vs30=vref, AF=1,
                           SID=Vs30toSID(vref), SM="openquake", sdLnAF=0)
  }

  # final
  return(list(
    AEPTable    = AEPTable,
    UHSTable    = UHSTable,
    AFmodel = AFmodel,
    SaTRmodel   = NULL,
    RMwTable    = RMwTable
  ))
}
