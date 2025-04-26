buildGMDP <- function(path,
                      IDo         = "00000000",
                      engine      = "openquake",
                      vs30        = NULL,
                      vref,
                      quantile_AF = "mean") {
  on.exit(expr = {rm(list = ls())}, add = TRUE)
  . <- NULL

  AF_q_TARGET <- quantile_AF

  # -----------------------------------------------------------
  # (1) Build AEP Table
  message(sprintf("> Build AEP Table..."))
  AEPTable <- NULL
  if (engine == "openquake") {
    message("> Unzip OQ data ...")
    TEMP <- tempdir()
    if (dir.exists(TEMP)) {
      unlink(TEMP, recursive=TRUE)
      TEMP <- tempdir()
    }
    FILES <- list.files(path, pattern="*.zip", full.names=TRUE)
    for (ZIPFILE in FILES) {
      utils::unzip(zipfile=ZIPFILE, junkpaths=TRUE, exdir=TEMP)
    }
    message("> Import AEP data from openquake...")
    AEPTable <- importModel.oqAEP(path=TEMP, vref=vref)


  } else if (engine == "user") {
    message("> Import AEP data from user...")
    AEPTable <- importModel.userAEP(path, filename="AEP.xlsx")
  } else {
    stop("Unknown engine: ", engine)
  }
  ITo <- unique(AEPTable$ITo)[1]

  # -----------------------------------------------------------
  # (2) Build Disaggregation Table if available
  message("> Building Disagregation Hazard Table...")
  RMwTable <- NULL
  if (engine == "openquake") {
    RMwTable <- importModel.oqRMw(path=TEMP, ITo=ITo, vref=vref)
  }
  if (!is.null(RMwTable)) {
    RMwTable[, `:=`(
      SID  = Vs30toSID(vref),
      Vs30 = vref,
      SM   = engine,
      ID   = IDo,
      IT   = ITo
    )]
  } else {
    message("> Disaggregation data not available.")
  }

  # -----------------------------------------------------------
  # (3) Re-mesh hazard data on a uniform TR grid
  message("> Re-mesh hazard data on a uniform TR grid...")

  # S1, S2 => ephemeral variables
  S1 <- seq(100, 20000, by=25)
  S2 <- c(1975, 2475, 4975, 6225, 9975, 12475, 19975) # e.g. special milestones
  TRo <- c(S1, S2) |> unique() |> sort()

  # local interpolation function
  reMeshCurve <- function(DF, TRo) {
    # DF has columns: TR, Sa (assumed sorted)
    Sa_star <- numeric(length(TRo))
    for (i in seq_along(TRo)) {
      trT <- TRo[i]
      if (trT <= DF$TR[1]) {
        Sa_star[i] <- DF$Sa[1]
      } else if (trT >= DF$TR[.N]) {
        Sa_star[i] <- DF$Sa[.N]
      } else {
        idx_high <- which(DF$TR >= trT)[1]
        idx_low  <- idx_high - 1
        x1 <- DF$TR[idx_low]
        x2 <- DF$TR[idx_high]
        y1 <- DF$Sa[idx_low]
        y2 <- DF$Sa[idx_high]
        frac <- (trT - x1)/(x2 - x1)
        val_log <- log(y1) + frac*(log(y2) - log(y1))
        Sa_star[i] <- exp(val_log)
      }
    }
    data.table(TR=TRo, Sa=Sa_star)
  }

  # -----------------------------------------------------------
  # (4) Build UHSTable by grouping (ignore Tn=-1 => PGV)
  message("> Build UHSTable ...")
browser()
  UHSTable <- AEPTable[
    Tn != -1,
    {
      # Sort
      DF <- .SD[order(TR)]
      # Filter invalid or infinite TR
      DF <- DF[is.finite(TR) & TR > 0]

      # We expect at least 2 rows for interpolation
      if (nrow(DF) < 2) {
        stop(sprintf(
          "Found no valid hazard points to interpolate (nrow=%d) for lat=%.4f lon=%.4f p=%s Tn=%.3f.
           Possibly parser/data issue. Stopping.",
          nrow(DF), .SD$lat[1], .SD$lon[1], as.character(.SD$p[1]), .SD$Tn[1]
        ))
      }

      # re-mesh
      RES <- reMeshCurve(DF[, .(TR, Sa)], TRo)
      RES[, `:=`(
        lat   = DF$lat[1],
        lon   = DF$lon[1],
        depth = DF$depth[1],
        p     = DF$p[1],
        Tn    = DF$Tn[1],
        # approximate if you want a consistent "POE" from TR
        POE   = ITo*(1/RES$TR),
        AEP   = 1/RES$TR,
        IT    = ITo
      )]
      RES
    },
    by=.(lat, lon, depth, p, Tn)
  ]

  UHSTable[, `:=`(
    ID   = IDo,
    SM   = engine,
    Vs30 = vref,
    SID  = Vs30toSID(vref)
  )]

  # -----------------------------------------------------------
  # (5) Merge PGA from Tn=0 if present
  message("> Merge PGA from Tn=0 if present ...")
  Tn_PGA <- 0
  PeakTable <- UHSTable[
    Tn == Tn_PGA,
    .(ID=IDo, PGA=Sa),
    by=.(lat, lon, depth, p, TR)
  ]
  COLS <- intersect(names(UHSTable), names(PeakTable))
  UHSTable <- PeakTable[UHSTable, on=COLS]

  # If you previously had a second merge into AEPTable, remove or comment it out:
  # (We rely on real Tn=0 data if it exists, no artificial PGA from min Tn.)
  # ...
  # e.g.
  # PeakTable <- AEPTable[ Tn==Tn_PGA, .(ID=IDo, PGA=Sa), by=.(p, Sa) ] |> unique()
  # ...
  # AEPTable <- PeakTable[AEPTable, on=COLS]

  # -----------------------------------------------------------
  # (6) Site Amplification logic
  AFmodel_UHS <- data.table()
  AFmodel_AEP <- data.table()

  if (!is.null(vs30) & vref %in% c(760,3000)) {
    for (Vs in vs30) {
      message(sprintf("> Fit UHS Site Response model (Stewart2017) for Vs30 %4.1f...", Vs))
      AUX <- UHSTable[
        ,
        dsra::fitModel.AF.TR(.x=.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs, vref=vref),
        by=.(p, lat, lon, depth, Tn)
      ]
      AFmodel_UHS <- data.table::rbindlist(list(AFmodel_UHS, AUX), use.names=TRUE)

      message(sprintf("> Fit AEP Site Response model (Stewart2017) for Vs30 %4.1f...", Vs))
      AUX <- AEPTable[
        ,
        dsra::fitModel.AF.TR(.x=.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs, vref=vref),
        by=.(p, lat, lon, depth, Tn)
      ]
      AFmodel_AEP <- data.table::rbindlist(list(AFmodel_AEP, AUX), use.names=TRUE)
    }

    message("> Update UHSTable ...")
    AUX <- AFmodel_UHS[
      , .(Vref, Vs30, lat, lon, depth, p, Tn, AF, SID, SM, PGA, sdLnAF)
    ] |> unique()
    COLS <- intersect(names(UHSTable), names(AUX))
    AFmodel_UHS <- unique(AFmodel_UHS,
                          by=c("lat","lon","depth","Tn","p","Vs30","Vref","SID","SM")
    )
    UHSTable <- AUX[UHSTable, on=COLS][
      , `:=`(Sa=AF*Sa, PGA=AF*PGA)
    ] |> unique()

    message("> Update AEPTable ...")
    AUX <- AFmodel_AEP[
      , .(Vref, Vs30, lat, lon, depth, p, Tn, AF, SID, SM, PGA, sdLnAF)
    ] |> unique()
    COLS <- intersect(names(AEPTable), names(AUX))
    AFmodel_AEP <- unique(AFmodel_AEP,
                          by=c("lat","lon","depth","Tn","p","Vs30","Vref","SID","SM")
    )
    AEPTable <- AUX[AEPTable, on=COLS][
      , `:=`(Sa=AF*Sa, PGA=AF*PGA)
    ] |> unique()

  } else if (is.null(vs30)) {
    # no site amplification
    UHSTable <- data.table(UHSTable,
                           Vref=vref, Vs30=vref, AF=1,
                           SID=Vs30toSID(vref), SM="openquake", sdLnAF=0)
    AEPTable <- data.table(AEPTable,
                           Vref=vref, Vs30=vref, AF=1,
                           SID=Vs30toSID(vref), SM="openquake", sdLnAF=0)
  } else {
    stop(sprintf(
      "Error: Trying to get spectral ordinates with vref=%4.1f => no AF model for that reference",
      vref
    ))
  }

  # -----------------------------------------------------------
  # (7) Return
  return(list(
    AEPTable    = AEPTable,
    UHSTable    = UHSTable,
    AFmodel_AEP = AFmodel_AEP,
    AFmodel_UHS = AFmodel_UHS,
    SaTRmodel   = NULL,  # param fit removed
    RMwTable    = RMwTable
  ))
}
