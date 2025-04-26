buildGMDP <- function(path,
                      IDo         = "gem",    # or "gmdp"
                      engine      = "openquake",
                      vs30        = NULL,
                      vref        = 760) {
  on.exit(expr = {rm(list = ls())}, add=TRUE)
  . <- NULL

  # 1) Import hazard data
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
    # or your user approach
    AEPTable <- importModel.userAEP(path, filename="AEP.xlsx")
  } else {
    stop("Unknown engine: ", engine)
  }
  ITo <- unique(AEPTable$ITo)[1]

  # 2) Build Disaggregation if needed
  RMwTable <- NULL
  message("> Building Disagregation Hazard Table...")
  if (engine=="openquake") {
    message("> Import Disaggregation data from openquake...")
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

  # 3) Now we decide: "gem" vs "gmdp" mode (or based on vs30 presence).
  UHSTable <- data.table()
  AFmodel_UHS <- data.table()
  AFmodel_AEP <- data.table()

  if (IDo=="gem" && is.null(vs30)) {
    message("> ID='gem' => Keep original AEPTable, no re-mesh, no site amp.")
    # we do not re-mesh AEPTable, no site amplification => build minimal UHSTable
    # (If you want a minimal "UHSTable" that just references raw data, you could do so,
    #  or you can keep it empty.)
    UHSTable <- data.table()
    # or if you want to unify, you might do a minimal "copy"
    # UHSTable <- copy(AEPTable)

    # no merges for PGA, or do them if you want Tn=0 => PGA in AEPTable
    # your choice. We'll skip for minimal.

  } else {
    # IDo=="gmdp" => we do re-mesh + site amplification, etc.
    message("> ID='gmdp' => Re-mesh hazard data, unify Tn=0 => PGA, apply site amp.")

    # define a re-mesh function
    reMeshCurve <- function(TRi, Sai, TRo) {
      Sa_star <- numeric(length(TRo))
      for (j in seq_along(TRo)) {
        if (TRo[j] <= TRi[1]) {
          Sa_star[j] <- Sai[1]
        } else if (TRo[j] >= TRi[length(TRi)]) {
          Sa_star[j] <- Sai[length(Sai)]
        } else {
          idx_high <- which(TRi >= TRo[j])[1]
          idx_low  <- idx_high - 1
          frac <- (TRo[j] - TRi[idx_low]) / (TRi[idx_high] - TRi[idx_low])
          val_log <- log(Sai[idx_low]) + frac*(log(Sai[idx_high]) - log(Sai[idx_low]))
          Sa_star[j] <- exp(val_log)
        }
      }
      Sa_star
    }

    reMeshGroup <- function(.SD, TRo, ITo) {
      # sort, filter
      DF <- .SD[order(TR)]
      DF <- DF[is.finite(TR) & TR>0]
      if (nrow(DF)<2) {
        return(data.table())  # or stop if you prefer
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
        POE   = ITo*(1/TRo),
        AEP   = 1/TRo,
        IT    = ITo,
        ID    = IDo,
        SM    = engine,
        Vs30  = vref,
        SID   = Vs30toSID(vref)
      )
      return(outDT)
    }

    # define ephemeral TRo
    S1 <- seq(100,20000,by=25)
    S2 <- c(1975,2475,4975,6225,9975,12475,19975)
    TRo <- sort(unique(c(S1,S2)))

    message("> Build UHSTable (re-meshed) ...")
    UHSTable <- AEPTable[
      Tn!=-1,
      reMeshGroup(.SD, TRo, ITo),
      by=.(lat,lon,depth,p,Tn)
    ]

    # Merge Tn=0 => PGA in UHSTable
    message("> Merge Tn=0 => PGA in UHSTable ...")
    PeakTable <- UHSTable[Tn==0, .(lat,lon,depth,p,TR,PGA=Sa)]
    UHSTable <- PeakTable[UHSTable, on=c("lat","lon","depth","p","TR")]

    # Also unify Tn=0 => PGA in AEPTable if you want site amp there
    # Re-mesh AEPTable if you want no NAs. Or skip if partial coverage is ok
    message("> Re-mesh AEPTable to unify coverage (optional) ...")
    # we re-mesh so Tn=0 also shares same TR => no leftover warnings
    AEP_remesh <- AEPTable[
      Tn!=-1,
      reMeshGroup(.SD, TRo, ITo),
      by=.(lat,lon,depth,p,Tn)
    ]
    # Now we unify Tn=0 => PGA
    PeakAEP <- AEP_remesh[Tn==0, .(lat,lon,depth,p,TR,PGA=Sa)]
    AEP_remesh <- PeakAEP[AEP_remesh, on=c("lat","lon","depth","p","TR")]

    AEPTable <- AEP_remesh  # override the old partial data

    # site amplification if vs30 != NULL
    if (!is.null(vs30)) {
      message("> Apply site amplification ...")
      for (Vs in vs30) {
        message(sprintf("> Fit UHS Site Response for Vs30=%.1f", Vs))
        AUX <- UHSTable[
          ,
          dsra::fitModel.AF.TR(.x=.SD, pga=PGA, q="mean", Tn=Tn, vs30=Vs, vref=vref),
          by=.(p,lat,lon,depth,Tn)
        ]
        AFmodel_UHS <- rbindlist(list(AFmodel_UHS, AUX), use.names=TRUE)

        message(sprintf("> Fit AEP Site Response for Vs30=%.1f", Vs))
        AUX <- AEPTable[
          ,
          dsra::fitModel.AF.TR(.x=.SD, pga=PGA, q="mean", Tn=Tn, vs30=Vs, vref=vref),
          by=.(p,lat,lon,depth,Tn)
        ]
        AFmodel_AEP <- rbindlist(list(AFmodel_AEP, AUX), use.names=TRUE)
      }

      # merges ...
      message("> Update UHSTable ...")
      AUX <- AFmodel_UHS[, .(Vref, Vs30, lat, lon, depth, p, Tn, AF, SID, SM, PGA, sdLnAF)] |> unique()
      COLS <- intersect(names(UHSTable), names(AUX))
      AFmodel_UHS <- unique(AFmodel_UHS, by=c("lat","lon","depth","Tn","p","Vs30","Vref","SID","SM"))
      UHSTable <- AUX[UHSTable, on=COLS][
        , `:=`(Sa=AF*Sa, PGA=AF*PGA)
      ] |> unique()

      message("> Update AEPTable ...")
      AUX <- AFmodel_AEP[, .(Vref, Vs30, lat, lon, depth, p, Tn, AF, SID, SM, PGA, sdLnAF)] |> unique()
      COLS <- intersect(names(AEPTable), names(AUX))
      AFmodel_AEP <- unique(AFmodel_AEP, by=c("lat","lon","depth","Tn","p","Vs30","Vref","SID","SM"))
      AEPTable <- AUX[AEPTable, on=COLS][
        , `:=`(Sa=AF*Sa, PGA=AF*PGA)
      ] |> unique()
    }
  }

  # if we want a default case for vs30 but not "gem" or "gmdp", can add code
  # finalize
  return(list(
    AEPTable    = AEPTable,
    UHSTable    = UHSTable,
    AFmodel_AEP = AFmodel_AEP,
    AFmodel_UHS = AFmodel_UHS,
    SaTRmodel   = NULL,
    RMwTable    = RMwTable
  ))
}
