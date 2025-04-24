#' @title Build Ground-Motion Data Products from OpenQuake
#'
#' @description
#' Replicates the old code logic but allows \code{param=FALSE} to skip param expansions.
#' Always reads "as-is" UHS from CSV if present, so \code{UHSTable} is never empty.
#'
#' @param path character. Path to the folder containing OQ hazard & UHS CSVs/zips
#' @param IDo character Output ID label
#' @param engine character c("openquake","user")
#' @param vs30 numeric. Vs30 steps
#' @param vref numeric reference Vs30
#' @param quantile_AF character. c("mean", 0.16, 0.50, 0.84)
#' @param param logical. If TRUE => do param expansions
#' @return A list of \code{(AEPTable, UHSTable, AFmodel_AEP, AFmodel_UHS, SaTRmodel, RMwTable)}
#' @export
#' @import data.table
buildGMDP <- function(path,
                      IDo="00000000",
                      engine="openquake",
                      vs30=NULL,
                      vref,
                      quantile_AF="mean",
                      param=FALSE)
{
  on.exit(expr = {rm(list = ls())}, add=TRUE)
  . <- NULL
  AF_q_TARGET <- quantile_AF

  # 1) Import AEP
  message("> Build AEP Table...")
  AEPTable <- NULL
  TEMP <- tempdir()
  if (engine=="openquake") {
    message("> Unzip OQ data ...")
    if (dir.exists(TEMP)) {
      unlink(TEMP, recursive=TRUE)
      TEMP <- tempdir()
    }
    zipHaz <- list.files(path, pattern="\\.zip$", full.names=TRUE)
    for (ZIPFILE in zipHaz) {
      utils::unzip(zipfile=ZIPFILE, exdir=TEMP, junkpaths=TRUE)
    }
    message("> Import AEP data from openquake...")
    AEPTable <- importModel.oqAEP(path=TEMP, vref=vref)
  } else if (engine=="user") {
    message("> (user-based hazard import) ...")
    # or importModel.userAEP etc.
    AEPTable <- importModel.userAEP(path, filename="AEP.xlsx")
  } else {
    stop("engine must be 'openquake' or 'user'")
  }
  if (!nrow(AEPTable)) {
    stop("No hazard data found in path=", path)
  }

  ITo <- unique(AEPTable$ITo)[1]

  # 2) Possibly disaggregation
  RMwTable <- NULL
  message("> Building Disaggregation Hazard Table...")
  if (engine=="openquake") {
    message("> Import Disaggregation data from openquake ...")
    RMwTable <- importModel.oqRMw(path=TEMP, ITo=ITo, vref=vref)
  }
  if (!is.null(RMwTable)) {
    # RMwTable[, SID := Vs30toSID(vref)]
    RMwTable[, Vs30 := vref]
    RMwTable[, SM := engine]
    RMwTable[, ID := IDo]
    RMwTable[, IT := ITo]
  } else {
    message("> Disaggregation data not available.")
  }

  # 3) Param expansions => SaTRmodel
  message("> Fit AEP model from ", path)
  Tn_PGA <- AEPTable[Tn >= 0, min(Tn)]
  SaTRmodel <- data.table()  # empty if param=FALSE

  if (param) {
    SaTRmodel <- AEPTable[
      ,
      fitModel.Sa.TR(x=.SD, TRmin=100, TRmax=10000),
      by=c("Tn","p")
    ][, .( Tn, p, a, b, c, sdLnA, Sa_Unit="g")]
  } else {
    message("> param=FALSE => skipping expansions => no SaTRmodel")
  }

  # 4) Build param-based UHS if param=TRUE. Else read "as-is" UHS from files
  message("> Set UHS spectral ordinates ...")
  UHSTable <- data.table()
  if (param && nrow(SaTRmodel)) {
    S1 <- seq(100,10000,25)
    S2 <- c(475,500,975,1000,2000,2475,2500,5000,10000)
    TRo<- sort(unique(c(S1,S2)))
    UHSTable <- SaTRmodel[
      ,
      {
        Sa_calc <- exp(a + b*log(TRo) + c*(1/TRo))
        data.table(
          IT=ITo,
          POE= ITo*(1/TRo),
          TR=TRo,
          Sa=Sa_calc,
          AEP=1/TRo
        )
      },
      by=.( Tn, p)
    ]
    message("> param-based UHS created from expansions => nrow=", nrow(UHSTable))
  } else {
    # param=FALSE => read "as-is" UHS from CSV
    message("> param=FALSE => read 'as-is' UHS from openquake files ...")
    UHSTable <- importModel.oqUHS(path=TEMP)  # or path, if that is correct
    if (!nrow(UHSTable)) {
      message("> No as-is UHS found => UHSTable empty")
    } else {
      message("> Found as-is UHS => nrow=", nrow(UHSTable))
    }
  }

  # Tag them
  if (nrow(UHSTable)) {
    UHSTable[, `:=`(ID=IDo, AF=1, Vref=vref)]
  }
  AEPTable[, `:=`(ID=IDo, AF=1, Vref=vref)]

  # 5) "Get PGA from Tn== Tn_PGA" => same old code
  message("> Get PGA at Tn_PGA=", Tn_PGA)
  if (nrow(UHSTable)) {
    grpCols <- intersect(c("p","TR"), names(UHSTable))
    PeakTable <- UHSTable[Tn == Tn_PGA, .(ID=IDo,PGA=Sa), by=grpCols]
    colMatch <- colnames(UHSTable)[colnames(UHSTable) %in% colnames(PeakTable)]
    UHSTable <- PeakTable[UHSTable, on=colMatch]
  }
  if (nrow(AEPTable)) {
    PK2 <- AEPTable[Tn==Tn_PGA, .(ID=IDo, PGA=Sa), by=.(p,Sa)] |> unique()
    colMatch2 <- colnames(AEPTable)[colnames(AEPTable) %in% colnames(PK2)]
    AEPTable <- PK2[AEPTable, on=colMatch2]
  }

  # 6) Site Amp => merges
  message("> Get AF*Sa for UHS ordinates ...")
  AFmodel_UHS <- data.table()
  AFmodel_AEP <- data.table()

  if (!is.null(vs30) && length(vs30)>0 && vref %in% c(760,3000)) {
    for (Vs in vs30) {
      if (nrow(UHSTable)) {
        message("> Fit UHS Site Response for Vs30=", Vs)
        # if UHSTable is non-empty => do it
        AUX <- UHSTable[
          ,
          fitModel.AF.TR(.x=.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs, vref=vref),
          by=.(p,  Tn)
        ]
        AFmodel_UHS <- rbind(AFmodel_UHS, AUX, use.names=TRUE)
      }

      # AEP
      if (nrow(AEPTable)) {
        message("> Fit AEP Site Response for Vs30=", Vs)
        AUX2 <- AEPTable[
          ,
          fitModel.AF.TR(.x=.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs, vref=vref),
          by=.(p,  Tn)
        ]
        AFmodel_AEP <- rbind(AFmodel_AEP, AUX2, use.names=TRUE)
      }
    }

    # now we merge site amp => multiply Sa,PGA
    if (nrow(UHSTable) && nrow(AFmodel_UHS)) {
      mU <- AFmodel_UHS[, .(Vref,Vs30,p,Tn,AF,PGA,sdLnAF)] |> unique()
      cU <- intersect(names(UHSTable), names(mU))
      AFmodel_UHS <- unique(AFmodel_UHS, by=c("Tn","p","Vs30","Vref"))
      UHSTable <- mU[UHSTable, on=cU][, `:=`(Sa=AF*Sa, PGA=AF*PGA)] |> unique()
    }

    if (nrow(AEPTable) && nrow(AFmodel_AEP)) {
      mA <- AFmodel_AEP[, .(Vref,Vs30,p,Tn,AF,PGA,sdLnAF)] |> unique()
      cA <- intersect(names(AEPTable), names(mA))
      AFmodel_AEP <- unique(AFmodel_AEP, by=c("Tn","p","Vs30","Vref"))
      AEPTable <- mA[AEPTable, on=cA][, `:=`(Sa=AF*Sa, PGA=AF*PGA)] |> unique()
    }
  } else if (!is.null(vs30) && !(vref %in% c(760,3000))) {
    stop("Error: you are trying site amp with vref=", vref, " not in {760,3000}")
  } else {
    # no site amp => fill default
    if (nrow(UHSTable)) {
      UHSTable[,Vs30:=vref]
      UHSTable[,AF:=1]
      UHSTable[,sdLnAF:=0]
    }
    AEPTable[,Vs30:=vref]
    AEPTable[,AF:=1]
    AEPTable[,sdLnAF:=0]

  }

  return(list(
    AEPTable    = AEPTable,
    UHSTable    = UHSTable,
    AFmodel_AEP = AFmodel_AEP,
    AFmodel_UHS = AFmodel_UHS,
    SaTRmodel   = SaTRmodel,
    RMwTable    = RMwTable
  ))
}
