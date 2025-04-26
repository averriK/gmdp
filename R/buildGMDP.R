#' @title buildGMDP (Old Code Style: Minimal PeakTable, No Extra Columns)
#'
#' @description
#' This function replicates your old code's approach for building UHS from AEP
#' or meltdown, ensuring no duplicate columns like \code{i.p}, \code{Tn.1}, etc.
#' **Key points**:
#' \enumerate{
#'   \item \code{param=TRUE}: do old \code{(a,b,c)} expansions from AEP, grouping by \code{(p,Tn)}.
#'   \item \code{param=FALSE}: read meltdown UHS via \code{importModel.oqUHS}.
#'   \item We create a minimal "peak table" \code{PeakUHS} with only \code{(p,TR,PGA=Sa)}
#'         for the row where \code{Tn==Tn_PGA}, then merge by \code{(p,TR)} so
#'         there's no overlap of \code{Tn}.
#'   \item Site amplification also merges similarly with base R merges, never
#'         using \code{allow.cartesian=TRUE}.
#' }
#'
#' @param path character hazard folder
#' @param IDo  string ID to store in the final tables
#' @param engine "openquake" or "user"
#' @param vs30 numeric vector or NULL => no site amp
#' @param vref numeric reference Vs30 (760 or 3000)
#' @param quantile_AF character e.g. "mean"
#' @param param logical. If TRUE => expansions, else meltdown
#'
#' @return list(AEPTable, UHSTable, AFmodel_AEP, AFmodel_UHS, SaTRmodel, RMwTable)
#' @import data.table
#' @export
buildGMDP <- function(
    path,
    IDo         = "00000000",
    engine      = "openquake",
    vs30        = NULL,
    vref,
    quantile_AF = "mean",
    param       = FALSE
)
{
  on.exit(expr={rm(list=ls())}, add=TRUE)
  . <- NULL

  AF_q_TARGET <- quantile_AF

  #----------------------------------------------------------------------
  # (1) Build AEP Table (old code style)
  #----------------------------------------------------------------------
  message("> Build AEP Table...")

  TEMP <- tempdir()
  if (dir.exists(TEMP)) {
    unlink(TEMP, recursive=TRUE)
    TEMP <- tempdir()
  }
  zips <- list.files(path, pattern="\\.zip$", full.names=TRUE)
  for (zf in zips) {
    utils::unzip(zf, exdir=TEMP, junkpaths=TRUE)
  }

  AEPTable <- data.table()
  if (engine=="openquake") {
    message("> Importing AEP data from openquake...")
    AEPTable <- importModel.oqAEP(TEMP, vref)
  } else {
    message("> Importing AEP data from user (AEP.xlsx)...")
    AEPTable <- importModel.userAEP(path, filename="AEP.xlsx")
  }
  if (!nrow(AEPTable)) {
    stop("No hazard data read from path=", path)
  }

  # pick an ITo
  ITo_val <- unique(AEPTable$ITo)[1]
  if (is.na(ITo_val)) ITo_val <- 50

  #----------------------------------------------------------------------
  # (1B) Disagg => RMwTable if engine=="openquake"
  #----------------------------------------------------------------------
  RMwTable <- NULL
  if (engine=="openquake") {
    message("> Building Disaggregation Hazard Table from OQ...")
    RMwTable <- importModel.oqRMw(TEMP, ITo=ITo_val, vref=vref)
    if (is.null(RMwTable) || !nrow(RMwTable)) {
      RMwTable <- NULL
      message("> Disaggregation data not available => RMwTable=NULL")
    }
  }

  #----------------------------------------------------------------------
  # (2) param=TRUE => expansions, else meltdown
  #----------------------------------------------------------------------
  message("> param=", param, " => building UHS from expansions or meltdown")

  SaTRmodel <- data.table()
  UHSTable  <- data.table()

  # old code => Tn_PGA is min Tn >= 0
  Tn_PGA <- AEPTable[Tn>=0, min(Tn)]

  if (param) {
    #--------------------------------------------------------------------
    # (2A) expansions => by=(p,Tn)
    #--------------------------------------------------------------------
    message("> expansions => fitModel.Sa.TR => grouping by (p,Tn)")
    SaTRmodel <- AEPTable[
      ,
      fitModel.Sa.TR(.SD, TRmin=100, TRmax=10000),
      by=.(p,Tn)
    ]
    if (!nrow(SaTRmodel)) {
      message(">> No expansions => empty SaTRmodel => skip UHS build.")
    } else {
      message(">> Build param-based UHS => S1+S2 ...")
      S1 <- seq(100,10000,by=25)
      S2 <- c(475,500,975,1000,2000,2475,2500,5000,10000)
      TRset <- sort(unique(c(S1,S2)))

      UHSTable <- SaTRmodel[
        ,
        {
          if (is.na(a) || is.na(b) || is.na(c)) return(data.table())
          Sa_ <- exp(a + b*log(TRset) + c*(1/TRset))
          data.table(
            p   = p,
            Tn  = Tn,
            TR  = TRset,
            Sa  = Sa_,
            AEP = 1/TRset,
            POE = 1 - exp(-ITo_val*(1/TRset))
          )
        },
        by=.(p,Tn,a,b,c,sdLnA)
      ]
      UHSTable[, c("a","b","c","sdLnA") := NULL]
    }
  } else {
    #--------------------------------------------------------------------
    # (2B) meltdown => importModel.oqUHS
    #--------------------------------------------------------------------
    message("> param=FALSE => read meltdown from OQ => importModel.oqUHS")
    UHSTable <- importModel.oqUHS(TEMP)
    if (!nrow(UHSTable)) {
      stop("No meltdown UHS data found for param=FALSE in path=", path)
    }
  }

  #----------------------------------------------------------------------
  # (3) define PGA => minimal "peak table" => no duplicate Tn
  #----------------------------------------------------------------------
  message("> define PGA from Tn_PGA=", Tn_PGA)

  # (3A) UHS => build PeakUHS with (p,TR,PGA=Sa) only
  if (nrow(UHSTable)) {
    PeakUHS <- UHSTable[Tn==Tn_PGA, .(p, TR, PGA=Sa)]
    PeakUHS <- unique(PeakUHS, by=c("p","TR"))  # ensure no duplicates
    # base R merge => no cartesian => no duplicate Tn
    UHSTable <- merge(
      x=UHSTable,
      y=PeakUHS,
      by=c("p","TR"),
      all.x=TRUE,
      suffixes=c("","_peak")
    )
    UHSTable[, PGA_peak := NULL]
  }

  # (3B) AEP => merges by p
  PeakAEP <- AEPTable[Tn==Tn_PGA, .(p, PGA=Sa)]
  PeakAEP <- unique(PeakAEP, by="p")
  AEPTable <- merge(
    x=AEPTable,
    y=PeakAEP,
    by="p",
    all.x=TRUE,
    suffixes=c("","_peak")
  )
  AEPTable[, PGA_peak := NULL]

  #----------------------------------------------------------------------
  # (4) site amplification => group by (p,Tn)
  #----------------------------------------------------------------------
  message("> site amplification => group by (p,Tn), minimal merges")

  AFmodel_UHS <- data.table()
  AFmodel_AEP <- data.table()
  finalUHS <- data.table()
  finalAEP <- data.table()

  if (!is.null(vs30) && length(vs30)>0) {
    if (!(vref %in% c(760,3000))) {
      stop(sprintf(
        "Error: vref=%g but only 760 or 3000 are valid for site amp", vref
      ))
    }
    for (Vs_ in vs30) {
      message(sprintf(">> Fit site amp => Vs30=%.1f", Vs_))

      # (4A) UHS
      if (nrow(UHSTable)) {
        AUXu <- UHSTable[
          ,
          fitModel.AF.TR(.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs_, vref=vref),
          by=.(p,Tn)
        ]
        blockUHS <- merge(
          x=UHSTable, y=AUXu,
          by=c("p","Tn"),
          all.x=TRUE,
          suffixes=c("","_af")
        )
        blockUHS[, `:=`(
          Sa  = Sa*AF,
          PGA = PGA*AF,
          Vs30= Vs_
        )]
        AFmodel_UHS <- rbind(AFmodel_UHS, AUXu, use.names=TRUE, fill=TRUE)
        finalUHS    <- rbind(finalUHS, blockUHS, use.names=TRUE, fill=TRUE)
      }

      # (4B) AEP
      AUXa <- AEPTable[
        ,
        fitModel.AF.TR(.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs_, vref=vref),
        by=.(p,Tn)
      ]
      blockAEP <- merge(
        x=AEPTable, y=AUXa,
        by=c("p","Tn"),
        all.x=TRUE,
        suffixes=c("","_af")
      )
      blockAEP[, `:=`(
        Sa  = Sa*AF,
        PGA = PGA*AF,
        Vs30= Vs_
      )]
      AFmodel_AEP <- rbind(AFmodel_AEP, AUXa, use.names=TRUE, fill=TRUE)
      finalAEP    <- rbind(finalAEP, blockAEP, use.names=TRUE, fill=TRUE)
    }
  } else {
    message(">> No site amp => single block => set AF=1")
    UHSTable[, `:=`(Vs30=vref, AF=1, sdLnAF=0)]
    AEPTable[,  `:=`(Vs30=vref, AF=1, sdLnAF=0)]
    finalUHS <- UHSTable
    finalAEP <- AEPTable
  }

  #----------------------------------------------------------------------
  # (5) Tag final ID => old code appended ID last
  #----------------------------------------------------------------------
  finalUHS[, ID := IDo]
  finalAEP[, ID := IDo]

  #----------------------------------------------------------------------
  # (6) Return => old code structure
  #----------------------------------------------------------------------
  message("> Done building GMDP => no duplicated columns => minimal merges with peakTable.")
  return(list(
    AEPTable    = finalAEP,
    UHSTable    = finalUHS,
    AFmodel_AEP = AFmodel_AEP,
    AFmodel_UHS = AFmodel_UHS,
    SaTRmodel   = SaTRmodel,
    RMwTable    = RMwTable
  ))
}
