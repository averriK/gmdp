#' @title Build GMDP (Rbind Approach, No Merges) With Param Col Removal
#'
#' @description
#' Same "rbind blocks for each Vs30" code as before, but now includes an optional
#' `TRo` argument for user-specified return periods when `param=TRUE`.
#'
#' @param path character. OQ hazard folder
#' @param IDo string. Identifier (e.g. "gmdp" or "gem")
#' @param engine c("openquake","user")
#' @param vs30 numeric vector
#' @param vref numeric
#' @param quantile_AF character. Default "mean"
#' @param param logical. If TRUE => expansions => param-based UHS, else read as-is
#' @param TRo numeric vector of return periods for param expansions. If NULL
#'   (default), uses the original built-in sequence. Used only when `param=TRUE`.
#'
#' @return list(AEPTable, UHSTable, AFmodel_AEP, AFmodel_UHS, SaTRmodel, RMwTable)
#' @export
#' @import data.table

buildGMDP <- function(
    path,
    IDo         = "gmdp",
    engine      = "openquake",
    vs30        = NULL,
    vref,
    quantile_AF = "mean",
    param       = FALSE,
    TRo         = NULL  # user can specify custom TR values
)
{
  on.exit(expr={rm(list=ls())}, add=TRUE) # your original pattern
  . <- NULL
  AF_q_TARGET <- quantile_AF

  ##### (1) Import AEP from OQ or user
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

  AEP_in <- data.table()
  if (engine=="openquake") {
    AEP_in <- importModel.oqAEP(TEMP, vref)
  } else {
    AEP_in <- importModel.userAEP(path, filename="AEP.xlsx")
  }
  if (!nrow(AEP_in)) stop("No hazard data read from path=", path)

  # Possibly read disagg
  RMwTable <- NULL
  message("> Building Disaggregation Hazard Table...")
  # RMwTable <- importModel.oqRMw(TEMP, ITo=unique(AEP_in$ITo)[1], vref=vref)

  # (2) param expansions => fitModel.Sa.TR => produce (a,b,c)
  message("> Fit AEP model from path=", path)
  Tn_PGA <- AEP_in[Tn >= 0, min(Tn)]
  SaTRmodel <- data.table()
  if (param) {
    message("> expansions => param => try fitModel.Sa.TR")
    SaTRmodel <- AEP_in[, fitModel.Sa.TR(.SD, TRmin=100, TRmax=10000), by=.(p,Tn)]
    if (!is.null(SaTRmodel) && nrow(SaTRmodel)) {
      message("> param expansions => nrow(SaTRmodel)=", nrow(SaTRmodel))
    } else {
      message("> no expansions => empty SaTRmodel")
      SaTRmodel <- data.table()
    }
  }

  # (3) Build param-based UHS or read UHS
  UHS_in <- data.table()
  if (param && nrow(SaTRmodel)) {
    message("> param=TRUE => manual expansions => no meltdown")

    # If user provided TRo, use it; else default expansions
    if (!is.null(TRo) && length(TRo)) {
      TRo_user <- sort(unique(TRo))
      message("> Using user-supplied TR: ", paste(TRo_user, collapse=", "))
    } else {
      # Original approach: 100–10000 by 25, plus key points
      S1 <- seq(100, 10000, 25)
      S2 <- c(475, 500, 975, 1000, 2000, 2475, 5000, 10000)
      TRo_user <- sort(unique(c(S1, S2)))
      message("> Using default param expansions: ", paste(TRo_user, collapse=", "))
    }

    # We'll guess an ITo from AEP_in
    ITo_val <- unique(AEP_in$ITo)[1]
    if (is.na(ITo_val)) ITo_val <- 50

    # EXACT STORAGE OF TR:
    UHS_in <- SaTRmodel[
      ,
      {
        if (is.na(a) || is.na(b) || is.na(c)) {
          return(data.table())
        }
        # Compute ground motion at each user-specified TRo_user
        Sa_calc <- exp(a + b*log(TRo_user) + c*(1/TRo_user))

        data.table(
          IT  = ITo_val,
          # We store EXACT user TR, no re-derivation from POE
          TR  = TRo_user,
          Sa  = Sa_calc,
          # AEP + POE come from the same TR, but we do NOT recalc TR from them
          AEP = 1 / TRo_user,
          POE = 1 - exp(-ITo_val * (1 / TRo_user))
        )
      },
      by = .(p,Tn,a,b,c,sdLnA)
    ]

    # Drop param columns
    dropCols <- c("a","b","c","sdLnA")
    dropCols <- intersect(dropCols, names(UHS_in))
    if (length(dropCols)) {
      UHS_in[, (dropCols) := NULL]
    }
    # Remove accidental .1 duplicates if any
    dupCols <- grep("\\.1$", names(UHS_in), value=TRUE)
    if (length(dupCols)) {
      UHS_in[, (dupCols) := NULL]
    }

  } else if (param) {
    message("> param=TRUE => no SaTRmodel => empty UHS_in")
  } else {
    message("> param=FALSE => read 'as-is' UHS from openquake")
    UHS_in <- importModel.oqUHS(TEMP)
  }

  # Tag them with ID, AF=1, etc.
  if (nrow(UHS_in)) {
    UHS_in[, `:=`(ID=IDo, AF=1, Vref=vref)]
  }
  setnames(AEP_in, old="IT", new="ITo", skip_absent=TRUE)
  AEP_in[, `:=`(ID=IDo, AF=1, Vref=vref)]

  # (4) define PGA from Tn== Tn_PGA
  message("> define PGA from Tn_PGA=", Tn_PGA)
  if (nrow(UHS_in)) {
    UHS_in[, PGA := ifelse(Tn == Tn_PGA, Sa, NA_real_)]
  }
  if (nrow(AEP_in)) {
    AEP_in[, PGA := ifelse(Tn == Tn_PGA, Sa, NA_real_)]
  }

  ##### (5) Apply site amp -> keep EXACT same TR
  # We do NOT recalc hazard or TR after we multiply Sa by AF
  finalUHS <- data.table()
  finalAEP <- data.table()
  AFmodel_UHS <- data.table()
  AFmodel_AEP <- data.table()

  if (is.null(vs30) || !length(vs30)) {
    message("> No site amp => single block => Vs30=vref, AF=1, sdLnAF=0")
    if (nrow(UHS_in)) {
      UHS_in[, `:=`(Vs30=vref, AF=1, sdLnAF=0)]
      finalUHS <- rbind(finalUHS, UHS_in, fill=TRUE)
    }
    if (nrow(AEP_in)) {
      AEP_in[, `:=`(Vs30=vref, AF=1, sdLnAF=0)]
      finalAEP <- rbind(finalAEP, AEP_in, fill=TRUE)
    }
  } else {
    if (!(vref %in% c(760,3000))) {
      stop("Site amp requires vref=760 or 3000. Found: ", vref)
    }
    message("> Will produce blocks for each Vs in vs30: ", paste(vs30, collapse=", "))
    for (Vs_ in vs30) {
      tmpUHS <- copy(UHS_in)
      tmpAEP <- copy(AEP_in)

      if (nrow(tmpUHS)) {
        AUXu <- tmpUHS[
          ,
          fitModel.AF.TR(.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs_, vref=vref),
          by=.(p,Tn)
        ]
        AFmodel_UHS <- rbind(AFmodel_UHS, AUXu, fill=TRUE)

        # We multiply Sa by AF but DO NOT recalc TR
        tmpUHS[, Sa     := Sa * AUXu$AF]
        tmpUHS[, PGA    := PGA * AUXu$AF]
        tmpUHS[, AF     := AUXu$AF]
        tmpUHS[, sdLnAF := AUXu$sdLnAF]
        tmpUHS[, Vs30   := Vs_]
      }
      if (nrow(tmpAEP)) {
        AUXa <- tmpAEP[
          ,
          fitModel.AF.TR(.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs_, vref=vref),
          by=.(p,Tn)
        ]
        AFmodel_AEP <- rbind(AFmodel_AEP, AUXa, fill=TRUE)

        # Multiply Sa by AF, keep same TR
        tmpAEP[, Sa     := Sa * AUXa$AF]
        tmpAEP[, PGA    := PGA * AUXa$AF]
        tmpAEP[, AF     := AUXa$AF]
        tmpAEP[, sdLnAF := AUXa$sdLnAF]
        tmpAEP[, Vs30   := Vs_]
      }

      finalUHS <- rbind(finalUHS, tmpUHS, fill=TRUE)
      finalAEP <- rbind(finalAEP, tmpAEP, fill=TRUE)
    }
  }

  return(list(
    AEPTable    = finalAEP,
    UHSTable    = finalUHS,
    AFmodel_AEP = AFmodel_AEP,
    AFmodel_UHS = AFmodel_UHS,
    SaTRmodel   = SaTRmodel,
    RMwTable    = RMwTable
  ))
}

