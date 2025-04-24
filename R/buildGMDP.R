#' @title Build GMDP (Rbind Approach, No Merges) With Param Col Removal
#'
#' @description
#' Same "rbind blocks for each Vs30" code as before, but we remove param columns
#' (`a,b,c,sdLnA`) immediately, so the final tables don't contain them when
#' you rbind with any other table that doesn't have them.
#'
#' @param path character. OQ hazard folder
#' @param IDo ...
#' @param engine c("openquake","user")
#' @param vs30 numeric vector
#' @param vref numeric
#' @param quantile_AF default "mean"
#' @param param if TRUE => expansions => param-based UHS, else read as-is
#' @return list(AEPTable, UHSTable, AFmodel_AEP, AFmodel_UHS, SaTRmodel, RMwTable)
#' @export
#' @import data.table

buildGMDP <- function(path,
                      IDo         = "gmdp",
                      engine      = "openquake",
                      vs30        = NULL,
                      vref,
                      quantile_AF = "mean",
                      param       = FALSE)
{
  on.exit(expr={rm(list=ls())}, add=TRUE) # your original choice, though usually not recommended
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
  # if you had "importModel.oqRMw" do it. If not, skip
  # RMwTable <- importModel.oqRMw(TEMP, ITo=unique(AEP_in$ITo)[1], vref=vref)

  # (2) param expansions => fitModel.Sa.TR => produce (a,b,c)
  message("> Fit AEP model from path=", path)
  Tn_PGA <- AEP_in[Tn>=0, min(Tn)]
  SaTRmodel <- data.table()
  if (param) {
    message("> expansions => param => try fitModel.Sa.TR")
    SaTRmodel <- AEP_in[ , fitModel.Sa.TR(.SD, TRmin=100, TRmax=10000), by=.(p,Tn)]
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
    S1 <- seq(100, 10000, 25)
    S2 <- c(475, 500, 975, 1000, 2000, 2475, 2500,5000, 9975, 10000)
    TRo <- sort(unique(c(S1, S2)))
    ITo_val <- unique(AEP_in$ITo)[1]
    if (is.na(ITo_val)) ITo_val <- 50

    # We'll rely on data.table automatically appending p,Tn,a,b,c,sdLnA from "by=..."
    # to avoid duplicates. So we do NOT explicitly store p=Tn=... inside the data.table(...) call.
    UHS_in <- SaTRmodel[
      ,
      {
        if (is.na(a) || is.na(b) || is.na(c)) {
          return(data.table())
        }
        Sa_calc <- exp(a + b*log(TRo) + c*(1/TRo))
        data.table(
          IT  = ITo_val,
          TR  = TRo,
          Sa  = Sa_calc,
          AEP = 1/TRo,
          POE = 1 - exp(-ITo_val*(1/TRo))
          # p, Tn, a, b, c, sdLnA come from the grouping, no need to re-add them
        )
      },
      by = .(p,Tn,a,b,c,sdLnA)  # grouping
    ]

    # Now remove the param columns
    dropCols <- c("a","b","c","sdLnA")
    dropCols <- intersect(dropCols, names(UHS_in))
    if (length(dropCols)) {
      UHS_in[, (dropCols) := NULL]
    }

    # Also check for any accidental duplicate columns (p, p.1, etc.)
    # This is a safe check to remove suffix .1 columns
    dupCols <- grep("\\.1$", names(UHS_in), value=TRUE)
    if (length(dupCols)) UHS_in[, (dupCols) := NULL]

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
  # AEP_in => keep "ITo" for old code
  setnames(AEP_in, old="IT", new="ITo", skip_absent=TRUE)
  AEP_in[, `:=`(ID=IDo, AF=1, Vref=vref)]

  # (4) define PGA from Tn== Tn_PGA (vectorized ifelse)
  message("> define PGA from Tn_PGA=", Tn_PGA)
  if (nrow(UHS_in)) {
    UHS_in[, PGA := ifelse(Tn == Tn_PGA, Sa, NA_real_)]
  }
  if (nrow(AEP_in)) {
    AEP_in[, PGA := ifelse(Tn == Tn_PGA, Sa, NA_real_)]
  }

  ##### (5) Rbind site amp => no merges
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
    for (Vs in vs30) {
      tmpUHS <- copy(UHS_in)
      tmpAEP <- copy(AEP_in)

      # site amp => no merges => row-by-row logic
      if (nrow(tmpUHS)) {
        AUXu <- tmpUHS[
          ,
          fitModel.AF.TR(.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs, vref=vref),
          by=.(p,Tn)
        ]
        AFmodel_UHS <- rbind(AFmodel_UHS, AUXu, fill=TRUE)
        setorder(tmpUHS, p,Tn)
        setorder(AUXu, p,Tn)
        if (nrow(tmpUHS) == nrow(AUXu)) {
          tmpUHS[, Sa     := Sa * AUXu$AF]
          tmpUHS[, PGA    := PGA * AUXu$AF]
          tmpUHS[, AF     := AUXu$AF]
          tmpUHS[, sdLnAF := AUXu$sdLnAF]
          tmpUHS[, Vs30   := Vs]
        } else {
          warning("UHSTable row mismatch => site amp skip.")
        }
      }
      if (nrow(tmpAEP)) {
        AUXa <- tmpAEP[
          ,
          fitModel.AF.TR(.SD, pga=PGA, q=AF_q_TARGET, Tn=Tn, vs30=Vs, vref=vref),
          by=.(p,Tn)
        ]
        AFmodel_AEP <- rbind(AFmodel_AEP, AUXa, fill=TRUE)
        setorder(tmpAEP, p,Tn)
        setorder(AUXa,  p,Tn)
        if (nrow(tmpAEP)==nrow(AUXa)) {
          tmpAEP[, Sa     := Sa * AUXa$AF]
          tmpAEP[, PGA    := PGA * AUXa$AF]
          tmpAEP[, AF     := AUXa$AF]
          tmpAEP[, sdLnAF := AUXa$sdLnAF]
          tmpAEP[, Vs30   := Vs]
        } else {
          warning("AEP row mismatch => site amp skip.")
        }
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
