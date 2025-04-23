#' @title Build Ground-Motion Data Products
#'
#' @description
#' Reads OpenQuake outputs (hazard curves, direct UHS, disagg if found),
#' optionally fits a parametric Sa-TR model, optionally applies site amplification,
#' and returns final tables with minimal ephemeral variables.
#'
#' @param path   Character. Path to folder containing OQ .zip outputs.
#' @param vref   Numeric. Reference Vs30 (e.g. 760). If site amp is desired, must be 760 or 3000.
#' @param vs30   Numeric vector of target site classes. If non-empty and \code{vref \%in\% c(760,3000)},
#'   site amplification is applied.
#' @param IDo    Character. A scenario ID label (default "gmdp").
#' @param param  Logical. If TRUE, fit parametric model and produce new/smoothed hazard + UHS.
#' @param quantile_AF Character or numeric. Which quantile or "mean" for site amp.
#' @param TRmin Numeric. Min TR bound for param fitting.
#' @param TRmax Numeric. Max TR bound for param fitting.
#'
#' @return A list with:
#' \itemize{
#'   \item \strong{AEPTable}: hazard data (original + param-based if any) with site amp if applied
#'   \item \strong{UHSTable}: UHS data (original + param-based) with site amp if applied
#'   \item \strong{RMwTable}: disaggregation data if found, else \code{NULL}
#'   \item \strong{SaTRmodel}: param model results (if \code{param=TRUE})
#'   \item \strong{AFmodel_AEP}, \strong{AFmodel_UHS}: site amp factors if any
#' }
#'
#' @import data.table
#' @export
buildGMDP <- function(path,
                      vref,
                      vs30          = NULL,
                      IDo           = "gmdp",
                      param         = FALSE,
                      quantile_AF   = "mean",
                      TRmin         = 100,
                      TRmax         = 10000) {
  # -------------------------------------------------------------------------
  # 1) Import AEP (hazard curves), UHS, and disagg from path

  AEPTable <- importModel.oqAEP(path = path, vref = vref)
  if (!nrow(AEPTable)) {
    stop("No hazard-curve data found in: ", path)
  }

  UHSTable <- importModel.oqUHS(path = path)
  if (!nrow(UHSTable)) {
    message("No direct UHS found in path: ", path)
    # We'll keep it empty for now, might fill param-based UHS later
  }

  RMwTable <- NULL
  if (exists("importModel.oqRMw", mode="function")) {
    # Try reading disagg
    # We'll get ITo from AEP if present, else default 50
    ITo <- if ("ITo" %in% names(AEPTable)) unique(AEPTable$ITo)[1] else 50
    RMwTable <- importModel.oqRMw(path, ITo, vref)
    if (is.null(RMwTable) || !nrow(RMwTable)) {
      RMwTable <- NULL
    }
  }

  # If no ITo in AEP, define 50
  if (!("ITo" %in% names(AEPTable))) {
    AEPTable[, ITo := 50]
  }

  # Mark original data
  AEPTable[, `:=`(paramBased=FALSE, siteAmp=FALSE, AF=1)]
  if (nrow(UHSTable)) {
    if (!("ITo" %in% names(UHSTable))) {
      UHSTable[, ITo := unique(AEPTable$ITo)[1]]
    }
    UHSTable[, `:=`(paramBased=FALSE, siteAmp=FALSE, AF=1)]
  }

  # -------------------------------------------------------------------------
  # 2) (Optional) Fit param model on hazard curves => generate param-based curves
  SaTRmodel <- data.table()
  if (param) {
    message("> Fitting parametric model on hazard curves ...")
    groupCols <- intersect(c("lat","lon","depth","p","Tn"), names(AEPTable))
    SaTRmodel <- AEPTable[
      ,
      fitModel.Sa.TR(.SD, TRmin=TRmin, TRmax=TRmax),
      by=groupCols
    ]

    if (!is.null(SaTRmodel) && nrow(SaTRmodel)) {
      # Build param-based hazard expansions
      newHaz <- buildParamHaz(SaTRmodel, AEPTable)
      # Mark them paramBased=TRUE
      newHaz[, `:=`(paramBased=TRUE, siteAmp=FALSE, AF=1)]
      AEPTable <- rbind(AEPTable, newHaz, fill=TRUE)

      # Also build param-based UHS
      newUHS <- buildParamUHS(SaTRmodel, AEPTable)
      if (nrow(newUHS)) {
        newUHS[, `:=`(paramBased=TRUE, siteAmp=FALSE, AF=1)]
        UHSTable <- rbind(UHSTable, newUHS, fill=TRUE)
      }
    } else {
      message("Param model returned 0 rows => no smoothing generated.")
      SaTRmodel <- data.table()
    }
  } else {
    message("> param=FALSE => skipping param-based steps.")
  }

  # -------------------------------------------------------------------------
  # 3) (Optional) Site Amplification
  AFmodel_AEP <- data.table()
  AFmodel_UHS <- data.table()

  doAmp <- (!is.null(vs30) && length(vs30)>0 && vref %in% c(760,3000))
  if (doAmp) {
    message("> Performing site amplification for vs30=", paste(vs30, collapse=", "))
    # Amplify hazard
    AFmodel_AEP <- applySiteAmp(AEPTable, vs30, vref, quantile_AF)
    if (nrow(AFmodel_AEP)) {
      # Merge AF to AEPTable
      AEPTable <- mergeAF(AEPTable, AFmodel_AEP)
    }

    # Amplify UHS
    if (nrow(UHSTable)) {
      AFmodel_UHS <- applySiteAmp(UHSTable, vs30, vref, quantile_AF)
      if (nrow(AFmodel_UHS)) {
        UHSTable <- mergeAF(UHSTable, AFmodel_UHS)
      }
    }
  } else {
    message("> No site amplification done.")
  }

  # Final labeling
  AEPTable[, `:=`(ID=IDo, Vref=vref)]
  UHSTable[, `:=`(ID=IDo, Vref=vref)]
  if (!is.null(RMwTable)) {
    RMwTable[, `:=`(ID=IDo, Vref=vref)]
  }

  # -------------------------------------------------------------------------
  # Return
  return(list(
    AEPTable     = AEPTable,
    UHSTable     = UHSTable,
    RMwTable     = RMwTable,
    SaTRmodel    = SaTRmodel,
    AFmodel_AEP  = AFmodel_AEP,
    AFmodel_UHS  = AFmodel_UHS
  ))
}


