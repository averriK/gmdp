#' @title Build Ground-Motion Data Products (AEP & UHS) from OpenQuake
#'
#' @description
#' Reads hazard (AEP) and UHS CSV/zip from OpenQuake in \code{path}, optionally
#' does param expansions (\code{param=TRUE}), and applies site amplification
#' for each vs30 in \code{vs30} if \code{vref \%in\% c(760,3000)}.
#' Returns final \code{AEPTable}, \code{UHSTable}, \code{SaTRmodel},
#' and site-amp info.
#'
#' @param path Character. Folder with OQ outputs
#' @param vref Numeric reference Vs30 used in OQ
#' @param vs30 Numeric vector. If given, site amp is done for each Vs30
#' @param IDo Character label (ID) in final tables
#' @param param Logical. If \code{TRUE}, fit param expansions from hazard
#' @param quantile_AF Site amp quantile factor
#' @param TRmin,TRmax Param expansion bounds
#'
#' @return A list with \code{(AEPTable, UHSTable, SaTRmodel, AFmodel_AEP, AFmodel_UHS)}
#' @import data.table
#' @export
buildGMDP <- function(path,
                      vref,
                      vs30         = NULL,
                      IDo          = "gmdp",
                      param        = FALSE,
                      quantile_AF  = "mean",
                      TRmin        = 100,
                      TRmax        = 10000)
{
  # 1) Import AEP + UHS from OQ (already "IT" not "ITo")
  AEP_in <- importModel.oqAEP(path, vref)
  UHS_in <- importModel.oqUHS(path)

  # Tag them
  AEP_in[, `:=`(ID=IDo, AF=1, Vref=vref)]
  UHS_in[, `:=`(ID=IDo, AF=1, Vref=vref)]

  # 2) Optional param expansions
  SaTRmodel <- data.table()
  if (param && nrow(AEP_in)) {
    groupCols <- intersect(c("lat","lon","depth","p","Tn"), names(AEP_in))
    SaTRmodel <- AEP_in[, fitModel.Sa.TR(.SD, TRmin=TRmin, TRmax=TRmax), by=groupCols]
    if (nrow(SaTRmodel)) {
      newHaz <- buildParamHaz(SaTRmodel, AEP_in)
      if (nrow(newHaz)) {
        newHaz[, `:=`(ID=IDo, AF=1, Vref=vref)]
        AEP_in <- rbind(AEP_in, newHaz, fill=TRUE)
      }
      newUHS <- buildParamUHS(SaTRmodel, AEP_in)
      if (nrow(newUHS)) {
        newUHS[, `:=`(ID=IDo, AF=1, Vref=vref)]
        UHS_in <- rbind(UHS_in, newUHS, fill=TRUE)
      }
    }
  }

  # 3) If vs30 is empty => no site amp
  if (is.null(vs30) || !length(vs30)) {
    # final tables are just the as-is combined
    return(list(
      AEPTable    = AEP_in,
      UHSTable    = UHS_in,
      SaTRmodel   = SaTRmodel,
      AFmodel_AEP = data.table(),
      AFmodel_UHS = data.table()
    ))
  }

  # 4) If site amp => must have vref=760 or 3000
  if (!(vref %in% c(760,3000))) {
    stop("Site amplification requires vref=760 or 3000, found: ", vref)
  }

  finalAEP <- data.table()
  finalUHS <- data.table()
  AFmodel_AEP <- data.table()
  AFmodel_UHS <- data.table()

  # For each Vs in vs30
  for (Vs in vs30) {
    tmpAEP <- copy(AEP_in)
    tmpUHS <- copy(UHS_in)

    # apply site amp
    AF_AEP <- applySiteAmp(tmpAEP, vs30=Vs, vref=vref, quantile_AF=quantile_AF)
    if (nrow(AF_AEP)) {
      tmpAEP <- mergeAF(tmpAEP, AF_AEP)
      AFmodel_AEP <- rbind(AFmodel_AEP, AF_AEP, fill=TRUE)
    }
    AF_UHS <- applySiteAmp(tmpUHS, vs30=Vs, vref=vref, quantile_AF=quantile_AF)
    if (nrow(AF_UHS)) {
      tmpUHS <- mergeAF(tmpUHS, AF_UHS)
      AFmodel_UHS <- rbind(AFmodel_UHS, AF_UHS, fill=TRUE)
    }

    # Possibly set Vs30=Vs if missing
    if (!"Vs30" %in% names(tmpAEP)) tmpAEP[, Vs30 := Vs]
    tmpAEP[is.na(Vs30), Vs30 := Vs]
    if (!"Vs30" %in% names(tmpUHS)) tmpUHS[, Vs30 := Vs]
    tmpUHS[is.na(Vs30), Vs30 := Vs]

    # Combine
    finalAEP <- rbind(finalAEP, tmpAEP, fill=TRUE)
    finalUHS <- rbind(finalUHS, tmpUHS, fill=TRUE)
  }

  return(list(
    AEPTable    = finalAEP,
    UHSTable    = finalUHS,
    SaTRmodel   = SaTRmodel,
    AFmodel_AEP = AFmodel_AEP,
    AFmodel_UHS = AFmodel_UHS
  ))
}
