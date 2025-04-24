#' @title Build Ground-Motion Data Products (AEP & UHS) from OpenQuake
#' @description
#' This is the patched version replicating the old code's final columns
#' with no fill=TRUE merges and guaranteed "PGA" from Tn=0.
#' @param path Folder with OQ outputs
#' @param vref Numeric reference Vs30
#' @param vs30 Numeric vector for site amplification
#' @param IDo Character label
#' @param param Logical (if TRUE => param expansions)
#' @param quantile_AF Amp quantile
#' @param TRmin,TRmax Param expansions range
#' @return list(AEPTable, UHSTable, SaTRmodel, AFmodel_AEP, AFmodel_UHS)
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
  # 1) Import AEP + UHS
  AEP_in <- importModel.oqAEP(path=path, vref=vref)  # => has "ITo" for time
  UHS_in <- importModel.oqUHS(path=path)            # => has "IT"

  # Tag them
  AEP_in[, `:=`(ID=IDo, AF=1, Vref=vref)]
  UHS_in[, `:=`(ID=IDo, AF=1, Vref=vref)]

  # 2) If param=TRUE => expansions
  SaTRmodel <- data.table()
  if (param && nrow(AEP_in)) {
    groupCols <- intersect(c("lat","lon","depth","p","Tn"), names(AEP_in))
    # fit (a,b,c)
    SaTRmodel <- AEP_in[, fitModel.Sa.TR(.SD, TRmin=TRmin, TRmax=TRmax), by=groupCols]
    if (nrow(SaTRmodel)) {
      newHaz <- buildParamHaz(SaTRmodel, AEP_in) # => => ITo
      if (nrow(newHaz)) {
        newHaz[, `:=`(ID=IDo, AF=1, Vref=vref)]
        # rbind with no fill=TRUE => must unify columns
        # but let's do a small unify step:
        unifyCols(AEP_in, newHaz, "AEP")
        AEP_in <- rbind(AEP_in, newHaz)
      }
      newUHS <- buildParamUHS(SaTRmodel, AEP_in)
      if (nrow(newUHS)) {
        newUHS[, `:=`(ID=IDo, AF=1, Vref=vref)]
        unifyCols(UHS_in, newUHS, "UHS")
        UHS_in <- rbind(UHS_in, newUHS)
      }
    }
  }

  # 3) Site amp or not
  AFmodel_AEP <- data.table()
  AFmodel_UHS <- data.table()

  doAmp <- (!is.null(vs30) && length(vs30)>0 && vref %in% c(760,3000))

  if (!doAmp) {
    # no site amp => just rename columns properly
    AEPTable <- copy(AEP_in)
    UHSTable <- copy(UHS_in)
  } else {
    # apply site amp for each vs30
    finalAEP <- data.table()
    finalUHS <- data.table()

    for (Vs in vs30) {
      tmpAEP <- copy(AEP_in)
      tmpUHS <- copy(UHS_in)

      # site amp
      AF_AEP <- applySiteAmp(tmpAEP, vs30=Vs, vref=vref, quantile_AF=quantile_AF)
      if (nrow(AF_AEP)) {
        tmpAEP <- mergeAF(tmpAEP, AF_AEP)
        AFmodel_AEP <- rbind(AFmodel_AEP, AF_AEP)
      } else {
        tmpAEP[, `:=`(AF=1, sdLnAF=0)]
      }
      tmpAEP[, Vs30 := Vs]

      AF_UHS <- applySiteAmp(tmpUHS, vs30=Vs, vref=vref, quantile_AF=quantile_AF)
      if (nrow(AF_UHS)) {
        tmpUHS <- mergeAF(tmpUHS, AF_UHS)
        AFmodel_UHS <- rbind(AFmodel_UHS, AF_UHS)
      } else {
        tmpUHS[, `:=`(AF=1, sdLnAF=0)]
      }
      tmpUHS[, Vs30 := Vs]

      finalAEP <- rbind(finalAEP, tmpAEP)
      finalUHS <- rbind(finalUHS, tmpUHS)
    }

    AEPTable <- finalAEP
    UHSTable <- finalUHS
  }

  # 4) Ensure Tn=0 => PGA in both AEP & UHS
  #    unify "IT" vs. "ITo" => old code used "ITo" in AEP, "IT" in UHS
  #    remove any leftover muI, muL, muNL, etc.

  # 4a) AEP => rename "IT => ITo" if needed
  if ("IT" %in% names(AEPTable) && !"ITo" %in% names(AEPTable)) {
    setnames(AEPTable, "IT","ITo")
  }
  # define Tn=0 => PGA
  AEPTable <- inventTn0_PGA(AEPTable, "AEP")  # sets PGA from row Tn=0

  # remove extra columns
  dropCols <- c("muI","muL","muNL","muLnPGA","sdL","sdI","sdNL")
  dropCols <- intersect(dropCols, names(AEPTable))
  if (length(dropCols)) AEPTable[, (dropCols) := NULL]

  # 4b) UHS => rename "ITo => IT" if needed
  if ("ITo" %in% names(UHSTable) && !"IT" %in% names(UHSTable)) {
    setnames(UHSTable, "ITo","IT")
  }
  UHSTable <- inventTn0_PGA(UHSTable, "UHS")
  dropCols <- intersect(dropCols, names(UHSTable))
  if (length(dropCols)) UHSTable[, (dropCols) := NULL]

  # 5) unify final columns => replicate old code
  AEPTable <- finalizeAEP_old(AEPTable, vref)
  UHSTable <- finalizeUHS_old(UHSTable, vref)

  return(list(
    AEPTable    = AEPTable,
    UHSTable    = UHSTable,
    SaTRmodel   = SaTRmodel,
    AFmodel_AEP = AFmodel_AEP,
    AFmodel_UHS = AFmodel_UHS
  ))
}

##############################################################################
# Extra helpers used by buildGMDP
##############################################################################

# unifyCols: ensures 2 DTs share same columns (no fill=TRUE needed)
unifyCols <- function(dt1, dt2, mode=c("AEP","UHS")) {
  # minimal approach: take colnames intersection or union
  allC <- union(names(dt1), names(dt2))
  missing1 <- setdiff(allC, names(dt1))
  missing2 <- setdiff(allC, names(dt2))
  for (m in missing1) dt1[, (m) := NA]
  for (m in missing2) dt2[, (m) := NA]
  # drop extraneous meltdown columns if any
  # we won't reorder here. We'll reorder at the end
}

# inventTn0_PGA: ensures Tn=0 row & PGA in "AEP" or "UHS"
inventTn0_PGA <- function(DT, mode=c("AEP","UHS")) {
  mode <- match.arg(mode, c("AEP","UHS"))
  groupC <- c("lat","lon","depth","p","ID")
  groupC <- intersect(groupC, names(DT))

  DT <- DT[
    ,
    {
      chunk <- copy(.SD)
      validTn <- chunk$Tn[chunk$Tn >= 0]
      if (length(validTn)) {
        mnTn <- min(validTn)
        if (!any(chunk$Tn==0)) {
          i_min <- which.min(abs(chunk$Tn - mnTn))
          row_min <- copy(chunk[i_min])
          row_min[, Tn := 0]
          chunk <- rbind(chunk, row_min)
        }
      }
      chunk
    },
    by=groupC
  ]
  DT[, PGA := {
    val0 <- Sa[Tn==0]
    if (length(val0)==1L) val0 else 0
  }, by=groupC]

  return(DT)
}

# finalizeUHS_old => replicate old code columns
finalizeUHS_old <- function(DT, vref) {
  wanted <- c(
    "Vref","Vs30","lat","lon","depth","p","Tn","AF","SID","SM","PGA",
    "sdLnAF","TR","ID","IT","POE","Sa","AEP"
  )
  dropC <- setdiff(names(DT), wanted)
  if (length(dropC)) DT[, (dropC) := NULL]
  for (colN in wanted) {
    if (!colN %in% names(DT)) {
      if (colN=="Vref") DT[, Vref := vref]
      else if (colN=="AF") DT[, AF := 1]
      else if (colN=="sdLnAF") DT[, sdLnAF := NA_real_]
      else if (colN=="IT") DT[, IT := 50]
      else if (colN=="PGA"||colN=="Sa"||colN=="AEP"||colN=="POE"||colN=="TR") DT[, (colN):=0]
      else if (colN=="Vs30") DT[, Vs30 := vref]
      else if (colN=="SM") DT[, SM := "openquake"]
      else if (colN=="SID") DT[, SID := ""] # or Vs30toSID(Vs30) if you want
      else DT[, (colN):=NA]
    }
  }
  data.table::setcolorder(DT, wanted)
  # ensure no NA in key columns except sdLnAF
  DT[is.na(AF), AF:=1]
  DT[is.na(Vs30), Vs30:=vref]
  DT[is.na(PGA), PGA:=0]
  return(DT[])
}

# finalizeAEP_old => replicate old code columns
finalizeAEP_old <- function(DT, vref) {
  wanted <- c(
    "Vref","Vs30","lat","lon","depth","p","Tn","AF","SID","SM","PGA",
    "sdLnAF","Sa","ID","ITo","POE","AEP","TR"
  )
  dropC <- setdiff(names(DT), wanted)
  if (length(dropC)) DT[, (dropC) := NULL]
  for (colN in wanted) {
    if (!colN %in% names(DT)) {
      if (colN=="Vref") DT[, Vref:=vref]
      else if (colN=="AF") DT[, AF:=1]
      else if (colN=="sdLnAF") DT[, sdLnAF:=NA_real_]
      else if (colN=="ITo") DT[, ITo:=50]
      else if (colN=="PGA"||colN=="Sa"||colN=="AEP"||colN=="POE"||colN=="TR") DT[, (colN):=0]
      else if (colN=="Vs30") DT[, Vs30:=vref]
      else if (colN=="SM") DT[, SM:="openquake"]
      else if (colN=="SID") DT[, SID:=""]
      else DT[, (colN):=NA]
    }
  }
  data.table::setcolorder(DT, wanted)
  DT[is.na(AF), AF:=1]
  DT[is.na(Vs30), Vs30:=vref]
  DT[is.na(PGA), PGA:=0]
  return(DT[])
}
