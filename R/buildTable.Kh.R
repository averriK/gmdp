#' Build Seismic Coefficient Table (Kmax) in g
#'
#' @param x GMDP object
#' @param Tso Ts in seconds
#' @param Dao Da in cm
#' @param size font size
#' @param po quantiles
#' @param engine c("flextable")
#' @param TRo return periods in years
#' @param tagUnits boolean
#'
#' @return Table
#' @export buildTable.Kh
#' @import data.table
#'
#' @examples
#'
#'

### Remover KmaxTable y DnTRTable y recalcularlas aqui para un set reducido de periodos

buildTable.Kh <- function(.x,Tso,Dao,Vs30o=760,size=12,po="mean",engine="flextable",TRo=c(500,1000,2500,5000,10000),tagUnits=FALSE){
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)

  # ****
  browser()
  UHSTable <- .x$UHSTable[TR %in% TRo && p %in% po][order(TR)]


  # ********************************************************************* ----
  # Newmark Displacements ----
  message(sprintf("> Building Newmark Displacements model"))
  DnTRmodel <- UHSTable[ fitModel.DnTR(x = .SD, Mw = 6.5, xD = 1.3, kymin = 0.005, kymax = 0.5, n = 30), by = .(Tn, TR, Vs30, p), .SDcols = colnames(UHSTable)]

  DnTRmodel[, SN := ID]
  message(sprintf("> Update UHSTable ..."))
  COLS <- colnames(DnTRmodel)[colnames(DnTRmodel) %in% colnames(UHSTable)]
  UHSTable <- UHSTable[DnTRmodel[, .(SN, p, Tn, Ts, TR, Dn, sdLnD, ky, Vs30, a, b, e)], on = COLS]
  UHSTable[, Dn_Unit := "cm"]
  UHSTable[, Ts_Unit := "s"]

  # ********************************************************************* ----
  # Pseudo-static coefficient ----

  message(sprintf("> Building pseudo-static coefficient model"))
  AUX <- UHSTable[, .(Tn, TR, p, Ts, Vs30, Vref, a, b, e, PGA)] |> unique()
  KmaxTable <- AUX[, fitModel.KmaxTR( x = .SD, n = 20), by = .(Tn, TR, p, Vs30, Vref), .SDcols = colnames(AUX)]
  rm(AUX)



  # ****
  # DT <- copy(x$KmaxTable)

  KmaxTable <- KmaxTable[,.(Ts,TR,p,Vs30,Vref,Da,Dmin,Dmax,PGA,Kh)] |> unique()


  # if(is.null(po)){
  #   DT <- DT[p == "mean"]
  # }
  # if(!is.null(po)){
  #   DT <- DT[p %in% po]
  # }

  # DT <- DT[TR %in% TRo][order(TR)]

  KmaxTable <- KmaxTable[,.predict.Kh(x=.SD,Tso=Tso,Dao=Dao),by=.(TR,p,Vs30),.SDcols=colnames(DT)]
  if(tagUnits==TRUE){
    data.table::setnames(
      KmaxTable,old=c("TR","Vs30","Kmax","PGA","Kh","Da","Ts"),new=c("TR[yr]","Vs30[m/s]","Kmax[g]","PGA[g]","Kh[%]","Da[cm]","Ts[s]"))
  }

  return(KmaxTable)
}


.predict.Kh <- function(x,Tso,Dao){

  DATA <- x
  PGA <- x$PGA |> unique()
  # stopifnot(length(PGA)==1)
  if(length(PGA)>1){
    browser()
    stop("PGA must be unique")
  }

  # Check ranges Ts
  if(!(Tso<=max(DATA$Ts) & Tso>=min(DATA$Ts))){
    warning(sprintf("Ts = %f s is out of range",Tso))
    return(NULL)
  }

  # Check ranges Da
  if(!(Dao<=DATA$Dmax[1] & Dao>=DATA$Dmin[1])){
    warning(sprintf("Da = %f cm is out of model range",Dao))
    return(NULL)
  }


  y.rf <- randomForest::randomForest(Kh~Ts+Da,data=DATA,importance=FALSE,proximity=FALSE)
  yp <- stats::predict(y.rf,newdata=data.table(Ts=Tso,Da=Dao))
  # DT <- data.table::rbindlist(list(x,data.table::data.table(Ts=Tso,Da=Dao,Kmax=yp)),use.names = TRUE)
  return(data.table::data.table(Ts=Tso,Da=Dao,Kh=round(yp,digits=1),PGA=round(PGA,digits = 3),Kmax=round(yp*PGA/100,digits=3)))
}

