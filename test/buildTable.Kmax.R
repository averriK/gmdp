#' Build Seismic Coefficient Table (Kmax) in g
#'
#' @param .x UHSTable object
#' @param Tso numeric Ts in seconds
#' @param Dao numeric. Da in cm
#' @param Vs30o number
#' @param size font size
#' @param po quantiles
#' @param TRo return periods in years
#' @param tagUnits boolean
#'
#' @return Table
#' @export buildTable.Kmax
#' @import data.table
#' @importFrom stats predict
#'
#' @examples
#'
#'
buildTable.Kmax <- function(.x,Td,Da,size=12,po="mean",tagUnits=FALSE){
  on.exit(expr = {rm(list = ls())  }, add = TRUE)
  . <- NULL

  # ********************************************************************* ----
  # Pseudo-static coefficient ----

  message(sprintf("> Building pseudo-static coefficient model"))
  AUX <- UHSTable[, .(ID,Tn, TR, p, Ts, Vs30, Vref, a, b, e, PGA,PGAref)] |> unique()
  DT <- AUX[, fitModel.Kmax.TR( a=a,b=b,e=e,pga=PGA,Ts=Ts, n = 100), by = .(ID,Tn, TR, p, Ts,Vs30,Vref)]

  # KmaxTable <- KmaxTable[,.(Ts,TR,p,Vs30,Vref,Da,Dmin,Dmax,PGA,Kh)] |> unique()
  # Check ranges Ts

  if(!(min(Tso)<=max(DT$Ts) & max(Tso)>=min(DT$Ts))){
    warning(sprintf("Ts = %f s is out of range",min(Tso)))
    return(NULL)
  }

  # Check ranges Da
  if(!(min(Dao)<=max(DT$Dmax) & max(Dao)>=min(DT$Dmin))){
    warning(sprintf("Da = %f cm is out of model range",max(Dao)))
    return(NULL)
  }

  KmaxTable <- DT[,.predict.Kmax(.SD,Tso,Dao),by=.(TR,p,Vs30),.SDcols=colnames(DT)]
  if(tagUnits==TRUE){
    data.table::setnames(
      KmaxTable,old=c("TR","Vs30","Kmax","PGA","Kh","Da","Ts"),new=c("TR[yr]","Vs30[m/s]","Kmax[g]","PGA[g]","Kh[%]","Da[cm]","Ts[s]"))
  }

  return(KmaxTable)
}



