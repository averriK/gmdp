#' Build Seismic Coefficient Table (Kmax) in g
#'
#' @param x GMDP object
#' @param Tso Ts in seconds
#' @param Dao Da in cm
#' @param size font size
#' @param po quantiles
#' @param engine c("flextable")
#' @param TRo return periods in years
#' @param Vs30o Vs30 in m/s
#' @param SIDo Site ID
#'
#' @return Table
#' @export
#'
#' @examples
#' @importFrom data.table setnames
#' @importFrom data.table data.table
buildTable.Kh <- function(x,Tso,Dao,size=12,po=c(0.16,0.50,0.84),engine="flextable",TRo=c(500,1000,2500,5000,10000),Vs30o=NULL,SIDo=NULL){
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)

  . <- .SD <- .N <- .I <- NULL

  # buildTable.Kmax(x=GMDP,Vs30o=760,Tso=0.4,Dao=1.0)
  DT <- x$KmaxTable
  # DT <- DT[,.(Ts,TR,p,Vs30,Vref,Da,Dmin,Dmax,Kh)] |> unique()
  DT <- DT[,.(Ts,TR,p,Vs30,Vref,Da,Dmin,Dmax,PGA,Kh)] |> unique()

  if(!is.null(Vs30o)){
    SIDo <- Vs30toSID(Vs30o)
    message(sprintf("Building Table for Vs30 %f m/s",Vs30o))
    DT <- DT[Vs30 %in% Vs30o,-c("Vref")]
  }

  if(!is.null(SIDo) & is.null(Vs30o)){
    message(sprintf("Building Table for SID %s",SIDo))
    DT <-DT[Vs30==Vref & SID %in% SIDo,-c("Vref")]
  }

  if(is.null(Vs30o) & is.null(SIDo)){
    Vs30o <- DT[Vs30==Vref]$Vs30 |> unique()
    DT <-DT[Vs30 %in% Vs30o,-c("Vref")]
  }




  if(is.null(po)){
    DT <- DT[p == "mean",-c("p")]
  }
  if(!is.null(po)){
    DT <- DT[p %in% po]
  }

  DT <- DT[TR %in% TRo][order(TR)]

  # Check final rows
  if(nrow(DT)==0){
    return(NULL)
  }


  # Predict Ranges

  DT <- DT[,.predict.Kh(x=.SD,Tso=Tso,Dao=Dao),by=.(TR,p,Vs30),.SDcols=colnames(DT)]
  data.table::setnames(DT,old=c("TR"),new=c("TR[yr]"))
  data.table::setnames(DT,old=c("Vs30"),new=c("Vs30[m/s]"))
  data.table::setnames(DT,old=c("Kmax"),new=c("Kmax[g]"))
  data.table::setnames(DT,old=c("PGA"),new=c("PGA[g]"))
  data.table::setnames(DT,old=c("Kh"),new=c("Kh[%]"))
  data.table::setnames(DT,old=c("Da"),new=c("Da[cm]"))
  data.table::setnames(DT,old=c("Ts"),new=c("Ts[s]"))

  return(DT)
}


.predict.Kh <- function(x,Tso,Dao){
  DATA <- x
  PGA <- x$PGA |> unique()
  stopifnot(length(PGA)==1)

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
#
# library(caret) |> suppressPackageStartupMessages()
# library(randomForest) |> suppressPackageStartupMessages()
# RF arroja mejores resultados con 10-folds (datasets mas chicos) que con 3-folds (dataset mas grandes)
# https://stats.stackexchange.com/questions/27730/choice-of-k-in-k-fold-cross-validation
# https://stats.stackexchange.com/questions/52274/how-to-choose-a-predictive-model-after-k-fold-cross-validation


