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


buildTable.Kh <- function(x,Tso,Dao,size=12,po=NULL,engine="flextable",TRo=c(500,1000,2500,5000,10000),tagUnits=FALSE){
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)

  DT <- copy(x$KmaxTable)

  DT <- DT[,list(Ts,TR,p,Vs30,Vref,Da,Dmin,Dmax,PGA,Kh)] |> unique()


  if(is.null(po)){
    DT <- DT[p == "mean"]
  }
  if(!is.null(po)){
    DT <- DT[p %in% po]
  }

  DT <- DT[TR %in% TRo][order(TR)]

  # Check final rows
  if(nrow(DT)>0){
    # Predict Ranges
    DT <- DT[,.predict.Kh(x=.SD,Tso=Tso,Dao=Dao),by=.(TR,p,Vs30),.SDcols=colnames(DT)]
    if(tagUnits==TRUE){
      data.table::setnames(DT,old=c("TR"),new=c("TR[yr]"))
      data.table::setnames(DT,old=c("Vs30"),new=c("Vs30[m/s]"))
      data.table::setnames(DT,old=c("Kmax"),new=c("Kmax[g]"))
      data.table::setnames(DT,old=c("PGA"),new=c("PGA[g]"))
      data.table::setnames(DT,old=c("Kh"),new=c("Kh[%]"))
      data.table::setnames(DT,old=c("Da"),new=c("Da[cm]"))
      data.table::setnames(DT,old=c("Ts"),new=c("Ts[s]"))
    }

  } else {
    message(sprintf("Table with %d rows",nrow(DT)))
  }
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

