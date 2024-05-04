#' Title
#'
#' @param size Font size
#' @param x GMDP object
#' @param engine c("flextable","kableExtra")
#' @param po c("mean",0.16,0.50,0.84)
#' @param Vs30o Vs30 in m/s
#' @param TRo Return Period in years
#' @param Tno Period in seconds
#' @param tagUnits boolean
#'
#' @return Table
#' @export buildTable.Sa
#' @import data.table
#' @examples
#'
buildTable.Sa <- function(x,Tno,TRo,size=12,po=NULL,engine="flextable",Vs30o=NULL,tagUnits=TRUE){
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)

  DT <- x[,.(Tn,TR,p,Sa,Vs30,Vref,SID)]

  . <- NULL
  # browser()

  # Check Vs30o

  if(is.null(Vs30o) ){
    Vref <- DT[Vs30==Vref]$Vs30 |> unique()
    DT <-DT[Vs30==Vref[1],-c("Vref")]
  }

  if(!is.null(Vs30o) & all(Vs30o %in% unique(DT$Vs30))){
    DT <- DT[Vs30 %in% Vs30o,-c("Vref")]

    if(tagUnits==TRUE){
      data.table::setnames(DT,old=c("Vs30"),new=c("Vs30[m/s]"))
    }
  }

  # Check po
  if(is.null(po)){
    DT <- DT[p == "mean",-c("p")]
  }
  if(!is.null(po)  & all(po %in% unique(DT$p))){
    DT <- DT[p %in% po]
  }


  # Check Tno
  if( all(Tno %in% unique(DT$Tn))){
    DT <- DT[Tn %in% Tno]
    if(tagUnits==TRUE){
      data.table::setnames(DT,old=c("Tn"),new=c("Tn[s]"))
    }
  }

  # Check TRo


  if(all(TRo %in% unique(DT$TR))){
    DT <- DT[TR %in% TRo]
    if(tagUnits==TRUE){
      data.table::setnames(DT,old=c("TR"),new=c("TR[yr]"))
    }
  }

  # Rounding
  DT[,Sa := round(Sa,3)]
  if(tagUnits==FALSE & all(DT$Tn==0) ){data.table::setnames(DT,old="Sa",new="PGA")}
  if(tagUnits==TRUE){
    if(all(DT$Tn==0)){
      data.table::setnames(DT,old=c("Sa"),new=c("PGA[g]"))
    } else {
      data.table::setnames(DT,old=c("Sa"),new=c("Sa[g]"))
    }
  }
  return(DT[])
}

.predict.Sa <- function(x,Tno,Vs30o,TRo){
  DATA <- x
  # Check ranges Ts
  if(!(Tno<=max(DATA$Tn) & Tno>=min(DATA$Tn))){
    warning(sprintf("Tn = %g s is outside the model range",Tno))
    return(NULL)
  }

  # Check ranges Da
  if(!(Vs30o<=max(DATA$Vs30) & Vs30o>=min(DATA$Vs30))){
    warning(sprintf("Vs30o = %f m/s is outside the model range",Vs30o))
    return(NULL)
  }

  y.rf <- randomForest::randomForest(Sa~Tn+Vs30,data=DATA,importance=FALSE,proximity=FALSE)
  yp <- stats::predict(y.rf,newdata=data.table(Tn=Tno,Vs30=Vs30o))
  return(data.table::data.table(Tn=Tno,Vs30=Vs30o,Sa=yp))
}
