#' Title
#'
#' @param size Font Size
#' @param x GMDP object
#' @param engine c("flextable","kableExtra")
#' @param po c("mean",0.16,0.50,0.84)
#' @param Vs30o Vs30 in m/s
#' @param SIDo Site Class
#' @param TRo Return Period in years
#'
#' @return Table
#' @import data.table
#' @export buildTable.PGA
#'
#' @examples

#'
buildTable.PGA <- function(x,size=12,po=c(0.16,0.50,0.84),engine="flextable",TRo=c(500,1000,2500,5000,10000),Vs30o=NULL,SIDo=NULL){
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  . <- NULL



  DT <- x$UHSTable[,list(TR,p,PGA,Vs30,Vref,SID)] |> unique()
  DT[,PGA:=round(PGA,digits=3)]


  if(!is.null(Vs30o) ){
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

  data.table::setnames(DT,old=c("Vs30"),new=c("Vs30[m/s]"))
  data.table::setnames(DT,old=c("TR","PGA"),new=c("TR[yr]","PGA[g]"))

  return(DT)
}


.predict.Sa <- function(x,Tno,Vs30o){
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
  yp <- stats::predict(y.rf,newdata=data.table(Tn=Tno,Vs30=Dno))
  return(data.table::data.table(Tn=Tno,Vs30=Vs30o,Sa=round(yp,digits=3)))
}
