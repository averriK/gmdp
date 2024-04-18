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
buildTable.Kmax <- function(.x,Tso,Dao,Vs30o,size=12,po="mean",engine="flextable",TRo=c(500,1000,2500,5000,10000),tagUnits=FALSE){
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)

  # ****
  # browser()

  Vs30o <- .find(V=unique(.x$Vs30) , X=Vs30o)
  TRo <- .find(V=unique(.x$TR) , X=TRo)
  po <- .find(V=unique(.x$p) , X=po)

  # control TRo

  UHSTable <- .x[TR %in% TRo & p %in% po & Vs30 %in% Vs30o][order(TR)]


  # ********************************************************************* ----
  # Newmark Displacements ----
  message(sprintf("> Building Newmark Displacements model"))

  DnTRmodel <-  UHSTable[,fitModel.DnTR(Sa=Sa,PGA=PGA,Tn=Tn,Mw = 6.5, xD = 1.3, kymin = 0.001, kymax = 0.55, n = 100), by = .(ID,Tn, TR,Vref,Vs30, p)] |> unique()

  message(sprintf("> Update UHSTable ..."))

  COLS <- colnames(DnTRmodel)[colnames(DnTRmodel) %in% colnames(UHSTable)]
  UHSTable <- UHSTable[DnTRmodel[, .(ID, p, Tn, Ts, TR, Dn, sdLnD, ky, Mw,Vs30, Vref,a, b, e)], on = COLS]
  UHSTable[, Dn_Unit := "cm"]
  UHSTable[, Ts_Unit := "s"]

  # ********************************************************************* ----
  # Pseudo-static coefficient ----
  browser()
  message(sprintf("> Building pseudo-static coefficient model"))
  AUX <- UHSTable[, .(ID,Tn, TR, p, Ts, Vs30, Vref, a, b, e, PGA,PGAref)] |> unique()
  KmaxTable <- AUX[, fitModel.KmaxTR( a=a,b=b,e=e,PGA=PGA,Ts=Ts, n = 100), by = .(ID,Tn, TR, p, Ts,Vs30,Vref), .SDcols = colnames(AUX)]

  # KmaxTable <- KmaxTable[,.(Ts,TR,p,Vs30,Vref,Da,Dmin,Dmax,PGA,Kh)] |> unique()

  KmaxTable <- KmaxTable[,.predict.Kh(x=.SD,Tso=Tso,Dao=Dao),by=.(TR,p,Vs30),.SDcols=colnames(KmaxTable)]
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


.find <- function(V, X) {
  # Ensure V is sorted
  V <- sort(unique(V))
  X <- sort(unique(X))
  # Initialize the result vector
  results <- numeric(length(X))

  # Process each element in X
  for (i in seq_along(X)) {
    x <- X[i]

    # Check if x is exactly in V
    if (x %in% V) {
      results[i] <- x
    } else {
      # Find the closest indices before and after x
      idx_before <- max(which(V < x), na.rm = TRUE)
      idx_after <- min(which(V > x), na.rm = TRUE)

      # Handle edge cases when x is outside the range of V
      if (length(idx_before) == 0) { # x is less than all elements in V
        idx_before <- idx_after
      }
      if (length(idx_after) == 0) { # x is more than all elements in V
        idx_after <- idx_before
      }

      # Fetch the closest values
      # Choose the closer one or both if x is equally distant from V[idx_before] and V[idx_after]
      if (abs(V[idx_before] - x) < abs(V[idx_after] - x)) {
        results[i] <- V[idx_before]
      } else if (abs(V[idx_before] - x) > abs(V[idx_after] - x)) {
        results[i] <- V[idx_after]
      } else {
        # If both are equally close, return the average or any other logic you want
        results[i] <- mean(c(V[idx_before], V[idx_after]))
      }
    }
  }

  return(results)
}

