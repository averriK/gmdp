#' Build Seismic Coefficient Table (Kmax) in g
#'
#' @param .x UHSTable object
#' @param Tso numeric Ts in seconds
#' @param Dao numeric. Da in cm
#' @param Vs30o number
#' @param size font size
#' @param po quantiles
#' @param engine c("flextable")
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
buildTable.Kmax <- function(.x,Tso,Dao,Vs30o,size=12,po="mean",engine="flextable",TRo=c(500,1000,2500,5000,10000),tagUnits=FALSE){
  on.exit(expr = {rm(list = ls())  }, add = TRUE)
  . <- NULL
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

  DnTRmodel <-  UHSTable[,fitModel.Dn.TR(Sa=Sa,PGA=PGA,Tn=Tn,Mw = 6.5, xD = 1.3, kymin = 0.001, kymax = 0.55, n = 100), by = .(ID,Tn, TR,Vref,Vs30, p)] |> unique()

  message(sprintf("> Update UHSTable ..."))

  COLS <- colnames(DnTRmodel)[colnames(DnTRmodel) %in% colnames(UHSTable)]
  UHSTable <- UHSTable[DnTRmodel[, .(ID, p, Tn, Ts, TR, Dn, sdLnD, ky, Mw,Vs30, Vref,a, b, e)], on = COLS]
  UHSTable[, Dn_Unit := "cm"]
  UHSTable[, Ts_Unit := "s"]

  # ********************************************************************* ----
  # Pseudo-static coefficient ----

  message(sprintf("> Building pseudo-static coefficient model"))
  AUX <- UHSTable[, .(ID,Tn, TR, p, Ts, Vs30, Vref, a, b, e, PGA,PGAref)] |> unique()
  DT <- AUX[, fitModel.Kmax.TR( a=a,b=b,e=e,PGA=PGA,Ts=Ts, n = 100), by = .(ID,Tn, TR, p, Ts,Vs30,Vref)]

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


.predict.Kmax <- function(.SD,Tso,Dao){
  on.exit(expr = {rm(list = ls())  }, add = TRUE)

  . <- NULL
  .newdata <- data.table(Ts=Tso,Da=Dao)
  #
  .data <- .SD[,.(Kh,Ts,Da)]
  .model <- randomForest::randomForest(Kh~.,data=.data,importance=FALSE,proximity=FALSE)
  Kh <- predict(.model,newdata=.newdata) |> round(digits=2)

  #
  .data <- .SD[,.(Kmax,Ts,Da)]
  .model <- randomForest::randomForest(Kmax~.,data=.data,importance=FALSE,proximity=FALSE)
  Kmax <- predict(.model,newdata=.newdata) |> round(digits=3)
  PGAo <- (100*Kmax/Kh )|>  round(digits=3)
  DT <- data.table::data.table(Ts=Tso,Da=Dao,Kh=Kh,Kmax=Kmax,PGAo=PGAo)
  return(DT)
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

