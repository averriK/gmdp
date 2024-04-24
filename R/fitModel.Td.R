#' Title
#'
#' @param Hmax Double. Height in m
#' @param lo Double. Truncation ratio
#' @param Bmax Double. Maximum width in m
#' @param Bo Double. TSF Crest in m
#' @param .newdata Data frame. New data to predict
#' @param .level Double. Confidence level (0-1)
#' @param s Double. Slope S=1/tan(beta)
#' @param .regression String. Regression method
#'
#' @return List
#' @export fitModel.Td
#'
#' @import data.table
#' @import quantregForest
#' @importFrom stats predict
#'
#' @examples
#'
fitModel.Td <- function(Hmax,lo=NULL,Bmax=NULL,Bo=NULL,s=NULL,.newdata,level=0.5,regression="qrf"){
  on.exit(expr={rm(list = ls())}, add = TRUE)

  if(is.null(Bmax) && is.null(Bo) && is.null(lo) && is.null(s)){
    stop("At least one of the following parameters must be defined: Bmax, Bo, lo,S")
  }

  if(is.null(lo) && !is.null(Bmax) && !is.null(Bo)){
    lo <- round(Bo/Bmax,digits=2)
  }
  if(is.null(lo) && !is.null(s) && !is.null(Bo)){
    Bmax <- 2*Hmax*s+Bo
    lo <- round(Bo/Bmax,digits=2)
  }

  #check lambda con CylinderRoots

  # Mean Values - temporary solution.
  # replace this code with simulation for all values and quantile estimation

  # GoF <- .fitModel.Go.F(Hso=Hmax,GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction,level=level)
  # VSoF <- fitModel.VSo.F(Hso=Hmax,GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction,level=level)
  # moF <- .fitModel.mo.F(Hso=Hmax,GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction,level=level)
  # an <- .fitModel.an(mo=moF[["mean"]],lo=lo,level=level)

  GoF <- fitModel(.data = SiteTable, y="Go",.newdata=.newdata,level=level,regression="qrf")
  VSoF <- fitModel(.data = SiteTable, y="VSo",.newdata=.newdata,level=level,regression="qrf")
  moF <- fitModel(.data = SiteTable, y="mo",.newdata=.newdata,level=level,regression="qrf")

  an <- fitModel(.data=CylinderRoots[n==1], y="an",.newdata=data.table(),level=level,regression="qrf")





  Ts <- (4*pi*Hmax/(an*(2 - moF[["mean"]])*VSoF[["mean"]]))
  return(list(Ts=round(Ts,3),Go=GoF,VSo=VSoF,mo=moF,an=an))


}
