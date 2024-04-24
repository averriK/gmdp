#' Title
#'
#' @param Hmax Double. Height in m
#' @param lo Double. Truncation ratio
#' @param Bmax Double. Maximum width in m
#' @param Bo Double. TSF Crest in m
#' @param material Data frame. New data to predict
#' @param level Double. Confidence level (0-1)
#' @param s Double. Slope S=1/tan(beta)
#' @param regression String. Regression method
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
fitModel.Td <- function(Hmax,lo=NULL,Bmax=NULL,Bo=NULL,s=NULL,material,level=0.5,regression="qrf"){
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

  INTERP <- FALSE
  if(all(lo<=max(CylinderRoots[n==1]$l) & lo>=min(CylinderRoots[n==1]$l) & mo<=max(CylinderRoots[n==1]$m) & mo>=min(CylinderRoots[n==1]$m))){
    INTERP <- TRUE
  }
  #check lambda con CylinderRoots
  DATA <- data.table(Hs=Hmax,material,l=lo)

  Go <- fitModel(.data = SiteTable, y="Go",.newdata=DATA,level=level,regression=regression)
  VSo <- fitModel(.data = SiteTable, y="VSo",.newdata=DATA,level=level,regression=regression)
  mo <- fitModel(.data = SiteTable, y="mo",.newdata=DATA,level=level,regression=regression)
  DATA <- CylinderRoots[n==1,.(m,l,an)]
  # browser()
  # if(INTERP==TRUE){
  #   with(DATA, akima::interp(x = l, y = m, z = an, xo = lo, yo = mo[1]))
  # }
  an <- fitModel(.data=CylinderRoots[n==1,.(m,l,an)], y="an",.newdata=data.table(m=mo,l=lo),level=level,regression=regression)
  Ts <- (4*pi*Hmax/(an*(2 - mo)*VSo))
  return(Ts)


}
