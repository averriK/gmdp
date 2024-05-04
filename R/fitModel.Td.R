#' Title
#'
#' @param .geometry Data frame
#' @param .material Data frame
#' @param level Double. Confidence level (0-1)
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
fitModel.Td <- function(.geometry,.material,level="mean",regression="qrf"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  . <- NULL
  GVARS <- names(.geometry)
  MVARS <- names(.material)
  lo <- NULL
  if(!(all(c("VSo","mo") %in% MVARS))){
    stop("Invalid .material data.table")
  }
  if(!(any(c("B","b","lo","s")%in% GVARS))){
  # if(is.null(B) && is.null(b) && is.null(lo) && is.null(s)){
    stop("At least one of the following parameters must be defined: B, b, lo,S")
  }

  if("lo" %in% GVARS){
    lo <- .geometry$lo
  }

  if(!("lo"%in% GVARS) & all(c("B","b") %in% GVARS)){
  # if(is.null(lo) && !is.null(B) && !is.null(b)){
    b <- .geometry$b
    B <- .geometry$B
    lo <- b/B
  }


  if(!("lo"%in% GVARS) & all(c("s","b") %in% GVARS)){
  # if(is.null(lo) && !is.null(s) && !is.null(b)){
    Hs <- .geometry$Hs
    b <- .geometry$b
    s <- .geometry$s
    B <- 2*Hs*s+b
    lo <- b/B
  }

  stopifnot(!is.null(lo))

  DATA <- CylinderRoots[n==1,.(m,l,an)]
  mo <- .material$mo
  .newdata <- data.table(m=mo,l=lo)
  an <- fitModel(.data=CylinderRoots[n==1,.(m,l,an)], y="an",.newdata=.newdata,level=level,regression="lm")

  # Build geometries and materials scenarios
  TS <- data.table(.material[,.(VSo,mo)],Hs=.geometry$Hs,an,level=level)
  TS[,Ts:=(4*pi*Hs/(an*(2 - mo)*VSo))]

  return(TS[])

}
