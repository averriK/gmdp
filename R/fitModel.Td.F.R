#' Title
#'
#' @param Hmax Double. Height in m
#' @param GravelsFraction Integer. Gravels fraction (0-100) in %
#' @param SandsFraction Integer. Sands fraction (0-100) in %
#' @param FinesFraction Integer. Fines fraction (0-100) in %
#' @param lo Double. Truncation ratio
#' @param Bmax Double. Maximum width in m
#' @param Bo Double. TSF Crest in m
#' @param level Double. Confidence level (0-1)
#' @param s Double. Slope S=1/tan(beta)
#'
#' @return List
#' @export .fitModel.Td.F
#'
#' @import data.table
#' @import quantregForest
#' @importFrom stats predict
#'
#' @examples
#'
.fitModel.Td.F <- function(Hmax,lo=NULL,Bmax=NULL,Bo=NULL,s=NULL,GravelsFraction=NULL,SandsFraction=NULL,FinesFraction=NULL,level=0.95){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  . <- NULL
  # browser()


  if(GravelsFraction==100 && is.null(SandsFraction) &&is.null(FinesFraction)){
    SandsFraction <- 0
    FinesFraction <- 0
  }

  if(SandsFraction==100 && is.null(GravelsFraction) &&is.null(FinesFraction)){
    GravelsFraction <- 0
    FinesFraction <- 0
  }

  if(FinesFraction==100 && is.null(GravelsFraction) &&is.null(SandsFraction)){
    GravelsFraction <- 0
    SandsFraction <- 0

  }
  OK <- .checkFractions(GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction)
  stopifnot(OK)

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
  #check Hmax con SiteTable

  if(is.null(GravelsFraction) && !is.null(SandsFraction) &&!is.null(FinesFraction)){
    GravelsFraction <- 100-SandsFraction-FinesFraction
  }

  if(is.null(SandsFraction) && !is.null(GravelsFraction) &&!is.null(FinesFraction)){
    SandsFraction <- 100-GravelsFraction-FinesFraction
  }


  if(is.null(FinesFraction) && !is.null(GravelsFraction) &&!is.null(SandsFraction)){
    FinesFraction <- 100-GravelsFraction-SandsFraction
  }

  # Mean Values - temporary solution.
  # replace this code with simulation for all values and quantile estimation

  GoF <- .fitModel.Go.F(Hso=Hmax,GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction,level=level)
  VSoF <- fitModel.VSo.F(Hso=Hmax,GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction,level=level)
  moF <- .fitModel.mo.F(Hso=Hmax,GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction,level=level)
  an <- .fitModel.an(mo=moF[["mean"]],lo=lo,level=level)

  Ts <- (4*pi*Hmax/(an*(2 - moF[["mean"]])*VSoF[["mean"]]))
  return(list(Ts=round(Ts,3),Go=GoF,VSo=VSoF,mo=moF,an=an))


}


.checkFractions <- function(GravelsFraction,SandsFraction,FinesFraction){
  # Check 1

  if(sum(c(is.null(GravelsFraction),is.null(SandsFraction),is.null(FinesFraction)))==3){
    warning("At least one fraction must be defined.")
    return(FALSE)
  }

  # Check 2
  if(!is.null(SandsFraction) && !(min(SandsFraction)>=0 & max(SandsFraction)<=100)){
    warning(sprintf("SandsFraction=%g  out of range.",mo))
    return(FALSE)
  }

  #
  if(!is.null(GravelsFraction) && !(min(GravelsFraction)>=0 & max(GravelsFraction)<=100)){
    warning(sprintf("GravelsFraction=%g out of range.",lo))
    return(FALSE)
  }


  #
  if(!is.null(FinesFraction) && !(min(FinesFraction)>=0 & max(FinesFraction)<=100)){
    warning(sprintf("FinesFraction=%g out of range.",lo))
    return(FALSE)
  }


  # Check 3


  if(is.null(GravelsFraction) && !is.null(SandsFraction) && !is.null(FinesFraction)){
    message("GravelsFraction no defined. Assuming GravelsFraction <- 100-SandsFraction-FinesFraction")
    GravelsFraction <- 100-SandsFraction-FinesFraction
    # Check 4
    if(sum(c(GravelsFraction,SandsFraction,FinesFraction))!=100){
      warning("GravelsFraction+SandsFraction+FinesFraction != 100")
      return(FALSE)
    }
  }

  if(is.null(SandsFraction) && !is.null(GravelsFraction) && !is.null(FinesFraction)){
    message("SandsFraction no defined. Assuming SandsFraction <- 100-GravelsFraction-FinesFraction")
    SandsFraction <- 100-GravelsFraction-FinesFraction
    # Check 4
    if(sum(c(GravelsFraction,SandsFraction,FinesFraction))!=100){
      warning("GravelsFraction+SandsFraction+FinesFraction != 100")
      return(FALSE)
    }
  }

  if(is.null(FinesFraction) && !is.null(GravelsFraction) && !is.null(SandsFraction)){
    message("FinesFraction no defined. Assuming FinesFraction <- 100-GravelsFraction-SandsFraction")
    FinesFraction <- 100-GravelsFraction-SandsFraction
    # Check 4
    if(sum(c(GravelsFraction,SandsFraction,FinesFraction))!=100){
      warning("GravelsFraction+SandsFraction+FinesFraction != 100")
      return(FALSE)
    }
  }
  return(TRUE)
}
