#' Title
#'
#' @param Hmax Double. Height in m
#' @param GravelsFraction Integer. Gravels fraction (0-100) in %
#' @param SandsFraction Integer. Sands fraction (0-100) in %
#' @param FinesFraction Integer. Fines fraction (0-100) in %
#' @param lo Double. Truncation ratio
#' @param Bmax Double. Maximum width in m
#' @param Bo Double. TSF Crest in m
#' @return List
#' @export fitModel.TdF
#'
#' @import data.table
#'
#' @examples
#'
fitModel.TdF <- function(Hmax,lo=NULL,Bmax=NULL,Bo=NULL,GravelsFraction=NULL,SandsFraction=NULL,FinesFraction=NULL){
  on.exit(expr={rm(list = ls())}, add = TRUE)

  OK <- checkFractions(GravelsFraction,SandsFraction,FinesFraction)

  OK <- OK & (!is.null(lo) | (!is.null(Bmax)&!is.null(Bo)))

  stopifnot(OK)

  if(is.null(lo) & !is.null(Bmax) &!is.null(Bo)){
    lo <- round(Bo/Bmax,digits=2)
  }




  #check lambda con CylinderRoots
  #check Hmax con SiteTable

  if(is.null(GravelsFraction) & !is.null(SandsFraction) &!is.null(FinesFraction)){
    GravelsFraction <- 100-SandsFraction-FinesFraction
  }

  if(is.null(SandsFraction) & !is.null(GravelsFraction) &!is.null(FinesFraction)){
    SandsFraction <- 100-GravelsFraction-FinesFraction
  }


  if(is.null(FinesFraction) & !is.null(GravelsFraction) &!is.null(SandsFraction)){
    FinesFraction <- 100-GravelsFraction-SandsFraction
  }

  GoF <- fitModel.GoF(Hso=Hmax,GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction)

  VSoF <- fitModel.VSoF(Hso=Hmax,GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction)
  moF <- fitModel.moF(Hso=Hmax,GravelsFraction=GravelsFraction,SandsFraction=SandsFraction,FinesFraction=FinesFraction)
  an <- fitModel.an(mo=moF,lo=lo)

  Ts <- (4*pi*Hmax/(an*(2 - moF)*VSoF))
  return(list(Hmax=Hmax,Bmax=Bmax,Bo=Bo,lo=lo,Go=GoF,VSo=VSoF,mo=moF,an=an,Ts=Ts))


}
