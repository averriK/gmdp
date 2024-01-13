#' Title
#'
#' @param GravelsFraction Integer. Gravels fraction (0-100) in %
#' @param SandsFraction Integer. Sands fraction (0-100) in %
#' @param FinesFraction Integer. Fines fraction (0-100) in %
#'#'
#' @return Boolean. TRUE if fractions are OK, FALSE otherwise
#' @export
#'
#' @examples
checkFractions <- function(GravelsFraction,SandsFraction,FinesFraction){
    # Check 1

  if(sum(c(is.null(GravelsFraction),is.null(SandsFraction),is.null(SandsFraction)))==3){
    warning("At least one fraction must be defined.")
    return(FALSE)
  }

  # Check 2
  if(!is.null(SandsFraction) & !(min(SandsFraction)>=0 & max(SandsFraction)<=100)){
    warning(sprintf("SandsFraction=%g  out of range.",mo))
    return(FALSE)
  }

  #
  if(!is.null(GravelsFraction) &!(min(GravelsFraction)>=0 & max(GravelsFraction)<100)){
    warning(sprintf("GravelsFraction=%g out of range.",lo))
    return(FALSE)
  }


  #
  if(!is.null(FinesFraction) &!(min(FinesFraction)>=0 & max(FinesFraction)<100)){
    warning(sprintf("FinesFraction=%g out of range.",lo))
    return(FALSE)
  }


  # Check 3


  if(is.null(GravelsFraction) & !is.null(SandsFraction) &!is.null(FinesFraction)){
    warning("GravelsFraction no defined. Assuming GravelsFraction <- 100-SandsFraction-FinesFraction")
    GravelsFraction <- 100-SandsFraction-FinesFraction
    # Check 4
    if(sum(c(GravelsFraction,SandsFraction,FinesFraction))!=100){
      warning("GravelsFraction+SandsFraction+FinesFraction != 100")
      return(FALSE)
    }
  }

  if(is.null(SandsFraction) & !is.null(GravelsFraction) &!is.null(FinesFraction)){
    warning("SandsFraction no defined. Assuming SandsFraction <- 100-GravelsFraction-FinesFraction")
    SandsFraction <- 100-GravelsFraction-FinesFraction
    # Check 4
    if(sum(c(GravelsFraction,SandsFraction,FinesFraction))!=100){
      warning("GravelsFraction+SandsFraction+FinesFraction != 100")
      return(FALSE)
    }
  }

  if(is.null(FinesFraction) & !is.null(GravelsFraction) &!is.null(SandsFraction)){
    warning("FinesFraction no defined. Assuming FinesFraction <- 100-GravelsFraction-SandsFraction")
    FinesFraction <- 100-GravelsFraction-SandsFraction
    # Check 4
    if(sum(c(GravelsFraction,SandsFraction,FinesFraction))!=100){
      warning("GravelsFraction+SandsFraction+FinesFraction != 100")
      return(FALSE)
    }
  }
  return(TRUE)
}
