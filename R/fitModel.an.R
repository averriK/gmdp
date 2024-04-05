#' Title
#'
#' @param mo Double. Inhomogeinity ratio. mo=0: homogeneous material. 0<=mo<=0.95
#' @param lo Double. Truncation Ratio lo<=0.5. no=0: no truncation. (bo=0)
#' @param OSF Double. Overshooting Factor. Default value: 0.30 (30%)
#' @param no Integer. Number of mode n=1..8
#'
#' @return Number. Cylinder roots
#' @export fitModel.an
#'
#' @import data.table
#' @importFrom stats predict
#' @importFrom randomForest randomForest
#'
#' @examples
#'
fitModel.an <- function(no=1,mo,lo,OSF=0.30){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  # OK <- exists("CylinderRootsModel") & !is.null(CylinderRootsModel)
  OK <- exists("CylinderRoots") & !is.null(CylinderRoots)
  stopifnot(OK)


  # Check ranges
  if(!(no %in% seq(min(CylinderRoots$n), max(CylinderRoots$n)) )){
    stop(sprintf("no=%g .Wrong mode number",no))}

  # Check ranges
  if((lo>max(CylinderRoots[n==no]$l))){
    warning(sprintf("lo=%g out of model range. Fixing to max value",lo))
    lo <- max(CylinderRoots[n==no]$l)
    }


  DATA <- CylinderRoots[n==no
                        & between(m,(1-OSF)*mo,(1+OSF)*mo)
                        & between(l,(1-OSF)*lo,(1+OSF)*lo)]



  MODEL <- randomForest::randomForest(an ~ m+l,data=DATA,importance=FALSE,proximity=FALSE)

  NEWDATA <- list(n=no,m=mo,l=lo)
  VALUE <- stats::predict(MODEL,newdata=NEWDATA)
  return(VALUE)
}
