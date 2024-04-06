#' Title
#'
#' @param Hso Double. Height of the soil column (m)
#' @param GravelsFraction Integer. Gravels fraction (0-100) in %
#' @param SandsFraction Integer. Sands fraction (0-100) in %
#' @param FinesFraction Integer. Fines fraction (0-100) in %
#' @param OSF Double. Overshooing Factor
#' @param POPo Double. Pre-Overburden Pressure in kPa
#' @param WaterFraction Dowuble. Water Table in % as Hw=Water*Hs
#'
#' @return Double. Maximum Shear Module (MPA)
#' @export fitModel.VSoF
#'

#' @import data.table
#' @importFrom stats predict
#' @importFrom randomForest randomForest
#'
#' @examples
#'
fitModel.VSoF <- function(Hso,GravelsFraction=NULL,SandsFraction=NULL,FinesFraction=NULL,OSF=0.30,POPo=0,WaterFraction=0){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  . <- NULL

  OK <- exists("SiteTable") & !is.null(SiteTable)
  stopifnot(OK)


  # Check ranges
  if(!(POPo>=min(SiteTable$POP) & POPo<=max(SiteTable$POP))){
    warning(sprintf("POPo=%g out of model range.",POPo))}


  DATA <- SiteTable[ between(Water,floor((1-OSF)*WaterFraction),floor((1+OSF)*WaterFraction))
                     & between(POP,floor((1-OSF)*POPo),floor((1+OSF)*POPo))
                     & between(Gravels,floor((1-OSF)*GravelsFraction),floor((1+OSF)*GravelsFraction))
                     & between(Sands,  floor((1-OSF)*SandsFraction),  floor((1+OSF)*SandsFraction))
                     & between(Fines,  floor((1-OSF)*FinesFraction),  floor((1+OSF)*FinesFraction))
                     ,.(Hs,POP,Go,mo,VSo,Gravels,Sands,Fines,Water)] |> unique()



  if(nrow(DATA)>1000){
    DATA <- DATA[between(Hs,floor((1-OSF)*Hso),floor((1+OSF)*Hso))]
  }


  NEWDATA <- list(Hs=Hso,Gravels=GravelsFraction,Sands=SandsFraction,Fines=FinesFraction,POP=POPo,Water=WaterFraction)

  MODEL <- randomForest::randomForest(VSo~Hs+Gravels+Sands+Fines+POP+Water,data=DATA,importance=FALSE,proximity=FALSE)

  VALUE <- stats::predict(MODEL,newdata=NEWDATA)
  return(VALUE)

}
