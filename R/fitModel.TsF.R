#' Title
#'
#' @param Hso Double. Height of the soil column (m)
#' @param GravelsFraction Integer. Gravels fraction (0-100) in %
#' @param SandsFraction Integer. Sands fraction (0-100) in %
#' @param FinesFraction Integer. Fines fraction (0-100) in %
#' @param POPo Double. Pre-Overburden Pressure in kPa
#' @param WaterFraction Dowuble. Water Table in % as Hw=Water*Hs
#' @param OSF Double. Overshooing Factor
#' @param level Double. Confidence level (0-1)
#'
#' @return Double. Maximum Shear Module (MPA)
#' @export fitModel.TsF
#'

#' @import data.table
#' @importFrom stats predict
#' @import quantregForest
#'
#' @examples
#'
fitModel.TsF <- function(Hso,GravelsFraction=NULL,SandsFraction=NULL,FinesFraction=NULL,OSF=0.30,POPo=0,WaterFraction=0,level=0.95){
  on.exit(expr={rm(list = ls())}, add = TRUE)

  . <- NULL

  OK <- exists("SiteTable") & !is.null(SiteTable)



  # Check ranges
  if(!(POPo>=min(SiteTable$POP) & POPo<=max(SiteTable$POP))){
    warning(sprintf("POPo=%g out of model range.",POPo))}


  DATA <- SiteTable[ between(Water,floor((1-OSF)*WaterFraction),floor((1+OSF)*WaterFraction))
                     & between(POP,floor((1-OSF)*POPo),floor((1+OSF)*POPo))
                     & between(Gravels,floor((1-OSF)*GravelsFraction),floor((1+OSF)*GravelsFraction))
                     & between(Sands,  floor((1-OSF)*SandsFraction),  floor((1+OSF)*SandsFraction))
                     & between(Fines,  floor((1-OSF)*FinesFraction),  floor((1+OSF)*FinesFraction))
                     ,list(Hs,POP,Go,mo,VSo,Gravels,Sands,Fines,Water)] |> unique()


  if(nrow(DATA)>1000){
    DATA <- DATA[between(Hs,floor((1-OSF)*Hso),floor((1+OSF)*Hso))]
  }


  NEWDATA <- list(Hs=Hso,Gravels=GravelsFraction,Sands=SandsFraction,Fines=FinesFraction,POP=POPo,Water=WaterFraction)


  Y <- DATA$Ts
  X <- DATA[,c("Hs","Gravels","Sands","Fines","POP","Water")]
  MODEL <- quantregForest::quantregForest(x=X,y=Y,nthread=8)
  VALUE <- predict(MODEL,newdata=NEWDATA, what = level)
  MEAN <- predict(MODEL,newdata=NEWDATA, what = mean)
  MEDIAN <- predict(MODEL,newdata=NEWDATA, what = 0.5)
  SD <- predict(MODEL,newdata=NEWDATA, what = sd)
  UPPER <- predict(MODEL, newdata=NEWDATA,  what=max(level,abs(1-level)))
  LOWER <- predict(MODEL, newdata=NEWDATA,  what=min(level,abs(1-level)))
  return(list(value=VALUE,mean=MEAN,median=MEDIAN,upper=UPPER,lower=LOWER,sd=SD,level=level))
}
