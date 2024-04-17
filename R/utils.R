.getPGA <- function(.x,Tn0=0){
browser()
  PGAref <- .x[Tn==Tn0]$Sa
  AUX <- .x[Tn==Tn0,.(PGAref=Sa,p,POE,IT,AEP,TR)]
  .x <- AUX[.x,on=.(p,POE,IT,AEP,TR)]
  return(.x)
}
