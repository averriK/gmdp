.getPGA <- function(.x,Tn0=0){
browser()
  PGAref <- .x[Tn==Tn0]$Sa
  AUX <- .x[Tn==Tn0,.(PGAref=Sa,p,POE)]
  .x <- AUX[.x,on=.(p,POE)]
  return(.x)
}
