
#' Convert Site Class to Vs30 in m/s
#'
#' @param SID Site Class
#'
#' @return Vs30 in m/s
#' @export SIDtoVs30
#' @import data.table
#'
#' @examples
#'
#' SIDtoVs30("A")
#'
#' SIDtoVs30("AB")
#'
#' SIDtoVs30(c("BC","C","CD","D"))
#'
#'
SIDtoVs30 <- function(SID=NULL){
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  OK <- all(toupper(SID) %in% c("A", "AB", "B", "BC", "C", "CD", "D", "DE", "E"))
  stopifnot(OK)
  sapply(SID,SIDtoVs30.char)
}

SIDtoVs30.char <- function(SID) {
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  switch(EXPR=toupper(SID),
    "A" = 1500,
    "AB" = 1500,
    "B" = 1200,
    "BC" = 760,
    "C" = 540,
    "CD" = 370,
    "D" = 255,
    "DE" = 180,
    "E" = 150
  )
}
