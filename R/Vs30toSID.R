#' Convert Vs30 in m/s to Site Class
#'
#' @param Vs30 Double/Vector. Vs30 in m/s
#'
#' @return Character vector with Site Class.
#' @export Vs30toSID
#'
#' @examples
#'
#' Vs30toSID(1500)
#'
#' Vs30toSID(c(120,790,3000,455))
#'
Vs30toSID <- function(Vs30){
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  OK <- is.numeric(Vs30)
  stopifnot(OK)
  sapply(Vs30,Vs30toSID.char)
}


Vs30toSID.char <- function(Vs30) {
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  OK <- is.numeric(Vs30)
  stopifnot(OK)
  if (Vs30 >= 1500) {
    "A"
  } # 1500
  else if (Vs30 >= 900 & Vs30 < 1500) {
    "B"
  } # 1200
  else if (Vs30 >= 640 & Vs30 < 900) {
    "BC"
  } # 770
  else if (Vs30 >= 440 & Vs30 < 640) {
    "C"
  } # 540
  else if (Vs30 >= 300 & Vs30 < 440) {
    "CD"
  } # 370
  else if (Vs30 >= 210 & Vs30 < 300) {
    "D"
  } # 255
  else if (Vs30 >= 150 & Vs30 < 210) {
    "DE"
  } # 180
  else if (Vs30 >= 0 & Vs30 < 150) {
    "E"
  } # 150
  else {
    stop()
  }
}

