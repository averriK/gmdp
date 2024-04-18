
#' Title
#'
#' @param x data.table
#' @param n Number of points
#'
#' @return data.table
#' @export fitModel.KmaxTR
#'
#' @import data.table
#' @importFrom stats predict
#' @importFrom randomForest randomForest
#'
#' @examples
#'


fitModel.KmaxTR <- function(a,b,e,PGA,Ts, n = 20) { # cm

  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)
  OK <- nrow(x) == 1
  # EQ <- exp((-a+sqrt(a^2-0.98*(b+LnDa-e)))/0.49)
  stopifnot(OK)
  a0 <- a#x$a
  b0 <- b#x$b
  e0 <- e#x$e
  PGA <- PGA#x$PGA
  Ts <- Ts#x$Ts
  LnDmax <- a0^2 / 0.98 + e0 - b0
  LnDmin <- LnDmax - log(200)
  LnDa <- seq(from = LnDmin, to = LnDmax, length.out = n)
  r <- (a0^2 - 0.98 * (b0 + LnDa - e0)) |> round(digits = 8)
  if (any(r < 0)) {
    stop("internal error: r<0")
  }
  Kmax <- exp((-a0 + sqrt(r)) / 0.49)
  Kh <- Kmax / PGA * 100

  DT <- data.table::data.table(Ts = Ts,Da = exp(LnDa), Kmax = Kmax, Kh = Kh, Kmax_Unit = "g", Kh_Unit = "%",Da_Unit="cm",Dmin=round(exp(LnDmin),digits = 2),Dmax=round(exp(LnDmax),digits = 2),PGA= PGA)
  return(DT)
}
