#' Fit Maximum Acceleration Model for Return Period
#'
#' @description
#' Fits a model to predict maximum acceleration (Kmax) and horizontal acceleration (Kh) based on period (Ts) and peak ground acceleration (PGA).
#'
#' @param a Numeric. Model parameter a.
#' @param b Numeric. Model parameter b.
#' @param e Numeric. Model parameter e.
#' @param pga Numeric. Peak ground acceleration value.
#' @param Ts Numeric. Period value.
#' @param n Numeric. Number of points to generate. Default is 20.
#'
#' @return A data.table containing:
#' \itemize{
#'   \item Ts: Period
#'   \item Da: Displacement (cm)
#'   \item Kmax: Maximum acceleration (g)
#'   \item Kh: Horizontal acceleration (%)
#'   \item Kmax_Unit: Unit of Kmax (g)
#'   \item Kh_Unit: Unit of Kh (%)
#'   \item Da_Unit: Unit of Da (cm)
#'   \item Dmin: Minimum displacement (cm)
#'   \item Dmax: Maximum displacement (cm)
#'   \item PGA: Peak ground acceleration
#' }
#'
#' @importFrom data.table data.table
#'
#' @examples
#' \dontrun{
#' result <- fitModel.Kmax.TR(
#'     a = 2.5,
#'     b = 1.0,
#'     e = 0.74,
#'     pga = 0.3,
#'     Ts = 1.0
#' )
#' }
#'
#' @export
fitModel.Kmax.TR <- function(a, b, e, pga, Ts, n = 20) { # cm

    on.exit(expr = {
        rm(list = ls())
    }, add = TRUE)

    a0 <- a # x$a
    b0 <- b # x$b
    e0 <- e # x$e
    PGA <- pga # x$PGA
    Ts <- Ts # x$Ts
    LnDmax <- a0^2 / 0.98 + e0 - b0
    LnDmin <- LnDmax - log(200)
    LnDa <- seq(from = LnDmin, to = LnDmax, length.out = n)
    r <- (a0^2 - 0.98 * (b0 + LnDa - e0)) |> round(digits = 8)
    if (any(r < 0)) {
        stop("internal error: r<0")
    }
    Kmax <- exp((-a0 + sqrt(r)) / 0.49)
    Kh <- Kmax / PGA * 100

    DT <- data.table::data.table(Ts = Ts, Da = exp(LnDa), Kmax = Kmax, Kh = Kh, Kmax_Unit = "g", Kh_Unit = "%", Da_Unit = "cm", Dmin = round(exp(LnDmin), digits = 2), Dmax = round(exp(LnDmax), digits = 2), PGA = PGA)
    return(DT)
}
