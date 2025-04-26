#' Fit Displacement Model for Return Period
#'
#' @description
#' Fits a displacement model for a given return period based on spectral acceleration, peak ground acceleration, and other parameters.
#'
#' @param Tn Numeric. Natural period.
#' @param Sa Numeric. Spectral acceleration.
#' @param PGA Numeric. Peak ground acceleration.
#' @param xD Numeric. BM19 factor. Default is 1.3.
#' @param Mw Numeric. Magnitude. Default is 6.5.
#' @param kymin Numeric. Minimum ky value. Default is 0.005.
#' @param kymax Numeric. Maximum ky value. Default is 0.5.
#' @param n Integer. Number of ky values. Default is 30.
#'
#' @return A data.table containing:
#' \itemize{
#'   \item PGA: Peak ground acceleration (g)
#'   \item PGV: Peak ground velocity (cm/s)
#'   \item Ts: Period
#'   \item ky: Yield coefficient
#'   \item Mw: Magnitude
#'   \item c1, c2, c3: Model coefficients
#'   \item a0, a1, a2: Model parameters
#'   \item muLnD: Mean of natural logarithm of displacement
#'   \item P, Q: Model parameters
#'   \item a, b: Model coefficients
#'   \item P0: Probability
#'   \item e: Model parameter
#'   \item Dn: Displacement (cm)
#'   \item sdLnD: Standard deviation of natural logarithm of displacement
#'   \item PGA_Unit: Unit of PGA (g)
#'   \item PGV_Unit: Unit of PGV (cm/s)
#'   \item Dn_Unit: Unit of displacement (cm)
#' }
#'
#' @importFrom data.table data.table
#' @importFrom stats pnorm
#'
#' @examples
#' \dontrun{
#' result <- fitModel.Dn.TR(
#'     Tn = 1.0,
#'     Sa = 0.5,
#'     PGA = 0.3,
#'     xD = 1.3,
#'     Mw = 6.5
#' )
#' }
#'
#' @export
fitModel.Dn.TR <- function(Tn, Sa, PGA, xD = 1.3, Mw = 6.5, kymin = 0.005, kymax = 0.5, n = 30) {
    on.exit(expr = {
        rm(list = ls())
    }, add = TRUE)

    # Tn <- x$Tn
    Ts <- xD * Tn # x$Tn
    # po <- x$p
    muSa <- Sa # x$Sa
    muLnSa <- log(Sa) # log(x$Sa)
    # PGA <- x$PGA
    PGV <- 55 * PGA # Idriss: pgv=55 * PGA[g] = [cm/s]
    ky <- seq(from = log(kymin), to = log(kymax), length.out = n) |>
        exp() |>
        round(digits = 4) |>
        unique()
    # Bray Macedo Model 2017/2019 ----
    if (PGV <= 60) {
        # Ordinary GM
        if (Ts < 0.1) {
            # Model 1
            c1 <- -4.551
            c2 <- -9.690
            c3 <- 0.0
        } else {
            # Model 2
            c1 <- -5.894
            c2 <- 3.152
            c3 <- -0.910
        }
        c4 <- +0.0
        sdLnDo <- 0.736
    }
    if (PGV > 60 & PGV <= 150) {
        # Pulse-like Records
        if (Ts < 0.1) {
            # Model 1
            c1 <- -6.235
            c2 <- -2.744
            c3 <- +0.0
        } else if (Ts >= 0.1) {
            # Model 2
            c1 <- -6.462
            c2 <- +1.069
            c3 <- -0.498
        } else {
            stop()
        }
        c4 <- +1.547
        sdLnDo <- 0.59
    }
    if (PGV >= 150) {
        if (Ts < 0.1) {
            # Model 1
            c1 <- +2.480
            c2 <- -2.744
            c3 <- +0.0
        } else if (Ts >= 0.1) {
            # Model 2
            c1 <- +2.253
            c2 <- +1.069
            c3 <- -0.498
        } else {
            stop()
        }
        c4 <- +0.097
        sdLnDo <- 0.59
    }

    a0 <- c1 + 0.607 * Mw + c2 * Ts + c3 * Ts^2 - 2.49 * log(ky) - 0.245 * log(ky)^2 + c4 * log(PGV)
    a1 <- 2.703 + 0.344 * log(ky)
    a2 <- -0.089
    if (Ts <= 0.7) {
        P <- -2.48 - 2.97 * log(ky) - 0.12 * log(ky)^2 - 0.72 * Ts * log(ky) + 1.70 * Ts
    } else {
        P <- -3.42 - 4.93 * log(ky) - 0.30 * log(ky)^2 - 0.35 * Ts * log(ky) + 0.62 * Ts
    }

    if (Ts <= 0.7) {
        Q <- -2.48 - 2.97 * log(ky) - 0.12 * log(ky)^2 - 0.72 * Ts * log(ky) + 1.70 * Ts + 2.78 * muLnSa
    }
    if (Ts > 0.7) {
        Q <- -3.42 - 4.93 * log(ky) - 0.30 * log(ky)^2 - 0.35 * Ts * log(ky) + 0.62 * Ts + 2.86 * muLnSa
    }
    # MODEL ----
    muLnD <- a0 + a1 * muLnSa + a2 * muLnSa^2
    sdLnD <- sdLnDo
    a <- +2.491 - 0.344 * muLnSa
    b <- -c1 - 2.703 * muLnSa + 0.089 * muLnSa^2 - c2 * Ts - c3 * Ts^2 - 0.607 * Mw
    P0 <- 1 - stats::pnorm(Q)
    e <- 0.74
    EXPR <- "exp((-a+sqrt(a^2-0.98*(b+LnDa-e)))/0.49)"
    DT <- data.table::data.table(PGA = PGA, PGV = PGV, Ts = Ts, ky = ky, Mw = Mw, c1 = c1, c2 = c2, c3 = c3, a0 = a0, a1 = a1, a2 = a2, muLnD = muLnD, P = P, Q = Q, a = a, b = b, P0 = P0, e = e, Dn = exp(muLnD), sdLnD = sdLnD, PGA_Unit = "g", PGV_Unit = "cm/s", Dn_Unit = "cm")
    return(DT)
}
