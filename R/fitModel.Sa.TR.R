#' @title Fit Parametric Model for Sa as a Function of TR
#'
#' @description
#' Performs a linear regression on \code{ln(Sa)} against \code{ln(TR)} and \code{1/TR}.
#' The model is:
#' \deqn{\ln(Sa) = a + b \ln(TR) + c (1/TR).}
#'
#' @param x A data.table with at least the columns \code{Sa}, \code{TR}, \code{AEP}, and \code{POE}.
#' @param TRmin Numeric. Minimum return period to include in the fitting.
#' @param TRmax Numeric. Maximum return period to include in the fitting.
#'
#' @return A data.table with columns:
#' \itemize{
#'   \item \strong{a, b, c}: regression coefficients
#'   \item \strong{sdLnA}: standard deviation of residuals in ln(Sa)
#'   \item \strong{R2}: R-squared
#'   \item \strong{MSE, RMSE}: mean squared error and root mean squared error (in ln-space)
#'   \item \strong{fit}: a string expression like \code{"exp(a + b * log(TR) + c * 1/TR)"}
#' }
#' If the fitting fails or returns NA values, it will produce \code{NULL}.
#'
#' @details
#' The function first filters rows where \code{AEP > 0 \& POE > 0}. Then it
#' keeps only rows with \code{TRmin <= TR <= TRmax}. It does a linear model of
#' \code{LnA ~ LnTr + I(1/TR)}. The final expression is stored in the \code{fit}
#' column for convenience.
#'
#' @import data.table
#' @importFrom stats lm predict
#' @importFrom stats qnorm
#' @importFrom epoxy epoxy
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(Sa=c(0.1,0.3,0.5),
#'                  TR=c(100,500,1000),
#'                  AEP=c(0.01,0.002,0.001),
#'                  POE=c(0.3935,0.0952,0.0488))
#' fitModel.Sa.TR(dt)
#' }
fitModel.Sa.TR <- function(x, TRmin = 100, TRmax = 10000) {
  if (!inherits(x, "data.table")) {
    stop("`x` must be a data.table")
  }
  required_cols <- c("Sa", "TR", "AEP", "POE")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop("The input data.table is missing required columns: ", paste(missing_cols, collapse=", "))
  }

  # Filter out zero or negative AEP/POE
  x <- x[AEP > 0 & POE > 0]
  if (nrow(x) == 0) {
    warning("No rows with AEP>0 and POE>0. Returning NULL.")
    return(NULL)
  }

  # Check TR limits
  # If no data <= TRmax, relax TRmax
  if (nrow(x[TR <= TRmax]) == 0) {
    warning("No TR <= TRmax. Setting TRmax=Inf.")
    TRmax <- Inf
  }
  # If no data >= TRmin, relax TRmin
  if (nrow(x[TR >= TRmin]) == 0) {
    warning("No TR >= TRmin. Setting TRmin=-Inf.")
    TRmin <- -Inf
  }

  DATA <- x[TR >= TRmin & TR <= TRmax, .(
    LnA = log(Sa),
    LnTr = log(TR),
    InvTr = 1 / TR
  )]

  if (nrow(DATA) < 2) {
    warning("Not enough rows to fit the model after filtering TR. Returning NULL.")
    return(NULL)
  }

  # Fit model: ln(Sa) = a + b*ln(TR) + c*(1/TR)
  # We'll do an LM with formula: LnA ~ LnTr + InvTr
  MDL <- tryCatch(
    stats::lm(LnA ~ LnTr + InvTr, data = DATA),
    error = function(e) NULL
  )
  if (is.null(MDL)) {
    warning("Model fitting failed. Returning NULL.")
    return(NULL)
  }

  SMDL <- summary(MDL)
  coefs <- MDL$coefficients
  if (length(coefs) < 3) {
    warning("Failed to extract a,b,c from linear model. Returning NULL.")
    return(NULL)
  }

  a <- unname(coefs[1])
  b <- unname(coefs[2])
  c <- unname(coefs[3])

  sd <- SMDL$sigma
  R2 <- SMDL$r.squared

  # Compute MSE, RMSE
  Y  <- DATA$LnA
  Yp <- stats::predict(MDL)
  RSS <- sum((Y - Yp)^2)
  MSE <- RSS / length(Y)
  RMSE <- sqrt(MSE)

  # We'll store a string representation of the final function
  EXPR <- epoxy::epoxy("exp({a} + {b} * log(TR) + {c} * 1/TR)")

  DT <- data.table::data.table(
    a = a,
    b = b,
    c = c,
    sdLnA = sd,
    R2 = R2,
    MSE = MSE,
    RMSE = RMSE,
    fit = EXPR
  )

  # If any NA
  if (anyNA(DT)) {
    warning("Some NA values in fitted parameters. Returning NULL.")
    return(NULL)
  }

  return(DT)
}
