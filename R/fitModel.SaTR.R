

#' Linear regression model for Sa in terms of TR
#'
#' @param x data.table with columns Sa and TR
#' @param TRmin minimum TR value to fit
#' @param TRmax maximum TR value to fit
#'
#' @return data table with columns a, b, c, sdLnA, R2, MSE, RMSE, fit
#' @export
#'

#' @import data.table
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom stats qnorm
#'
#' @examples
#'
fitModel.SaTR <- function(x, TRmin = 100, TRmax = 10000) {
  on.exit(expr = {
    rm(list = ls())
  }, add = TRUE)




  if(any(is.na(x))){
    stop("The input data.table has NA values.")
  }
  if (all(c("Sa", "TR") %in% colnames(x))) {
    DATA <- x[TR >= TRmin & TR <= TRmax, .(LnA = log(Sa), LnTr = log(TR), AEP = 1 / TR)]
  } else {
    stop("The input data.table must have columns named  Sa, and TR.")
  }

  MDL <- stats::lm(LnA ~ ., data = DATA)
  SMDL <- summary(MDL)
  a <- unname(MDL$coefficients)[1]
  b <- unname(MDL$coefficients)[2]
  c <- unname(MDL$coefficients)[3]
  sd <- SMDL$sigma
  R2 <- SMDL$r.squared

  # Residuals & prediction plots
  Y <- DATA$LnA # True Values

  Yp <- stats::predict(MDL) # Predicted Values
  RSS <- (Y - Yp) %*% (Y - Yp) |> as.double()
  MSE <- RSS / length(Y) # caret::MSE(Yp,Y)
  RMSE <- sqrt(MSE) # caret::RMSE(Yp,Y)
  # muY <- mean(Y)
  # TSS <- (Y-muY )%*%(Y-muY )|> as.double()
  # R2 <-  1-RSS/TSS #caret::R2(Yp,Y)
  EXPR <- epoxy::epoxy("exp({a} + {b} * log(TR) + {c}*1/TR)")
  DT <- data.table::data.table(a = a, b = b, c = c, sdLnA = sd, R2 = R2, MSE = MSE, RMSE = RMSE, fit = EXPR)
  if(any(is.na(DT))){
    DT <- NULL
  }
  return(DT)
}
