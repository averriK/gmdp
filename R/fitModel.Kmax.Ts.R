#' Fit Maximum Acceleration Model for Period
#'
#' @description
#' Fits a model to predict maximum acceleration (Kmax) and horizontal acceleration (Kh) based on period (Ts) and displacement (Da).
#'
#' @param .data data.table. Input data containing Ts, Kh, Kmax, and Da values.
#' @param Tso Numeric. Target period value.
#' @param Dao Numeric. Target displacement value.
#' @param model Character. Model type to use: "nlm" for non-linear mixed effects or "rf" for random forest. Default is "rf".
#' @param OSF Numeric. Oversampling factor for random forest model. Default is 0.3.
#'
#' @return A data.table containing:
#' \itemize{
#'   \item Ts: Period
#'   \item Da: Displacement
#'   \item Kh: Horizontal acceleration
#'   \item Kmax: Maximum acceleration
#' }
#'
#' @importFrom data.table data.table CJ between
#' @importFrom randomForest randomForest
#' @importFrom stats glm predict
#'
#' @examples
#' \dontrun{
#' data <- data.table(
#'     Ts = c(0.5, 1.0, 1.5),
#'     Kh = c(0.3, 0.4, 0.5),
#'     Kmax = c(0.4, 0.5, 0.6),
#'     Da = c(10, 20, 30)
#' )
#' result <- fitModel.Kmax.Ts(
#'     .data = data,
#'     Tso = 1.0,
#'     Dao = 20,
#'     model = "rf"
#' )
#' }
#'
#' @export
fitModel.Kmax.Ts <- function(.data, Tso, Dao, model = "rf", OSF = 0.3) {
    # stopifnot(length(Tso)==1&length(Dao)==1)
    stopifnot(model %in% c("nlm", "rf"))
    .newdata <- data.table::CJ(Tso = Tso, Dao = Dao)
    . <- NULL
    # Non-Linear interpolation. Mixed effects. Best
    if (model == "nlm") {
        NEWDATA <- .newdata[, .(LnTs = log(Tso), LnTs2 = log(Tso)^2, LnDa = log(Dao))]
        RFDATA <- .data[Ts > 0 & Kh > 0 & Da > 0, .(LnKh = log(Kh), LnKmax = log(Kmax), LnTs = log(Ts), LnTs2 = log(Ts)^2, LnDa = log(Da))]


        MODEL <- glm(LnKh ~ LnTs + LnDa + LnTs2 + LnTs * LnDa, data = RFDATA)
        Kh <- predict(MODEL, newdata = NEWDATA) |>
            exp() |>
            round(digits = 1)

        MODEL <- glm(LnKmax ~ LnTs + LnDa + LnTs2 + LnTs * LnDa, data = RFDATA)
        Kmax <- predict(MODEL, newdata = NEWDATA) |>
            exp() |>
            round(digits = 3)
    }



    # Random Forest
    if (model == "rf") {
        NEWDATA <- .newdata[, .(Ts = Tso, Da = Dao)]
        AUX <- .data[between(Da, (1 - OSF) * min(Dao), (1 + OSF) * max(Dao))]
        if (nrow(AUX >= 100)) {
            RFDATA <- AUX
        } else {
            RFDATA <- .data
        }

        MODEL <- randomForest(Kh ~ Ts + Da, data = RFDATA, importance = FALSE, proximity = FALSE)
        Kh <- predict(MODEL, newdata = NEWDATA) |> round(digits = 1)

        MODEL <- randomForest(Kmax ~ Ts + Da, data = RFDATA, importance = FALSE, proximity = FALSE)
        Kmax <- predict(MODEL, newdata = NEWDATA) |> round(digits = 3)
    }
    DT <- data.table::data.table(Ts = Tso, Da = Dao, Kh = Kh, Kmax = Kmax)
    return(DT)
}
