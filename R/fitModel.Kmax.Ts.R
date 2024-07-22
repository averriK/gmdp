
#' Title
#'
#' @param .data data.table
#' @param Tso double
#' @param Dao double
#' @param model string c("lm","nlm","dt","rf")
#'
#' @return
#' @export fitModel.Kmax.Ts
#'
#' @import rpart
#' @import randomForest
#' @import data.table
#' @importFrom stats predict
#' @importFrom stats glm
#' @importFrom stats lm
#'
#'
#' @examples
#'
fitModel.Kmax.Ts <- function(.data,Tso,Dao,model="rf"){
  on.exit(expr = {rm(list = ls())  }, add = TRUE)

  # . <- NULL
  # .newdata <- data.table(Ts=Tso,Da=Dao)
  #
  # .data <- .SD[,.(Kh,Ts,Da)]

  # Linear interpolation
  if(model=="lm"){
    MODEL <- glm(Kh ~Td+Da,data=.data)
    NEWDATA <- list(Ts=Tso,Da=Dao)
    Kh <- predict(MODEL,newdata=NEWDATA)

    MODEL <- glm(Kmax ~Ts+Da,data=.data)
    Kmax <- predict(MODEL,newdata=NEWDATA)

  }
  # Non-Linear interpolation. Mixed effects. Best
  if(model=="nlm"){
    MODEL <- glm(Kh ~Ts+Da+Ts*Da,data=.data)
    NEWDATA <- list(Ts=Tso,Da=Dao)
    Kh <- predict(MODEL,newdata=NEWDATA) |> round(digits=2)

    MODEL <- glm(Kmax ~Ts+Da+Ts*Da,data=.data)
    Kmax <- predict(MODEL,newdata=NEWDATA) |> round(digits=4)

  }

  # Decision Trees
  if(model=="dt"){
    MODEL <- rpart(Kh ~Ts+Da,data=.data)
    NEWDATA <- list(Ts=Tso,Da=Dao)
    Kh <- predict(MODEL,newdata=NEWDATA) |> round(digits=2)

    MODEL <- rpart(Kmax ~Ts+Da,data=.data)
    Kmax <- predict(MODEL,newdata=NEWDATA) |> round(digits=4)

  }


  # Random Forest
  if(model=="rf"){
    # Reduce the dataset
    MODEL <- randomForest(Kh ~Ts+Da,data=.data,importance=FALSE,proximity=FALSE)
    NEWDATA <- list(Ts=Tso,Da=Dao)
    Kh <- predict(MODEL,newdata=NEWDATA) |> round(digits=2)

    MODEL <- randomForest(Kmax ~Ts+Da,data=.data,importance=FALSE,proximity=FALSE)
    Kmax <- predict(MODEL,newdata=NEWDATA) |> round(digits=4)
  }
  DT <- data.table::data.table(Ts=Tso,Da=Dao,Kh=Kh,Kmax=Kmax)
  return(DT)
}


