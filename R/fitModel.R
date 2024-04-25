
#' Title
#'
#' @param .data data.table
#' @param y character
#' @param .newdata vector
#' @param level double.
#' @param regression character
#' @param removeZeroInstances logical
#' @param uniqueResponses logical
#' @param ntree integer
#' @param x character
#'
#' @return vector
#' @export fitModel
#'
#' @examples
#'
#'

# ********************************
# Fix level for lm
# remove rf. keep qrf only
# set default level to 0.16,0.50,0.84


fitModel <- function(.data, x=NULL,y,.newdata=NULL,level=0.95,regression="qrf",removeZeroInstances=FALSE,uniqueResponses=FALSE,ntree=500) {
  . <- NULL
  # Capture the variable arguments as a vector
  stopifnot(y %in% names(.data))
. <- NULL
  if(!is.null(x)) {
    # .newdata==NULL: build model. Return model. ignore .newdata
    VARS <- x
    XCOLS <- VARS[VARS %in% names(.data)]
  }

  if(is.data.table(.newdata)){
    # .newdata!=NULL: build model, predict new data, return response, ignore .x
    VARS <- names(.newdata)
    XCOLS <- VARS[VARS %in% names(.data)]
  }
  # Subset the data to include only valid columns
  # Check and keep only those columns that are present in the data.table

  # COLS <- VARS[VARS %in% names(.data)]

  YCOL <- y
  COLS <- c(XCOLS, YCOL)
  #remmove N/A
  DATA <- .data[, ..COLS, with = FALSE] |> na.omit() |> unique()

  # Remove duplicated responses
  if(uniqueResponses){
    idx <- !duplicated(DATA[[YCOL]])
    DATA <- DATA[idx]
  }
  # Remove instances with Zero values
  if(removeZeroInstances){
    idx <- DATA[,apply(.SD, 1, function(x) all(x > 0)),.SDcols = XCOLS]
    DATA <- DATA[idx]
  }

  # Average responses with identical instances
  DATA <- DATA[, .(Y = mean(get(YCOL))), by = mget(XCOLS)]

  if(nrow(DATA) == 0) {
    stop("No valid data found")
  }
  X <- DATA[, ..XCOLS, with = FALSE]
  Y <- DATA$Y

  .model <- switch(regression,
                   "qrf"=quantregForest::quantregForest(x=X,y=Y,nthread=8,keep.inbag = FALSE,ntree=ntree),
                   "rf"=randomForest::randomForest(Y ~ ., data=DATA,importance=FALSE,proximity=FALSE,ntree=ntree),
                   "lm"=stats::lm(Y ~ ., data=DATA)
  )

  if(is.null(.newdata)){
    # .newdata==NULL: build model. Return model

    Yp <- predict(.model,newdata=DATA, what = 0.50) |> unname()
    RSS <- (Y - Yp) %*% (Y - Yp) |> as.double()
    MSE <- RSS / length(Y) # caret::MSE(Yp,Y)
    RMSE <- sqrt(MSE) # caret::RMSE(Yp,Y)
    muY <- mean(Y)
    TSS <- (Y-muY )%*%(Y-muY )|> as.double()
    R2 <-  1-RSS/TSS #caret::R2(Yp,Y)


    return(list(model=.model,data=DATA, RSS=RSS, MSE=MSE, RMSE=RMSE, R2=R2))
  }

  if(is.data.table(.newdata)){
    # .newdata!=NULL: build model, predict new data, return response
    if(regression=="lm"){
      VALUE <- (predict(.model,newdata=.newdata,interval = "prediction",level=level)) |> as.data.table()
      VALUE <- VALUE$upr
    }

    if(regression=="qrf"){
      VALUE <- predict(.model,newdata=.newdata, what = level)
    }


    return(VALUE)
  }






  # Return the filtered data.table
  return()
}
