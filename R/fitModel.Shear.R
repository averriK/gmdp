#' Title
#'
#' @param .newdata Data frame. New data to predict
#' @param .data Double. Data frame
#' @param level Double. Confidence level (0-1)
#' @param regression String. Regression method
#'
#' @return List
#' @export fitModel.Shear
#'
#' @import data.table
#' @import quantregForest
#' @importFrom stats predict
#'
#' @examples
#'
fitModel.Shear <- function(.data=SiteTable,.newdata,level="mean",regression="qrf"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  . <- NULL
  DATA <- .data
  NEWDATA <- .newdata
  # If Hs is prescribed, filter data an remove parameter
  if("Hs" %in% names(.newdata)){
    Hso <- .find(V=unique(.data$Hs) , X=unique(.newdata$Hs))
    DATA <- DATA[Hs %in% Hso]
    NEWDATA <- NEWDATA[,-c("Hs")]
  }

  if("Gravels" %in% names(.newdata) & all(.newdata$Gravels %in% c(0,100))){
    DATA <- DATA[Gravels %in% .newdata$Gravels]
    NEWDATA <- NEWDATA[,-c("Gravels")]
  }

  if("Sands" %in% names(.newdata) & all(.newdata$Sands %in% c(0,100))){
    DATA <- DATA[Sands %in% .newdata$Sands]
    NEWDATA <- NEWDATA[,-c("Sands")]
  }

  if("Clays" %in% names(.newdata) & all(.newdata$Clays %in% c(0,100))){
    DATA <- DATA[Clays %in% .newdata$Clays]
    NEWDATA <- NEWDATA[,-c("Clays")]
  }

  if("Silts" %in% names(.newdata) & all(.newdata$Silts %in% c(0,100))){
    DATA <- DATA[Silts %in% .newdata$Silts]
    NEWDATA <- NEWDATA[,-c("Silts")]
  }

  if("Water" %in% names(.newdata) & all(.newdata$Water %in% c(0,100))){
    DATA <- DATA[Water %in% .newdata$Water]
    NEWDATA <- NEWDATA[,-c("Water")]
  }
  NR <- nrow(DATA)

  if(NR==0){
    message("No data available. Stop")
    return(NULL)
  }

  Go <- fitModel(.data = DATA, y="Go",.newdata=NEWDATA,level=level,regression=regression)
  mo <- fitModel(.data = DATA, y="mo",.newdata=NEWDATA,level=level,regression=regression)
  Ts <- fitModel(.data = DATA, y="Ts",.newdata=NEWDATA,level=level,regression=regression)
  VSo <- fitModel(.data = DATA, y="VSo",.newdata=NEWDATA,level=level,regression=regression)
  VS30 <- fitModel(.data = DATA, y="VS30",.newdata=NEWDATA,level=level,regression=regression)
  return(data.table(Go=Go,mo=mo,Ts=Ts,VSo=VSo,VS30=VS30,NEWDATA,NR=NR))

}
