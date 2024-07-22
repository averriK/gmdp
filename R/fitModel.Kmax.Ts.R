
#' Title
#'
#' @param .data data.table
#' @param Tso double
#' @param Dao double
#' @param model string c("nlm","rf")
#' @param OSF double
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
fitModel.Kmax.Ts <- function(.data,Tso,Dao,model="rf",OSF=0.3){
  on.exit(expr = {rm(list = ls())  }, add = TRUE)
# stopifnot(length(Tso)==1&length(Dao)==1)


  # Non-Linear interpolation. Mixed effects. Best
  if(model=="nlm"){
    NEWDATA <- data.table(LnTs=log(Tso),LnTs2=log(Tso)^2,LnDa=log(Dao))


    MODEL <- glm(LnKh ~LnTs+LnDa+LnTs2+LnTs*LnDa,data=.data[Ts>0 & Kh>0 & Da>0,.(LnKh=log(Kh),LnTs=log(Ts),LnTs2=log(Ts)^2,LnDa=log(Da))])
    Kh <- predict(MODEL,newdata=NEWDATA) |> exp()  |> round(digits=1)

    MODEL <- glm(LnKmax ~LnTs+LnDa+LnTs2+LnTs*LnDa,data=.data[Ts>0 & Kh>0 & Da>0,.(LnKmax=log(Kmax),LnTs=log(Ts),LnTs2=log(Ts)^2,LnDa=log(Da))])
    Kmax <- predict(MODEL,newdata=NEWDATA) |> exp() |> round(digits=3)
  }



  # Random Forest
  if(model=="rf"){
    NEWDATA <- data.table(Ts=Tso,Da=Dao)
    MODEL <- randomForest(Kh ~Ts+Da,data=.data[between(Da,(1-OSF)*min(Dao),(1+OSF)*max(Dao)) ],importance=FALSE,proximity=FALSE)
    Kh <- predict(MODEL,newdata=NEWDATA) |> round(digits=1)

    MODEL <- randomForest(Kmax ~Ts+Da,data=.data[between(Da,(1-OSF)*min(Dao),(1+OSF)*max(Dao)) ],importance=FALSE,proximity=FALSE)
    Kmax <- predict(MODEL,newdata=NEWDATA) |> round(digits=3)
  }
  DT <- data.table::data.table(Ts=Tso,Da=Dao,Kh=Kh,Kmax=Kmax)
  return(DT)
}


