library(caret)
library(doParallel)
## ----
SiteModel <- list()
if(!exists("cl")) {
  CORES <- detectCores(logical = TRUE)
  cl <- makePSOCKcluster(min(CORES,40))
  registerDoParallel(cl)
} else {
  stopCluster(cl)
  CORES <- detectCores(logical = TRUE)
  cl <- makePSOCKcluster(min(CORES,40))
  registerDoParallel(cl)
}

DT.train <- SiteTable[Hw==0,.(Hs,Go,mo,VSo,Gravels,Sands,Fines)] |> unique()

## ----
SiteModel.GoF <- train(Go~Hs+Gravels+Sands+Fines,
                       data = DT.train,
                       method = "lm",
                       preProcess=c("BoxCox"),
                       metric = "RMSE",# metric ="MAE" # metric ="Rsquared"
                       trControl = trainControl(method="none",search="random",allowParallel = TRUE))

predict.train(SiteModel.GoF,newdata=list(Hs=43,Gravels=90,Sands=10,Fines=0,Water=0))
# usethis::use_data(SiteModel.GoF,overwrite = TRUE, internal = FALSE,version = 3)
## ----

SiteModel.moF <-  train(mo~Hs+Gravels+Sands+Fines,
                        data = DT.train,
                        method = "lm",
                        preProcess=c("BoxCox"),
                        metric = "RMSE",# metric ="MAE" # metric ="Rsquared"
                        trControl = trainControl(method="none",search="random",allowParallel = TRUE))

predict.train(SiteModel.moF,newdata=list(Hs=43,Gravels=90,Sands=10,Fines=0,Water=0))
# usethis::use_data(SiteModel.moF,overwrite = TRUE, internal = FALSE,version = 3)


## ----

SiteModel.VSoF <- train(VSo~Hs+Gravels+Sands+Fines,
                        data = DT.train,
                        method = "lm",
                        preProcess=c("BoxCox"),
                        metric = "RMSE",# metric ="MAE" # metric ="Rsquared"
                        trControl = trainControl(method="none",search="random",allowParallel = TRUE))

predict.train(SiteModel.VSoF,newdata=list(Hs=43,Gravels=90,Sands=10,Fines=0,Water=0))
# usethis::use_data(SiteModel.VSoF,overwrite = TRUE, internal = FALSE,version = 3)

