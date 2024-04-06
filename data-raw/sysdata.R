## ----
## CAUTION: THIS SCRIPT MAY TAKE A LONG TIME TO RUN. IT IS NOT NECESSARY TO RUN THIS SCRIPT TO USE THE PACKAGE. JUST USE WHEN SiteTable DATA Changes

library(data.table)
# library(caret)
# library(doParallel)

# ------------------------------------------------------------------------

PATH <- "data-raw/MaterialProperties.xlsx"
ShearModelParameters <- readxl::read_excel(path = PATH,sheet ="ShearModelParameters" ) |> data.table::as.data.table()
# use_data(ShearModelParameters,internal=TRUE,overwrite = TRUE,version = 3)

VoidRatiosUSCS <- readxl::read_excel(path = PATH,sheet ="VoidRatiosUSCS" ) |> data.table::as.data.table()
# use_data(VoidRatiosUSCS,internal=FALSE,overwrite = TRUE,version = 3)


RelativeDensityRanges <- readxl::read_excel(path = PATH,sheet ="RelativeDensityRanges" ) |> data.table::as.data.table()
# use_data(RelativeDensityRanges,internal=FALSE,overwrite = TRUE,version = 3)

UnitWeightRanges <- readxl::read_excel(path =PATH,sheet ="UnitWeightRanges" ) |> data.table::as.data.table()
# use_data(UnitWeightRanges,internal=FALSE,overwrite = TRUE)


ParticleSize  <- readxl::read_excel(path = PATH,sheet ="ParticleSize" ) |> data.table::as.data.table()
# use_data(ParticleSize,internal=FALSE,overwrite = TRUE,version = 3)

USCS  <- readxl::read_excel(path = PATH,sheet ="USCS" ) |> data.table::as.data.table()
# use_data(USCS,internal=FALSE,overwrite = TRUE,version = 3)

ValidSands <- c("SP","SM","SC","SW")
ValidGravels <- c("GW","GP","GM","GC")
ValidSilts <- c("MH","ML")
ValidClays <- c("CH","CL")
ValidOrganic <- c("OH","OL","PT")
ValidFines <- c(ValidSilts,ValidClays,ValidOrganic)
ValidCoarse <- c(ValidSands,ValidGravels)
ValidUSCS <- c(ValidCoarse,ValidFines)
ValidGroups <- c("gravels","sands","fines","silts","clays","organic")

# ----------------------------------------
library(data.table)
LIST <- list.files("data-raw/dsra",full.names = TRUE,recursive = FALSE)
SiteTable <- data.table()
for(FILE in LIST){
  # Read data with possible duplicate columns. check.names=FALSE
  AUX <- fread(file=FILE,yaml = TRUE,check.names = FALSE) |> suppressWarnings()
  AUX <- AUX[, .SD, .SDcols = unique(colnames(AUX))]

  SiteTable <- rbindlist(list(AUX,SiteTable),use.names = TRUE)
}

# saveRDS(SiteTable,file=file.path("data-raw","SiteTable.Rds"))
# fwrite(SiteTable,file=file.path("data-raw","SiteTable.csv"),yaml = TRUE)
# usethis::use_data(SiteTable,overwrite = TRUE, internal = FALSE,version = 3)
# --------------------------------------------

PATH <- "data-raw/dg85"
FILES <- list.files(path=PATH,pattern = "roots_.*\\.csv",full.names = TRUE)
CylinderRoots <- data.table()
for(FILE in FILES){
  DT <- data.table::fread(FILE)
  CylinderRoots <- data.table::rbindlist(list(CylinderRoots,DT))
}
# fwrite(CylinderRoots,file="data-raw/CylinderRoots.csv",yaml = TRUE)
# usethis::use_data(CylinderRoots,overwrite = TRUE, internal = FALSE,version = 3)


# ----
#
# if(!exists("cl")) {
#   CORES <- detectCores(logical = TRUE)
#   cl <- makePSOCKcluster(min(CORES,40))
#   registerDoParallel(cl)
# } else {
#   stopCluster(cl)
#   CORES <- detectCores(logical = TRUE)
#   cl <- makePSOCKcluster(min(CORES,40))
#   registerDoParallel(cl)
# }
#
# DT.train <- CylinderRoots[n==1]
#
# ## ----
# CylinderRootsModel <- train(an ~ m+l,
#                       data = DT.train,
#                       method = "lm",
#                       preProcess=NULL,
#                       metric = "RMSE",# metric ="MAE" # metric ="Rsquared"
#                       trControl = trainControl(method="cv",search="grid",allowParallel = TRUE))
#
# predict.train(CylinderRootsModel,newdata=list(m=0.27,l=0.035))
# source("data-raw/R/SiteModel.R")



# --------------------------------------------

usethis::use_data(CylinderRoots,ShearModelParameters,VoidRatiosUSCS,RelativeDensityRanges,UnitWeightRanges,ParticleSize,USCS,SiteTable,ValidSands,ValidGravels,ValidSilts,ValidClays,ValidOrganic,ValidFines,ValidCoarse,ValidUSCS,ValidGroups,overwrite = TRUE, internal = TRUE,version = 3)
