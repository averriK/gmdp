devtools::load_all()
source("test/data.R")
BUILD_ST17 <- TRUE
BUILD_UHS <- TRUE
SMOOTH_Tn <- FALSE


source("test/build_UHS.R")
UHSTable <- UHSTable[TR %in% TR_TARGET]
