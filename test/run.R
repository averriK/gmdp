<<<<<<< HEAD
<<<<<<< HEAD
devtools::load_all()
source("test/data.R")
BUILD_ST17 <- TRUE
BUILD_UHS <- TRUE
SMOOTH_Tn <- FALSE


source("test/build_UHS.R")
UHSTable <- UHSTable[TR %in% TR_TARGET]
=======
=======
>>>>>>> origin/v0.7.1
# nolint start

# MCE

if (BUILD_MCE) {
    source("R/build_MCE.R")
} else {
    MCE <- NULL
}

if (BUILD_UHS) {
    source("R/build_UHS.R")
} else {
    UHSTable <- readRDS("data/UHSTable.Rds")
    AEPTable <- readRDS("data/AEPTable.Rds")
}


# Fundamental Periods -----
if (BUILD_TS) {
    source("R/build_TS.R")
} else {
    SHT <- readRDS("data/SHT.Rds")
    MAT <- readRDS("data/MAT.Rds")
}



# Seismic Coefficient ----
if (BUILD_KIT) {
    source("R/build_KIT.R")
} else {
    KIT <- readRDS("data/KIT.Rds")
}

# nolint end
<<<<<<< HEAD
>>>>>>> 3bae604 (clean)
=======
>>>>>>> origin/v0.7.1
