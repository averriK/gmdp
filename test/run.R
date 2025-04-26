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
