# nolint start
TITLE <- "Probabilistic Seismic Hazard Assessment"
SUBTITLE <- "Seabee Gold Mine, Saskatchewan"
CLIENT <- "SSR Mining – Seabee Gold Operation Inc."
PROJECT <- "Seabee Gold Mine"


# TR :        475    600    975    1225   1975   2475   4975   6225   9975   12475  19975
# NEHRP BOUNDARIES
Vs30_TARGET <- c(200,300, 400, 500, 600, 700, 800, 900,1000, 1100)

TR_OBE <- 2475
TR_MDE <- 9975
TR_TARGET <- c(475,975,1975,2475,4975,9975)

p_TARGET <- c(0.50, "mean", 0.84, 0.90, 0.95)
Mw_TARGET <- 5.8
MAT <- data.table::data.table(
  ID = c("S1", "S2", "S3"),
  Name = c("TSF#1", "TSF#2", "TSF#3"),
  b = c(5, 5, 5),
  s = c(2.5, 2.5, 2.5),
  Hs = c(20, 25, 30)
)
uscs <- list()
uscs[["S1"]] <- c("GW", "GP")
uscs[["S2"]] <- c("GW", "GP")
uscs[["S3"]] <- c("GW", "GP")


REBUILD_DEAGG <- FALSE


NR_TARGET <- 100
LEVELS <- c(0.05, 0.10, 0.16, 0.50, "mean", 0.84, 0.90, 0.95)


AF_MODEL_TARGET <- "gmdp"
PGA_MODEL_TARGET <- "gmdp"


UHS_MODEL_TARGET <- "gmdp"#c( "gem", "gmdp")
AEP_MODEL_TARGET <- "gmdp" #c( "gem", "gmdp")




