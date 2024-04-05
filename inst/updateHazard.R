# rm(list=ls())
library(gmdp)
library(stringr)

# OutputFolder <- file.path("~/Database/gmdp/output/disagg/BC") |> normalizePath()
OutputFolder <- file.path("~/Database/gmdp/output/classical/BC") |> normalizePath()

HazardFolder <- file.path("~/Database/gmdp/hazard") |> normalizePath()
dir.exists(OutputFolder)
dir.exists(HazardFolder)
IN <- list.dirs(OutputFolder,full.names = FALSE,recursive = FALSE)
OUT <- list.files(HazardFolder,full.names = FALSE,recursive = FALSE) |>   stringr::str_remove(pattern = ".Rds") |> stringr::str_remove(pattern = "PSHA.")
PENDING <- setdiff(IN,OUT) |> na.omit()
for(OutputID in sample(PENDING)){

  PATH <- file.path(OutputFolder,OutputID) |> normalizePath()
  dir.exists(PATH)
  GMDP <- gmdp::buildGMDP(path=PATH,ID=OutputID,TRo = c(100,200,475,500,1000,2000,2475,2500,5000,10000),Vs30_STEP=10)
  message(sprintf("> Saving openquake model %s...",OutputID))
  FILE <- file.path(HazardFolder,paste0("PSHA.",OutputID,".Rds"))
  saveRDS(GMDP,file = FILE)
}
#
OutputID <- "ARM2J1W5"
OutputFolder <- file.path("~/Database/gmdp/output/user") |> normalizePath()
HazardFolder <- file.path("~/Database/gmdp/hazard") |> normalizePath()
dir.exists(OutputFolder)
dir.exists(HazardFolder)
PATH <- file.path(OutputFolder,OutputID) |> normalizePath()
GMDP <- gmdp::buildGMDP(path=PATH,ID=OutputID,TRo = c(100,200,475,500,1000,2000,2475,2500,5000,10000))
message(sprintf("> Saving openquake model %s...",OutputID))
FILE <- file.path(HazardFolder,paste0("PSHA.",OutputID,".Rds"))
saveRDS(GMDP,file = FILE)
