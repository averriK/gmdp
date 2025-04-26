# calling_script.R
devtools::load_all()
UHSTable <- data.table()
AEPTable <- data.table()
# Vs30_TARGET <- c(200,300, 400, 500, 600, 700, 800, 900,1000, 1100)
Vs30_TARGET <- c(400,500)

# --- (1) Build UHS from Vref=760 using GEM model => "gmdp"
if (dir.exists("oq/output/gem/760")) {
  Vs30_MODEL <- Vs30_TARGET
  GMDP <- buildGMDP(
    path   = "oq/output/gem/760/",
    vref   = 760,
    vs30   = Vs30_MODEL,  # site amp
    IDo    = "gmdp"
  )

  UHSTable <- rbind(UHSTable, GMDP$UHSTable) |> unique()
  AEPTable <- rbind(AEPTable, GMDP$AEPTable) |> unique()
}


# --- (2) Build "gem" for each subfolder under gem/ (including reprocessing 760 if you want)
devtools::load_all()
Vs30_GEM <- list.dirs("oq/output/gem/", full.names=FALSE, recursive=FALSE) |>   basename()
if (length(Vs30_GEM) > 0) {
  for (Vs in Vs30_GEM) {
    GMDP <- buildGMDP(
      path   = paste0("oq/output/gem/", Vs, "/"),
      vref   = Vs,
      vs30   = NULL,   # skip site amp
      IDo    = "gem"
    )
    UHSTable <- rbind(UHSTable, GMDP$UHSTable) |> unique()
    AEPTable <- rbind(AEPTable, GMDP$AEPTable) |> unique()
  }
}

# Finally, save
saveRDS(UHSTable, "data/UHSTable.Rds")
saveRDS(AEPTable, "data/AEPTable.Rds")

