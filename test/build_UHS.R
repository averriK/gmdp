# calling_script.R
devtools::load_all()
UHSTable <- data.table()
AEPTable <- data.table()
Vs30_TARGET <- c(200,300, 400, 500, 600, 700, 800, 900,1000, 1100)
POE_TARGET <- c(0.0999, 0.0800, 0.0500, 0.0400, 0.0250, 0.0200, 0.0100, 0.0080 ,0.0050 ,0.0040, 0.0025)
# --- (1) Build UHS from Vref=760 using GEM model => "gmdp"
if (dir.exists("test/oq/output/gem/760")) {
  Vs30_MODEL <- Vs30_TARGET
  GMDP <- buildGMDP(
    path   = "test/oq/output/gem/760/",
    vref   = 760,
    vs30   = Vs30_MODEL,  # site amp
    IDo    = "gmdp",
    POE_TARGET    = POE_TARGET
  )

  UHSTable <- rbind(UHSTable, GMDP$UHSTable) |> unique()
  AEPTable <- rbind(AEPTable, GMDP$AEPTable) |> unique()
}

# --- (2) Build "gem" for each subfolder under gem/ (including reprocessing 760 if you want)
Vs30_GEM <- list.dirs("test/oq/output/gem/", full.names=FALSE, recursive=FALSE) |>   basename()
if (length(Vs30_GEM) > 0) {
  for (Vs in Vs30_GEM) {
    # We re-run buildGMDP for each folder, even if Vs=760
    tryCatch({
      GMDP <- buildGMDP(
        path   = paste0("test/oq/output/gem/", Vs, "/"),
        vref   = Vs,
        vs30   = NULL,   # skip site amp
        IDo    = "gem",
        POE_TARGET    = POE_TARGET
      )
      UHSTable <- rbind(UHSTable, GMDP$UHSTable) |> unique()
      AEPTable <- rbind(AEPTable, GMDP$AEPTable) |> unique()
    }, error=function(e) {
      message("Skipping folder '", Vs, "': ", e$message)
    })
  }
}

