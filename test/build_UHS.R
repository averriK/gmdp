# calling_script.R

UHSTable <- data.table()
AEPTable <- data.table()

# --- (1) Build UHS from Vref=760 using GEM model => "gmdp"
if (BUILD_ST17 == TRUE && dir.exists("oq/output/gem/760")) {
  Vs30_MODEL <- Vs30_TARGET
  GMDP <- buildGMDP(
    path   = "oq/output/gem/760/",
    vref   = 760,
    vs30   = Vs30_MODEL,  # site amp
    IDo    = "gmdp",
    param  = TRUE  # expansions on
  )

  UHSTable <- rbind(UHSTable, GMDP$UHSTable) |> unique()
  AEPTable <- rbind(AEPTable, GMDP$AEPTable) |> unique()
}

# --- (2) Build "gem" for each subfolder under gem/ (including reprocessing 760 if you want)
Vs30_GEM <- list.dirs("oq/output/gem/", full.names=FALSE, recursive=FALSE) |>   basename()
if (length(Vs30_GEM) > 0) {
  for (Vs in Vs30_GEM) {
    # We re-run buildGMDP for each folder, even if Vs=760
    tryCatch({
      GMDP <- buildGMDP(
        path   = paste0("oq/output/gem/", Vs, "/"),
        vref   = Vs,
        vs30   = NULL,   # skip site amp
        IDo    = "gem"
        #TRo    = TR_TARGET,
        #param  = TRUE    # expansions or read as-is as you prefer
      )
      UHSTable <- rbind(UHSTable, GMDP$UHSTable) |> unique()
      AEPTable <- rbind(AEPTable, GMDP$AEPTable) |> unique()
    }, error=function(e) {
      message("Skipping gem folder '", Vs, "': ", e$message)
    })
  }
}

# Finally, save
saveRDS(UHSTable, "data/UHSTable.Rds")
saveRDS(AEPTable, "data/AEPTable.Rds")

message("Done building both gmdp & gem tables, with minimal error handling.")


if(SMOOTH_Tn){
  AUX <- UHSTable[Tn>0,
    refineUHS(.SD,
      x_ID         = "Tn",
      y_ID         = "Sa",
      method       = "spline", # "approx"
      nout         = 100,
      logspace     = TRUE
      # spline_method= "monoH.FC"
    ),
    by = .(ID, p, TR, Vs30)
  ]
  UHSTable <- rbind(UHSTable, AUX) |> unique()
}


saveRDS(UHSTable, "data/UHSTable.Rds")
saveRDS(AEPTable, "data/AEPTable.Rds")
