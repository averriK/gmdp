SHT <- data.table()


# MAT[,lo:=(b/(2*Hs*s+b)) |> round(digits=3)]
MAT <- MAT[, .(ID, Name, b, s, lo = (b / (2 * Hs * s + b)) |> round(digits = 3)), by = .(ID, Name, b, s, Hs)]

for(id in unique(MAT$ID)){
  AUX <- MAT[ID == id, .(ID, Name, b, s, lo, getSiteProperties(Hs = Hs, USCS = uscs[[id]], NR = NR_TARGET, levels = LEVELS))]
  SHT <- rbind(SHT, AUX)
}

# Fundamental periods ----
SHT[, an := mapply(function(mo, lo) {
  getCylinderRoots(mo = mo, lo = lo, no = 1, model = "nlm", extrapolate = TRUE) |>
    round(4) |>
    suppressWarnings()
}, mo, lo)]

SHT[, Ts := (4 * pi * Hs / (an * (2 - mo) * VSo)) |> round(3)]
saveRDS(MAT, "data/MAT.Rds")
saveRDS(SHT, "data/SHT.Rds")
