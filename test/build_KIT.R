
SHT <- readRDS("data/SHT.Rds")
SHT <- SHT[level %in% p_TARGET, .(ID, Name, Hs = Hs, Tso = Ts, p = level)]
# ADOPT gem models for KIT
# UHS <- UHSTable[Vs30 %in% Vs30_TARGET & ID=="gem" & TR %in% TR_TARGET & p %in% p_TARGET]
UHS <- UHSTable[Vs30 %in% Vs30_TARGET & TR %in% TR_TARGET & p %in% p_TARGET,.(ID="average",PGA=mean(PGA),Sa=mean(Sa),AEP=mean(AEP)),by=.(Tn,TR,p,Vs30) ]



DnTable <- UHS[, dsra::fitModel.Dn.TR(Sa = Sa, PGA = PGA, Tn = Tn, Mw = Mw_TARGET, xD = 1.3, kymin = 0.001, kymax = 0.55, n = 100), by = .(Tn, TR, Vs30, p)] |> unique()
DnTRmodel <- DnTable[, .(Tn, TR, p, Ts, Vs30, a, b, e, PGA)] |> unique()
KhTRmodel <- DnTRmodel[, fitModel.Kmax.TR(a = a, b = b, e = e, pga = PGA, Ts = Ts, n = 100), by = .(Tn, TR, p, Vs30)]

#
KIT <- data.table()
for(id in unique(MAT$ID)){
  ID_TARGET <- id
  Hs <- SHT[ID == ID_TARGET]$Hs |> unique()
  Da_TARGET <- Hs * 100 * XDa
  DATA <- KhTRmodel[SHT[ID == ID_TARGET], on = c("p")]
  AUX <- DATA[Vs30 %in% Vs30_TARGET, fitModel.Kmax.Ts(.SD, Tso = Tso, Dao = Da_TARGET, model = "rf"), by = .(ID, Name, TR, p, Vs30, Tso)]
  KIT <- rbindlist(list(KIT, AUX), use.names = TRUE)
}



KIT <- KIT[, -c("Tso")]
saveRDS(KIT, "data/KIT.Rds")
