# Example #1

devtools::load_all()
GMDP <- buildGMDP(vref=760,path="test/shm6_760/",IDo="shm6_760")

devtools::load_all()
GMDP <- buildGMDP(vref=760,path="test/gem_760/",IDo="shm6_760")

devtools::load_all()
GMDP <- buildGMDP(vref=760,path="test/760/",IDo="ARM4V4D")


# Example #2. No PGV data
devtools::load_all()
GMDP <- buildGMDP(vref=3000,vs30=450,path="test/3000/",IDo="ARM4V4D")

# Example #2. No PGV data
devtools::load_all()
GMDP <- buildGMDP(vref=3000,vs30=c(450,290),path="test/3000/",IDo="ARM4V4D")
# Example #3
Vs30_STEP <- 100
S1 <- seq(SIDtoVs30("E"), SIDtoVs30("A"), by = Vs30_STEP)
S2 <- sapply(c("A","B","BC", "C", "CD", "D", "DE", "E"), SIDtoVs30) |> unname()
Vs30_TARGET <- c(S1,S2) |> unique() |> sort()
devtools::load_all()
GMDP <- buildGMDP(vref=3000,vs30=Vs30_TARGET,path="test/3000/",IDo="ARM4V4D")

# Example #4. Build DnTRmodel
devtools::load_all()
UHSTable <- readRDS("test/UHSTable.rds")

Vs30_TARGET <- 250
TR_TARGET <- c(100,475,1000,2475,5000,10000)
UHS <- UHSTable[ SM=="openquake" & ID=="nrc_v3.19.1" & TR %in% TR_TARGET & Vs30 == Vs30_TARGET  & p=="mean"]
DnTable <-  UHS[,dsra::fitModel.Dn.TR(Sa=Sa,PGA=PGA,Tn=Tn,Mw = 6.5, xD = 1.3, kymin = 0.001, kymax = 0.55, n = 100), by = .(ID,SM,Tn, TR,Vref,Vs30, p)] |> unique()


# Example #3. Build KmaxTable
devtools::load_all()
DnTRmodel <- DnTable[, .(ID,SM,Tn, TR, p, Ts, Vs30, Vref, a, b, e, PGA)] |> unique()
KhTRmodel <- DnTRmodel[, fitModel.Kmax.TR( a=a,b=b,e=e,pga=PGA,Ts=Ts, n = 100), by = .(ID,SM,Tn, TR, p,Vs30,Vref)]
KhTRmodel[,fitModel.Kmax.Ts(.SD,Tso=0.6,Dao=1,model="rf"),by=.(TR,p,Vs30),.SDcols=colnames(KhTRmodel)]
KhTRmodel[,fitModel.Kmax.Ts(.SD,Tso=0.6,Dao=1,model="nlm"),by=.(TR,p,Vs30),.SDcols=colnames(KhTRmodel)]




# buildTable.Kmax(.x=UHS,Tso=0.12,Dao=15.0,po=0.50,TRo=10000)
