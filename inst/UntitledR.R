devtools::load_all()
Vs30_TARGET <- 180
TR_TARGET <- c(200,500,1000,2500,5000,10000)
po_TARGET <- c(0.05,0.10,0.16,0.50,"mean",0.84,0.90,0.95)
GMDP <- readRDS("~/Git/AR-MA619/data/GMDP.Rds")
UHSTable <- GMDP$UHSTable
DT <- UHSTable[Tn==0 & TR %in% TR_TARGET & Vs30 ==Vs30_TARGET & p %in% po_TARGET,.(TR,Vs30,Vref,PGAref,p)]
DT[,fitModel.AF.TR(.x=.SD,Tn=0,q=p,Vs30=180,Vref=760),by=.(TR,p)][,.(TR,AF,p,Vs30)]
