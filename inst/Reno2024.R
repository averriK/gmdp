devtools::load_all()
Vs30_TARGET <- 180
TR_TARGET <- c(200,500,1000,2500,5000,10000)
po_TARGET <- c(0.05,0.10,0.16,0.50,"mean",0.84,0.90,0.95)
GMDP <- readRDS("~/Git/AR-MA619/data/GMDP.Rds")
UHSTable <- GMDP$UHSTable
DT <- UHSTable[TR %in% TR_TARGET & p %in% po_TARGET & Vs30 %in% Vs30_TARGET][order(TR)]
# BRAY MACEDO
DT <- DT[,fitModel.Dn.TR(Sa=Sa,PGA=PGA,Tn=Tn,Mw = 6.5, xD = 1.3, kymin = 0.001, kymax = 0.55, n = 100), by = .(ID,Tn, TR,Vref,Vs30, p)] |> unique()


DATA <- DT[p=="mean" & Tn==0,.(X=TR,Y=Dn,ID=Tn)]

setorder(DATA,X)
xplot::xplot(
  library = "highcharter",
  color.palette = "Dynamic",
  yAxis.label =TRUE,
  plot.type="spline",
  legend.layout="horizontal",
  legend.show=TRUE,
  xAxis.log = FALSE,
  yAxis.log = FALSE,
  xAxis.legend="Tn[s]",
  yAxis.legend="Dn[cm]",
  group.legend="TR[yr]",
  data=DATA)


# STEWART

DT <- UHSTable[Tn==0 & TR %in% TR_TARGET & Vs30 ==Vs30_TARGET & p %in% po_TARGET,.(TR,Vs30,Vref,PGAref,p)]
DT[,fitModel.AF.TR(.x=.SD,Tn=0,q=p,Vs30=180,Vref=760),by=.(TR,p)][,.(TR,AF,p,Vs30)]
