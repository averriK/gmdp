devtools::load_all()
GMDP <- gmdp::buildGMDP(path="~/Database/gmdp/output/classical/BC/ARMA611",ID="ARMA619",TRo = c(100,200,475,500,1000,2000,2475,2500,5000,10000),Vs30_STEP=25)
buildTable.Kh(x=GMDP,Tso=0.13,Dao=1.0,po=c(0.16,0.50,0.84),engine="flextable",TRo=c(500,1000,2500,5000,10000))
fitModel.TdF(Hmax=15,lo=0.10,GravelsFraction=60,SandsFraction=30,FinesFraction=10)
