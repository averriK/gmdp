devtools::load_all()
GMDP <- buildGMDP(path="~/Database/gmdp/output/classical/BC/ARMA611",ID="ARMA619",TRo = c(500,2500,5000,10000),Vs30_STEP=50,ITo=1)
buildTable.Kh(x=GMDP,Tso=0.14,Dao=15.0,p=0.50,TRo=c(500,5000,10000))
