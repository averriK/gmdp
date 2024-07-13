devtools::load_all()
# GMDP <- buildGMDP(path="~/Database/gmdp/output/classical/BC/ARMA611",IDo="ARMA619",Vref=760,Vs30_STEP=10,ITo=1)
GMDP <- buildGMDP(path="~/Database/gmdp/output/classical/BC/ARMA611",IDo="ARMA619",Vref=760,Vs30_STEP=10,ITo=1)


devtools::load_all()
buildTable.Kmax(.x=GMDP$UHSTable,Vs30o=c(200,300,400),Tso=0.12,Dao=15.0,po=0.50,TRo=c(500,5000,10000))
