devtools::load_all()
GMDP <- buildGMDP(path="~/Database/gmdp/output/classical/BC/ARMA611",IDo="ARMA619",Vref=760,Vs30_STEP=50,ITo=1)


devtools::load_all()
buildTable.Kmax(.x=GMDP$UHSTable,Vs30o=600,Tso=0.14,Dao=15.0,p=0.50,TRo=c(2500,10000))
