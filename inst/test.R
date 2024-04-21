devtools::load_all()
GMDP <- buildGMDP(path="~/Database/gmdp/output/classical/BC/ARMA611",IDo="ARMA619",Vref=760,Vs30_STEP=10,ITo=1)


devtools::load_all()
buildTable.Kmax(.x=GMDP$UHSTable,Vs30o=seq(200,400,by=10),Tso=c(0.11,0.17),Dao=c(5.0,15.0),p=0.50,TRo=c(500,5000,10000))
