devtools::load_all()
DT <- readRDS("inst/PSHA.ARMA611.Rds")
buildTable.Kh(x=DT$KmaxTable,Tso=0.1,Dao=1.0,po=c(0.16,0.50,0.84),engine="flextable",TRo=c(500,1000,2500,5000,10000),Vs30o=680,SIDo=NULL)
