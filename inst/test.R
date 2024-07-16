# Example #1
# GMDP <- buildGMDP(path="~/Database/gmdp/output/classical/BC/ARMA611",IDo="ARMA619",Vref=760,Vs30_STEP=10,ITo=1)
devtools::load_all()
GMDP <- buildGMDP(vref=250,path="inst/250/",IDo="ARM4V4D")


# Example #2. No PGV data
devtools::load_all()
GMDP <- buildGMDP(vref=3000,vs30=450,path="inst/3000/",IDo="ARM4V4D")

# Example #2. No PGV data
devtools::load_all()
GMDP <- buildGMDP(vref=3000,vs30=c(450,290),path="inst/3000/",IDo="ARM4V4D")
# Example #3
Vs30_STEP <- 100
S1 <- seq(SIDtoVs30("E"), SIDtoVs30("A"), by = Vs30_STEP)
S2 <- sapply(c("A","B","BC", "C", "CD", "D", "DE", "E"), SIDtoVs30) |> unname()
Vs30_TARGET <- c(S1,S2) |> unique() |> sort()
devtools::load_all()
GMDP <- buildGMDP(vref=3000,vs30=Vs30_TARGET,path="inst/3000/",IDo="ARM4V4D")

# Example #4
devtools::load_all()
buildTable.Kmax(.x=GMDP$UHSTable,Vs30o=c(200,300,400),Tso=0.12,Dao=15.0,po=0.50,TRo=c(500,5000,10000))
