  MCE <- data.table::fread("../mapper/output/epicenters.csv")
  MCE <- MCE[,.(Repi=round(Repi,0),Mw=mag,Date=as.Date(time),Lat=latitude,Lon=longitude,Depth=depth,Location=place)]

