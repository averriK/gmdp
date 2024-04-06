
# Define the wrapper function for buildProfile using the helper
buildPlot <- function(suffix, ...) {
  on.exit(expr={rm(list = ls())}, add = TRUE)
  # Use the prefix 'buildProfile' with the provided suffix to get the function details
  details <- get_function_details(".buildPlot", suffix)
  func_name <- details$name
  expected_params <- details$params

  # Collect the additional arguments
  args <- list(...)
  arg_names <- names(args)

  # Check if all expected parameters are provided
  if (!all(expected_params %in% arg_names)) {
    missing_params <- setdiff(expected_params, arg_names)
    stop("Missing parameters for ", func_name, ": ", paste(missing_params, collapse = ", "), ".")
  }

  # Check for any extra parameters not expected
  if (!all(arg_names %in% expected_params)) {
    extra_params <- setdiff(arg_names, expected_params)
    stop("Extra parameters provided that are not required for ", func_name, ": ", paste(extra_params, collapse = ", "), ".")
  }

  # Call the matching function with the provided arguments
  do.call(func_name, args)
}

# Example usage:
# Assuming the case-specific functions have been defined...
# buildPlot("AEPSaByQ", x = 1, y = 2, z = 3)
# buildPlot("Dissagg3D", y = 2, z = 3, w = 4, m = 5)



.buildPlot_AEPSaByQ<- function(x,pID=NULL,Tn=NULL,pID_SET=NULL,SN,type="highcharter"){
  AEPTable <- x
  Tn_TARGET <- Tn
  SN_TARGET <- SN
  pID_TARGET <- pID
  if(is.null(Tn_TARGET)) Tn_TARGET <- 0
  if(is.null(pID_SET)) pID_SET <- AEPTable$pID |> unique()
  if(is.null(pID_TARGET)) pID_TARGET <- "+50%"

  DATA <- AEPTable[SN == SN_TARGET & Tn==Tn_TARGET &  (pID %in% pID_SET),list(AEP,Sa,pID)][order(pID)][Sa>0.0001]


  setnames(DATA,old=c("Sa","AEP","pID"),new=c("X","Y","ID"))
  pIDfix <- .pidFIX(pID_TARGET)

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: Tn=%3.2f s. SC=BC",Tn_TARGET),
            XT="Sa [g]",
            YT="AEP [1/yr]",
            YLOG=any("AEP" == input$logScale),
            XLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID=pID_TARGET,
            FILE=paste0("phsaAEPSa_byQ_",pIDfix,"_Plot"),
            TITLE="PSHA: Ground Intensity - Annual Exceedance Probability (AEP)",
            TIP=paste0("ID: {point.series.name}","Sa(AEP,Tn): {point.x}  g","","AEP: {point.y} 1/yr",sep=" <br> "),
            ALIGN=input$plotAlign,LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE =input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}

.buildPlot_Dissagg3D<- function(x,SN,Tn,TR,TR_SET=NULL,R_MAX=500,M_MAX=9,type="highcharter"){
  DHT <- x
  SN_TARGET <- SN
  Tn_TARGET <- Tn
  TR_TARGET <- TR
  # if(is.null(pID_SET)) pID_SET <- AEPTable$pID |> unique()
  # if(is.null(pID_TARGET)) pID_TARGET <- "+50%"

  DATA <- DHT[SN %in% SN_TARGET & TR==TR_TARGET,list(Mw,R,p,Tn,SN)]
  DATA <- DATA[,list(p=approx(x=as.double(Tn),y=p,xout = Tn_TARGET,ties = "ordered")$y),by=.(Mw,R)]
  if(nrow(DATA)==0) return(NULL)
  BINS <- input$bins
  A3D <- .buildA3D(DATA,bins=input$bins,Rmax=R_MAX,Mmax=M_MAX)
  TITLE <- sprintf("Target Scenario: Tn=%3.2f s TR=%d yr",Tn_TARGET,TR_TARGET)
  PALETTE <- input$palette
  plot3D::hist3D(
    plot=TRUE,
    x=A3D$X,y=A3D$Y,z=A3D$Z,
    contour = FALSE,
    col=hcl.colors(BINS+1,palette = PALETTE),
    main = TITLE,
    along="xy",
    theta=as.integer(input$theta),
    phi=as.integer(input$phi),
    box=TRUE,
    axes=TRUE,label=TRUE, facets = TRUE,
    #nticks=5,
    xlab = "R [km]", ylab = "Mw", zlab = "POE",
    ticktype="detailed",
    #lighting=list(ambient = 0.8),
    shade=0.75,
    space=0,
    d = 1
  )
}

.buildPlot_SaTnByNEHRP <- function(x,pID,pID_SET=NULL,SN,TR,TR_SET=NULL, SID,SID_SET=NULL,type="highcharter"){
  SaTR <- x
  SN_TARGET <- SN
  TR_TARGET <- TR
  pID_TARGET <- pID
  SID_TARGET <- SID

  if(is.null(pID_SET)) pID_SET <- SaTR$pID |> unique()
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- SaTR$TR |> unique()
  DATA <- SaTR[pID==pID_TARGET & SN == SN_TARGET & TR== TR_TARGET & SID %in% SID_SET][,list(Tn,Sa,SID)]

  setnames(DATA,old=c("Tn","Sa","SID"),new=c("X","Y","ID"))
  pIDfix <- .pidFIX(pID_TARGET)
  # --

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: TR=%d years. p=%s",TR_TARGET,pID_TARGET),
            YT="Sa(Tn) [g]",
            XT="Tn [s]",
            XLOG=any("Tn" == input$logScale),
            YLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID=SID_TARGET,
            FILE=paste0("SaTnByNEHRP","_",pIDfix,"_Plot"),
            TITLE="PSHA: Spectral Ordinates Sa(Tn)",
            TIP="ID: {point.series.name}<br>Tn: {point.x} s<br><br>Sa(Tn): {point.y} g",
            ALIGN=input$plotAlign,
            LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE = input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}

.buildPlot_SaTnByTR <- function(x,pID,pID_SET=NULL,SN,TR=NULL,TR_SET=NULL, SID,SID_SET=NULL,type="highcharter"){
  SaTR <- x
  SN_TARGET <- SN
  pID_TARGET <- pID
  SID_TARGET <- SID
  if(is.null(TR)){
    TR <- 2500
  }
  TR_TARGET <- TR
  if(is.null(pID_SET)) pID_SET <- SaTR$pID |> unique()
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- SaTR$TR |> unique()
  # DATA <- SaTR[pID==pID_TARGET & SN == SN_TARGET & SID== SID_TARGET][,list(Tn,Sa,TR)]
  DATA <- SaTR[pID==pID_TARGET & SN == SN_TARGET & SID== SID_TARGET & TR %in% TR_SET][,list(Tn,Sa,TR)]

  setnames(DATA,old=c("Tn","Sa","TR"),new=c("X","Y","ID"))
  pIDfix <- .pidFIX(pID_TARGET)
  # --

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: SC=%s  p=%s",SID_TARGET,pID_TARGET),
            YT="Sa(Tn) [g]",
            XT="Tn [s]",
            XLOG=any("Tn" == input$logScale),
            YLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID=TR_TARGET,
            FILE=paste0("SaTnByTR","_",pIDfix,"_Plot"),
            TITLE="PSHA: Spectral Ordinates Sa(Tn)",
            TIP="ID: {point.series.name}<br>Tn: {point.x} s<br><br>Sa(Tn): {point.y} g",
            ALIGN=input$plotAlign,
            LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE = input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}

.buildPlot_SaTnByQ <- function(x,pID,pID_SET=NULL,SN,TR,TR_SET=NULL,SID,SID_SET=NULL,type="highcharter"){
  SaTR <- x
  SN_TARGET <- SN
  TR_TARGET <- TR
  pID_TARGET <- pID
  SID_TARGET <- SID

  if(is.null(pID_SET)) pID_SET <- SaTR$pID |> unique()
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- SaTR$TR |> unique()
  DATA <- SaTR[pID %in% pID_SET & SN == SN_TARGET & SID== SID_TARGET & TR==TR_TARGET][,list(Tn,Sa,pID)]

  setnames(DATA,old=c("Tn","Sa","pID"),new=c("X","Y","ID"))
  pIDfix <- .pidFIX(pID_TARGET)
  # --

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: TR=%d years.SC=%s",TR_TARGET,SID_TARGET),
            YT="Sa(Tn) [g]",
            XT="Tn [s]",
            XLOG=any("Tn" == input$logScale),
            YLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID=pID_TARGET,
            FILE=paste0("SaTnByTR","_",pIDfix,"_Plot"),
            TITLE="PSHA: Spectral Ordinates Sa(Tn)",
            TIP="ID: {point.series.name}<br>Tn: {point.x} s<br><br>Sa(Tn): {point.y} g",
            ALIGN=input$plotAlign,
            LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE = input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}

.buildPlot_SaTnByRC_OBE_CDA <- function(x,pID,pID_SET=NULL,SN, SID,SID_SET=NULL,type="highcharter"){
  SaTR <- x

  SN_TARGET <- SN
  pID_TARGET <- pID
  SID_TARGET <- SID
  if(is.null(pID_SET)) pID_SET <- SaTR$pID |> unique()
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(!exists("CDA")) .setStandardsAEP()
  AEP <- CDA[,list(RC,Category,TR=TR_OBE)]

  # set DATA ----
  DT <- SaTR[pID==pID_TARGET & SN == SN_TARGET & SID == SID_TARGET]
  DATA <- NULL
  for(rc in seq_along(AEP$RC)){
    AUX <- DT[TR==AEP[RC==rc]$TR,list(Tn,Sa,RC=rc)]
    DATA <- rbindlist(list(DATA,AUX))
  }
  DATA <- AEP[,list(RC,Category)][DATA,on="RC"]
  pIDfix <- .pidFIX(pID_TARGET)
  setnames(DATA,old=c("Tn","Sa","Category"),new=c("X","Y","ID"))

  # plot ----

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: SC=%s p=%s values",SID_TARGET,pID_TARGET),
            YT="Sa(Tn) [g]",
            XT="Tn [s]",
            XLOG=any("Tn" == input$logScale),
            YLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID="Low",
            FILE=paste0("phsaSaTn_byTR_",input$SID_TARGET,"_",pIDfix,"_Plot"),
            TITLE="Spectral Ordinates Sa(Tn,SC) - Active Care (OBE)",
            TIP="ID: {point.series.name}<br>Tn: {point.x} s<br><br>Sa(Tn): {point.y} g",
            ALIGN=input$plotAlign,LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE = input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}
.buildPlot_SaTnByRC_MDE_CDA <- function(x,pID,pID_SET=NULL,SN,SID,SID_SET=NULL,type="highcharter"){
  SaTR <- x
  SN_TARGET <- SN
  pID_TARGET <- pID
  SID_TARGET <- SID
  if(is.null(pID_SET)) pID_SET <- SaTR$pID |> unique()
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(!exists("CDA")) .setStandardsAEP()
  AEP <- CDA[,list(RC,Category,TR=TR_MDE)]

  # set DATA ----
  DT <- SaTR[pID==pID_TARGET & SN == SN_TARGET & SID == SID_TARGET]
  DATA <- NULL
  for(rc in seq_along(AEP$RC)){
    AUX <- DT[TR==AEP[RC==rc]$TR,list(Tn,Sa,RC=rc)]
    DATA <- rbindlist(list(DATA,AUX))
  }
  DATA <- AEP[,list(RC,Category)][DATA,on="RC"]
  pIDfix <- .pidFIX(pID_TARGET)
  setnames(DATA,old=c("Tn","Sa","Category"),new=c("X","Y","ID"))

  # plot ----

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: SC=%s p=%s values",SID_TARGET,pID_TARGET),
            YT="Sa(Tn) [g]",
            XT="Tn [s]",
            XLOG=any("Tn" == input$logScale),
            YLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID="Low",
            FILE=paste0("phsaSaTn_byTR_",input$SID_TARGET,"_",pIDfix,"_Plot"),
            TITLE="Spectral Ordinates Sa(Tn,SC) - Passive Care (MDE)",
            TIP="ID: {point.series.name}<br>Tn: {point.x} s<br><br>Sa(Tn): {point.y} g",
            ALIGN=input$plotAlign,LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE = input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}

.buildPlot_SaTnByRC_OBE_GISTM <- function(x,pID,pID_SET=NULL,SN,SID,SID_SET=NULL,type="highcharter"){
  SaTR <- x
  SN_TARGET <- SN
  pID_TARGET <- pID
  SID_TARGET <- SID
  if(is.null(pID_SET)) pID_SET <- SaTR$pID |> unique()
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(!exists("GISTM")) .setStandardsAEP()
  AEP <- GISTM[,list(RC,Category,TR=TR_OBE)]

  # set DATA ----
  DT <- SaTR[pID==pID_TARGET & SN == SN_TARGET & SID == SID_TARGET]
  DATA <- NULL
  for(rc in seq_along(AEP$RC)){
    AUX <- DT[TR==AEP[RC==rc]$TR,list(Tn,Sa,RC=rc)]
    DATA <- rbindlist(list(DATA,AUX))
  }
  DATA <- AEP[,list(RC,Category)][DATA,on="RC"]
  pIDfix <- .pidFIX(pID_TARGET)
  setnames(DATA,old=c("Tn","Sa","Category"),new=c("X","Y","ID"))

  # plot ----

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: SC=%s p=%s values",SID_TARGET,pID_TARGET),
            YT="Sa(Tn) [g]",
            XT="Tn [s]",
            XLOG=any("Tn" == input$logScale),
            YLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID="Low",
            FILE=paste0("phsaSaTn_byTR_",input$SID_TARGET,"_",pIDfix,"_Plot"),
            TITLE="Spectral Ordinates Sa(Tn,SC) - Active Care (OBE)",
            TIP="ID: {point.series.name}<br>Tn: {point.x} s<br><br>Sa(Tn): {point.y} g",
            ALIGN=input$plotAlign,LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE = input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}
.buildPlot_SaTnByRC_MDE_GISTM <- function(x,pID,pID_SET=NULL,SN, SID,SID_SET=NULL,type="highcharter"){
  SaTR <- x
  SN_TARGET <- SN
  pID_TARGET <- pID
  SID_TARGET <- SID
  if(is.null(pID_SET)) pID_SET <- SaTR$pID |> unique()
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(!exists("GISTM")) .setStandardsAEP()
  AEP <- GISTM[,list(RC,Category,TR=TR_MDE)]

  # set DATA ----
  DT <- SaTR[pID==pID_TARGET & SN == SN_TARGET & SID == SID_TARGET]
  DATA <- NULL
  for(rc in seq_along(AEP$RC)){
    AUX <- DT[TR==AEP[RC==rc]$TR,list(Tn,Sa,RC=rc)]
    DATA <- rbindlist(list(DATA,AUX))
  }
  DATA <- AEP[,list(RC,Category)][DATA,on="RC"]
  pIDfix <- .pidFIX(pID_TARGET)
  setnames(DATA,old=c("Tn","Sa","Category"),new=c("X","Y","ID"))

  # plot ----

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: SC=%s p=%s values",SID_TARGET,pID_TARGET),
            YT="Sa(Tn) [g]",
            XT="Tn [s]",
            XLOG=any("Tn" == input$logScale),
            YLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID="Low",
            FILE=paste0("phsaSaTn_byTR_",input$SID_TARGET,"_",pIDfix,"_Plot"),
            TITLE="Spectral Ordinates Sa(Tn,SC) - Passive Care (MDE)",
            TIP="ID: {point.series.name}<br>Tn: {point.x} s<br><br>Sa(Tn): {point.y} g",
            ALIGN=input$plotAlign,LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE = input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}

.buildPlot_SaTnByRC_OBE_ANCOLD <- function(x,pID,pID_SET=NULL,SN, SID,SID_SET=NULL,type="highcharter"){
  SaTR <- x
  SN_TARGET <- SN
  pID_TARGET <- pID
  SID_TARGET <- SID
  if(is.null(pID_SET)) pID_SET <- SaTR$pID |> unique()
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(!exists("ANCOLD")) .setStandardsAEP()
  AEP <- ANCOLD[,list(RC,Category,TR=TR_OBE)]

  # set DATA ----
  DT <- SaTR[pID==pID_TARGET & SN == SN_TARGET & SID == SID_TARGET]
  DATA <- NULL
  for(rc in seq_along(AEP$RC)){
    AUX <- DT[TR==AEP[RC==rc]$TR,list(Tn,Sa,RC=rc)]
    DATA <- rbindlist(list(DATA,AUX))
  }
  DATA <- AEP[,list(RC,Category)][DATA,on="RC"]
  pIDfix <- .pidFIX(pID_TARGET)
  setnames(DATA,old=c("Tn","Sa","Category"),new=c("X","Y","ID"))

  # plot ----

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: SC=%s p=%s values",SID_TARGET,pID_TARGET),
            YT="Sa(Tn) [g]",
            XT="Tn [s]",
            XLOG=any("Tn" == input$logScale),
            YLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID="Low",
            FILE=paste0("phsaSaTn_byTR_",input$SID_TARGET,"_",pIDfix,"_Plot"),
            TITLE="Spectral Ordinates Sa(Tn,SC) - Active Care (OBE)",
            TIP="ID: {point.series.name}<br>Tn: {point.x} s<br><br>Sa(Tn): {point.y} g",
            ALIGN=input$plotAlign,LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE = input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}
.buildPlot_SaTnByRC_MDE_ANCOLD <- function(x,pID,pID_SET=NULL,SN, SID,SID_SET=NULL,type="highcharter"){
  SaTR <- x
  SN_TARGET <- SN
  pID_TARGET <- pID
  SID_TARGET <- SID
  if(is.null(pID_SET)) pID_SET <- SaTR$pID |> unique()
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(!exists("ANCOLD")) .setStandardsAEP()
  AEP <- ANCOLD[,list(RC,Category,TR=TR_MDE)]

  # set DATA ----
  DT <- SaTR[pID==pID_TARGET & SN == SN_TARGET & SID == SID_TARGET]
  DATA <- NULL
  for(rc in seq_along(AEP$RC)){
    AUX <- DT[TR==AEP[RC==rc]$TR,list(Tn,Sa,RC=rc)]
    DATA <- rbindlist(list(DATA,AUX))
  }
  DATA <- AEP[,list(RC,Category)][DATA,on="RC"]
  pIDfix <- .pidFIX(pID_TARGET)
  setnames(DATA,old=c("Tn","Sa","Category"),new=c("X","Y","ID"))

  # plot ----

  .PlotData(type,
            DATA,
            CAPTION=sprintf("Target Scenario: SC=%s p=%s values",SID_TARGET,pID_TARGET),
            YT="Sa(Tn) [g]",
            XT="Tn [s]",
            XLOG=any("Tn" == input$logScale),
            YLOG=any("Sa" == input$logScale),
            CR=CREDITS,
            MID="Low",
            FILE=paste0("phsaSaTn_byTR_",input$SID_TARGET,"_",pIDfix,"_Plot"),
            TITLE="Spectral Ordinates Sa(Tn,SC) - Passive Care (MDE)",
            TIP="ID: {point.series.name}<br>Tn: {point.x} s<br><br>Sa(Tn): {point.y} g",
            ALIGN=input$plotAlign,LAYOUT=input$plotLayout,
            LT=input$plotType,
            PALETTE = input$palette,
            THEME = HC_THEMES[[match(input$HCT,HC_LIST)]]
  )

}



# options(shiny.maxRequestSize = 1000 * 1024^2)
.buildA3D <- function(DT=NULL,Rmax=NULL,Mmax=NULL,bins=20){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  stopifnot(!is.null(DT),nrow(DT)>0)
  Mmax <- ifelse(is.null(Mmax),max(DT$Mw),min(max(DT$Mw),Mmax))
  Rmax <- ifelse(is.null(Rmax),max(DT$R),min(max(DT$R),Rmax))

  R_GRID <- seq(from=min(DT$R),to=Rmax,length.out=bins)
  # Mw_GRID <- seq(from=min(DT$Mw),to=Mmax,length.out=length(R_GRID))
  Mw_GRID <- seq(from=min(DT$Mw),to=Mmax,length.out=bins)
  DT <- DT[,list(
    R=approx(x=R,y=p,xout=R_GRID,ties = "ordered")$x,
    p=approx(x=R,y=p,xout=R_GRID,ties = "ordered")$y),by=.(Mw)]

  DT <- DT[,list(
    Mw=approx(x=Mw,y=p,xout=Mw_GRID,ties = "ordered")$x,
    p=approx(x=Mw,y=p,xout=Mw_GRID,ties = "ordered")$y),by=.(R)]


  X = DT$R |> unique()#c(1,2,3,4,5)
  Y = DT$Mw |> unique() #c(1,2,3,4,5)
  z <- NULL
  for(x in X){
    for(y in Y){
      z <- c(z,DT[R==x & Mw==y]$p  )
    }
  }
  NR <- length(X)
  NC <- length(Y)
  Z <-  matrix(z, nrow=NR, ncol=NC, byrow=TRUE)
  # get modes
  Mm <- DT$Mw[which.max(DT$p)]
  Rm <-  DT$R[which.max(DT$p)]
  A3D <- list(X=X,Y=Y,Z=Z,Mm=Mm,Rm=Rm)
  return(A3D)
}

.PlotData <- function(DATA=NULL,XT="X Values",YT="Y Values",MID=NULL,FILE=NULL,TITLE=NULL,TIP=NULL,LT=plotType_DEFAULT,PALETTE=NULL,THEME=NULL,CR=NULL,XLOG=TRUE,YLOG=FALSE,CAPTION=NULL,LAYOUT="horizontal",ALIGN="left",XMAX=NULL,YMAX=NULL,type="highcharter"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  stopifnot(!is.null(DATA),!is.null(MID))
  HC <- highchart() |>
    hc_yAxis(
      title= list(text=YT),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = FALSE,
      showLastLabel = TRUE) |>

    hc_xAxis(
      title= list(text=XT),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = TRUE,
      showLastLabel = TRUE) |>

    hc_add_series(
      DATA[ID==MID],# main curve
      type=LT,
      dashStyle = "Solid",
      hcaes(x=X,y=Y, group=ID)) |>

    hc_add_series(
      DATA[ ID != MID],# secondary curves
      type=LT,
      dashStyle = "ShortDashDotDot",
      hcaes(x=X,y=Y, group=ID)) |>

    hc_chart(style=list(fontFamily = "Helvetica")) |>

    hc_pane(size = "200%")

  # hc_size(height=800,width=600)


  if(!is.null(TIP)){
    HC <- HC |>  hc_tooltip(
      sort = FALSE, split=FALSE, crosshairs = TRUE, pointFormat = TIP)
  }

  if(!is.null(CAPTION)){
    HC <- HC |>   hc_caption(text = CAPTION, verticalAlign="top", align="left")
  }

  if(!is.null(CR)){
    HC <- HC |>    hc_credits(enabled = TRUE, text = CR)
  }

  if(!is.null(FILE)){
    HC <- HC |>    hc_exporting(
      enabled = TRUE, # always enabled
      filename = FILE)
  }

  if(!is.null(LAYOUT) & !is.null(ALIGN) ){
    XA <- ifelse(ALIGN=="left",+50,-50)
    YA <- ifelse(LAYOUT=="horizontal",50,0)
    HC <- HC |>
      hc_legend(
        layout = LAYOUT,
        align = ALIGN,
        verticalAlign="top",
        floating = TRUE,
        x=XA,y=YA)
  }

  if(!is.null(TITLE)){
    HC <- HC |>    hc_title(text = TITLE)
  }

  if(!is.null(THEME)){
    HC <- HC |>   hc_add_theme(hc_thm = THEME)
  }

  if(!is.null(PALETTE)){
    HC <- HC |>  hc_colors(colors = hcl.colors(10,palette = PALETTE))
  }

  if(!is.null(XMAX)){
    HC <- HC |> hc_xAxis(max = XMAX)
  }
  if(!is.null(YMAX)){
    HC <- HC |> hc_yAxis(max = YMAX)
  }
  if(XLOG==TRUE) {
    HC <- HC |> hc_xAxis(type = "logarithmic")
  }

  if(YLOG==TRUE) {
    HC <- HC |> hc_yAxis(type = "logarithmic")
  }
  return(HC)
}

