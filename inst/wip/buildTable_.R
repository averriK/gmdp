
buildTable <- function(suffix, ...) {
  on.exit(expr={rm(list = ls())}, add = TRUE)
  details <- get_function_details(".buildTable", suffix)
  func_name <- details$name
  expected_params <- details$params
  args <- list(...)
  arg_names <- names(args)
  if (!all(expected_params %in% arg_names)) {
    missing_params <- setdiff(expected_params, arg_names)
    stop("Missing parameters for ", func_name, ": ", paste(missing_params, collapse = ", "), ".")
  }
  if (!all(arg_names %in% expected_params)) {
    extra_params <- setdiff(arg_names, expected_params)
    stop("Extra parameters provided that are not required for ", func_name, ": ", paste(extra_params, collapse = ", "), ".")
  }
  do.call(func_name, args)
}

# Example usage:
# Assuming the case-specific functions have been defined...
# buildTable("AEPSaByQ", x = 1, y = 2, z = 3)
# buildTable("Dissagg3D", y = 2, z = 3, w = 4, m = 5)


# .buildTable_* CDA ------
.buildTable_Kmax.CDA.OBE <- function(x,SN,SID,Da,Ts){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- x
  OBE <- c(getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,SID_TARGET=SID)#100 yr
  )
  P <- c("mean","mean","mean","mean","mean")
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,"kmax [g]"=OBE)
  return(DT)
}

.buildTable_Kmax.CDA.MDE <- function(x,SN,SID,Da,Ts){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- x
  MDE <- c(getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,SID_TARGET=SID)
  )
  P <- c("+84%","mean","mean","mean","mean")
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,"kmax [g]"=MDE)
  return(DT)
}

.buildTable_Kh_CDA.OBE <- function(x,BM19TR,SN,SID,Da,Ts,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  OBE <- c(
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID)#100 yr
  )
  P <- c(pID,pID,pID,pID,pID)
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,"kh [%]"=OBE)
  return(DT)
}

.buildTable_Kh_CDA.MDE <- function(x,BM19TR,SN,SID,Da,Ts,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  MDE <- c(
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID)
  )

  P <- c(pID,pID,pID,pID,pID)
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,"kh [%]"=MDE)
  return(DT)
}

.buildTable_PGA_CDA.OBE <- function(x,SN,SID,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  OBE <- c(getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=5000,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=2500,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID) #100 yr
  )
  P <- c(pID,pID,pID,pID,pID)
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,PGA=OBE,pID=P)
  return(DT)
}


.buildTable_PGA_CDA.MDE <- function(c,SN,SID,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  MDE <- c(
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=5000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=2500,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID)
  )
  P <- c(pID,pID,pID,pID,pID)
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,PGA=MDE,pID=P)
  return(DT)
}

# .buildTable_* GISTM ------
.buildTable_Kmax.GISTM_OBE <- function(y,SN,SID,Da,Ts){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  OBE <- c(getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,SID_TARGET=SID)#100 yr
  )
  P <- c("mean","mean","mean","mean","mean")
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,"kmax [g]"=OBE)
  return(DT)
}

.buildTable_Kmax.GISTM_MDE <- function(y,SN,SID,Da,Ts){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  MDE <- c(getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID)
  )
  P <- c("+84%","mean","mean","mean","mean")
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,"kmax [g]"=MDE)
  return(DT)
}

.buildTable_Kh_GISTM_OBE <- function(x,y,SN,SID,Da,Ts,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  SaTR <- x
  OBE <- c(
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID)#100 yr
  )
  P <- c(pID,pID,pID,pID,pID)
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,"kh [%]"=OBE)
  return(DT)
}

.buildTable_Kh_GISTM_MDE <- function(x,y,SN,SID,Da,Ts,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  SaTR <- x
  MDE <- c(
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID)
  )

  P <- c(pID,pID,pID,pID,pID)
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,"kh [%]"=MDE)
  return(DT)
}

.buildTable_PGA_GISTM_OBE <- function(x,SN,SID,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  OBE <- c(getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=5000,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=2500,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID) #100 yr
  )
  P <- c(pID,pID,pID,pID,pID)
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,PGA=OBE,pID=P)
  return(DT)
}

.buildTable_PGA_GISTM_MDE <- function(x,SN,SID,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  MDE <- c(
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID)
  )
  P <- c(pID,pID,pID,pID,pID)
  CAT <- c("Extreme","Very High","High","Significant","Low")
  DT <- data.table("Category"=CAT,PGA=MDE,pID=P)
  return(DT)
}

# .buildTable_* ANCOLD ------

.buildTable_Kmax.ANCOLD_OBE <- function(y,SN,SID,Da,Ts){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  OBE <- c(getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,SID_TARGET=SID)
  )
  P <- c("mean","mean","mean","mean","mean","mean")
  DT <- data.table("Category"=c("Extreme","High A","High B","High C","Significant","Low"),
                   "kmax [g]"=OBE)
  return(DT)
}

.buildTable_Kmax.ANCOLD_MDE <- function(y,SN,SID,Da,Ts){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  MDE <- c(getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,SID_TARGET=SID),
           getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,SID_TARGET=SID)
  )
  P <- c("mean","mean","mean","mean","mean","mean")
  DT <- data.table("Category"=c("Extreme","High A","High B","High C","Significant","Low"),
                   "kmax [g]"=MDE)
  return(DT)
}

.buildTable_Kh_ANCOLD_OBE <- function(x,y,SN,SID,Da,Ts,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  SaTR <- x
  OBE <- c(getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET="+84%",SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID)
  )
  P <- c("+84%",pID,pID,pID,pID,pID)
  DT <- data.table("Category"=c("Extreme","High A","High B","High C","Significant","Low"),
                   "kh [%]"=OBE)
  return(DT)
}

.buildTable_Kh_ANCOLD_MDE <- function(x,y,SN,SID,Da,Ts,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  SaTR <- x
  MDE <- c(getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET="+84%",SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID)
  )
  P <- c("+84%",pID,pID,pID,pID,pID)
  DT <- data.table("Category"=c("Extreme","High A","High B","High C","Significant","Low"),
                   "kh [%]"=MDE)

  return(DT)
}

.buildTable_PGA_ANCOLD_OBE <- function(x,SN,SID,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  OBE <- c(getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET="+84%",SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID),
           getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID)
  )
  P <- c("+84%",pID,pID,pID,pID,pID)
  DT <- data.table("Category"=c("Extreme","High A","High B","High C","Significant","Low"),
                   PGA=OBE,pID=P)
  return(DT)
}

.buildTable_PGA_ANCOLD_MDE <- function(x,SN,SID,pID="mean"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  MDE <- c(
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET="+84%",SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=5000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=2500,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
    getPGAbyTR(SaTR=SaTR,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID)
  )
  P <- c("+84%",pID,pID,pID,pID,pID)
  DT <- data.table("Category"=c("Extreme","High A","High B","High C","Significant","Low"),
                   PGA=MDE,pID=P)
  return(DT)
}

# .buildTable_* general ------

.buildTable_Kmax.TR <- function(y,SN,SID,Da,Ts){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y

  K <- c(getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,SID_TARGET=SID),
         getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,SID_TARGET=SID),
         getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,SID_TARGET=SID),
         getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,SID_TARGET=SID),
         getData("KmaxbyTR",BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,SID_TARGET=SID)
  )
  DT <- data.table("Category"=c("1:10000","1:5000","1:2500","1:1000","1:500"),
                   "kmax [g]"=K)
  return(DT)
}


.buildTable_Kh_TR <- function(x,y,SN,SID,Da,Ts,pID){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  SaTR <- x
  OBE <- c(getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=10000,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=5000,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=2500,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=1000,pID_TARGET=pID,SID_TARGET=SID),
           getData("KhbyTR",SaTR=SaTR,BM19TR=BM19TR,Ts_TARGET=Ts,Da_TARGET=Da,SN_TARGET=SN,TR_TARGET=500,pID_TARGET=pID,SID_TARGET=SID)
  )
  DT <- data.table("Category"=c("1:10000","1:5000","1:2500","1:1000","1:500"),
                   "kh [%]"=OBE)
  return(DT)
}


.buildTable_PGA <- function(x,SN,pID,SID_SET=NULL,TR_SET=NULL){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  SN_TARGET <- SN
  pID_TARGET <- pID
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- SaTR$TR |> unique()

  SA <-SaTR[pID==pID_TARGET & SN == SN_TARGET & Tn==0 &  SID %in% SID_SET & TR %in% TR_SET,list(
    PGA=round(Sa,digits = 3)),list(SID,TR)]
  DT <- reshape(SA,idvar =c("SID"), timevar = "TR", direction = "wide")
  setnames(DT,old = "SID",new="SC")
  COL <- colnames(DT)
  NCOL <- str_replace(COL,pattern = "PGA.",replacement = "TR=")
  setnames(DT,old = COL,new=NCOL)
  return(DT)
}

.buildTable_AFPGA <- function(x,pID,SID_SET,SN,TR_SET=NULL){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  SN_TARGET <- SN
  pID_TARGET <- pID
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- SaTR$TR |> unique()

  SA <- SaTR[pID==pID_TARGET & SN == SN_TARGET & Tn==0 & SID %in% SID_SET & TR %in% TR_SET,list(
    AF=round(AF,digits = 3)),list(SID,TR)]
  DT1 <- reshape(SA,idvar =c("SID"), timevar = "TR", direction = "wide")
  setnames(DT1,old = "SID",new="SC")
  COL <- colnames(DT1)
  NCOL <- str_replace(COL,pattern = "AF.",replacement = "TR=")
  setnames(DT1,old = COL,new=NCOL)
  return(DT1)
}


.buildTable_AFSa <- function(x,pID,SID_SET,SN,Tn,TR_SET=NULL){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SaTR <- x
  SN_TARGET <- SN
  pID_TARGET <- pID
  Tn_TARGET <- Tn
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- SaTR$TR |> unique()

  SA <- SaTR[pID==pID_TARGET & SN == SN_TARGET & Tn==Tn_TARGET & SID %in% SID_SET & TR %in% TR_SET,list(
    AF=round(AF,digits = 3)),list(SID,TR,Tn)]
  DT1 <- reshape(SA,idvar =c("SID","Tn"), timevar = "TR", direction = "wide")
  setnames(DT1,old = "SID",new="SC")
  COL <- colnames(DT1)
  NCOL <- str_replace(COL,pattern = "AF.",replacement = "TR=")
  setnames(DT1,old = COL,new=NCOL)
  return(DT1)
}

.buildTable_Sa <- function(x,Tn,pID,SID_SET=NULL,SN,TR_SET=NULL){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  SN_TARGET <- SN
  pID_TARGET <- pID
  Tn_TARGET <- Tn
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- SaTR$TR |> unique()
  SA <- .interpolateSaTR( t=Tn_TARGET,SaTR=SaTR[pID==pID_TARGET & SN == SN_TARGET & SID %in% SID_SET & TR %in% TR_SET])

  SA <- SA[,list(Sa=round(Sa,digits = 3)),list(SID,TR,Tn)]
  DT <- reshape(SA,idvar =c("SID","Tn"), timevar = "TR", direction = "wide")
  setnames(DT,old = "SID",new="SC")
  COL <- colnames(DT)
  NCOL <- str_replace(COL,pattern = "Sa.",replacement = "TR=")
  setnames(DT,old = COL,new=NCOL)
  return(DT)
}

.buildTable_MmRm <- function(z,Tn,BINS=60,SN){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  RMwTable <- z
  SN_TARGET <- SN
  Tn_TARGET <- Tn
  DATA <- RMwTable[SN == SN_TARGET,list(TR,Mw,R,p,Tn,SN)]
  DATA <- DATA[,list(p=approx(x=as.double(Tn),y=p,xout = as.double(Tn_TARGET),ties = mean)$y),by=.(TR,Mw,R,SN)]
  TR <- RMwTable$TR |> unique()
  DT <- data.table()
  for(tr in TR){
    if(nrow(DATA[TR==tr])>0){
      A3D <- .buildA3D(DATA[TR==tr],bins=BINS)
      DT <- rbindlist(list(DT,data.table(TR=tr,Tn=Tn_TARGET,SID="BC",Mm= round(A3D$Mm,2),Rm= round(A3D$Rm,0))))
    }

  }
  setnames(DT,old=c("SID","Rm"),new=c("NEHRP","Rm"))
  setcolorder(DT,c("NEHRP","Tn","TR"))
  return(DT)
}

.buildTable_Dn <- function(w,ky,Ts,pID,SID_SET,SN,TR_SET=NULL){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  DnTR <- w
  SN_TARGET <- SN
  Ts_TARGET <- Ts
  ky_TARGET <- ky
  pID_TARGET <- pID

  if(is.null(SID_SET)) SID_SET <- DnTR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- DnTR$TR |> unique()
  DT <- .interpolateDnTR(k=ky_TARGET,ts=Ts_TARGET,DnTR=DnTR[pID==pID_TARGET & SN == SN_TARGET & SID %in% SID_SET & TR %in% TR_SET])
  DT <- DT[,list(Dn=round(Dn,digits=1),TR,Ts,SID)]
  DT <- reshape(DT,idvar =c("SID","Ts"), timevar = "TR", direction = "wide")
  setnames(DT,old = c("SID"),new=c("NEHRP"))
  COL <- colnames(DT)
  NCOL <- str_replace(COL,pattern = "Dn.",replacement = "TR=")
  setnames(DT,old = COL,new=NCOL)
  setcolorder(DT,c("NEHRP","Ts"))

  return(DT)
}

.buildTable_Kmax <- function(y,Da,Ts,SID_SET=NULL,TR_SET=NULL,SN){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  SN_TARGET <- SN
  Ts_TARGET <- Ts
  Da_TARGET <- Da
  LnDa <- log(Da_TARGET)
  #kmax <- exp((-a+sqrt(a^2-0.98*(b+LnDa-e)))/0.49)
  if(is.null(SID_SET)) SID_SET <- BM19TR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- BM19TR$TR |> unique()
  DT <- .interpolateBM19TR(ts=Ts_TARGET,BM19TR=BM19TR[SN == SN_TARGET & SID %in% SID_SET & TR %in% TR_SET])
  DT <- DT[,list(Ts,TR,SID,Da=Da_TARGET,Kmax=round(exp((-a+sqrt(a^2-0.98*(b+LnDa-e)))/0.49),4))]
  DT <- reshape(DT,idvar =c("SID","Da","Ts"), timevar ="TR", direction = "wide")
  setnames(DT,old = c("SID"),new=c("NEHRP"))
  COL <- colnames(DT)
  NCOL <- str_replace(COL,pattern = "Kmax.",replacement = "TR=")
  setnames(DT,old = COL,new=NCOL)
  setcolorder(DT,c("NEHRP","Ts","Da"))
  return(DT)
}

.buildTable_Kh <- function(SN,y,x,Da,Ts,SID_SET=NULL,pID,TR_SET=NULL){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  BM19TR <- y
  SaTR <- x
  SN_TARGET <- SN
  Ts_TARGET <- Ts
  Da_TARGET <- Da
  pID_TARGET <- pID

  LnDa <- log(Da_TARGET)
  #kmax <- exp((-a+sqrt(a^2-0.98*(b+LnDa-e)))/0.49)
  if(is.null(SID_SET)) SID_SET <- SaTR$SID |> unique()
  if(is.null(TR_SET)) TR_SET <- SaTR$TR |> unique()
  DT <- .interpolateBM19TR(ts=Ts_TARGET,BM19TR=BM19TR[SN == SN_TARGET & SID %in% SID_SET & TR %in% TR_SET])
  DT <- DT[,list(Ts,TR,SID,Da=Da_TARGET,Kmax=round(exp((-a+sqrt(a^2-0.98*(b+LnDa-e)))/0.49),4))]
  SA <-SaTR[pID==pID_TARGET & SN == SN_TARGET & Tn==0 ,list(PGA=round(Sa,digits = 3)),list(SID,TR)]

  DT <- SA[DT,on=c("SID","TR")][,list(SID,TR,Ts,Da,Kh=round(100*Kmax/PGA,digits = 0))]
  DT <- reshape(DT,idvar =c("SID","Da","Ts"), timevar ="TR", direction = "wide")
  setnames(DT,old = c("SID"),new=c("NEHRP"))
  COL <- colnames(DT)
  NCOL <- str_replace(COL,pattern = "Kh.",replacement = "TR=")
  setnames(DT,old = COL,new=NCOL)
  setcolorder(DT,c("NEHRP","Ts","Da"))

  return(DT)
}
