#' @noRd
#'
#'


.runUSCS <- function(FILE,USCS,Hs,POP=0,NITER=10,W=0){
  DT <- data.table()
  for(pop in POP){
    for(hs in Hs){
      for(w in W){
        for(ITER in NITER){
          DSRA <- buildDSRA(Hs=hs,h=1.00,W=w,USCS=USCS,Group=NULL,POP=pop)
          if(!is.null(DSRA)){
            AUX <- DSRA$SiteProperties
            # print(AUX)
            DT <- rbindlist(list(DT,AUX),use.names = TRUE)
          }
        }
      }
    }
  }

  if(file.exists(FILE)){
    fwrite(DT,file=FILE,append = TRUE,yaml = TRUE)
  } else {
    fwrite(DT,file=FILE,yaml = TRUE)
  }
}


.runGROUP <- function(FILE,Group,Hs,W=0,POP=0,NITER=10){
  for(group in Group){
    DT <- data.table()
    for(pop in POP){
      for(hs in Hs){
        for(w in W){
          for(ITER in NITER){
            DSRA <- buildDSRA(Hs=hs,h=1.00,W=w,USCS = NULL,Group=group,POP=pop)
            if(!is.null(DSRA)){
              AUX <- DSRA$SiteProperties
              # print(AUX)
              DT <- rbindlist(list(DT,AUX),use.names = TRUE)
            }
          }
        }
      }
    }
    if(file.exists(FILE)){
      fwrite(DT,file=FILE,append = TRUE,yaml = TRUE)
    } else {
      fwrite(DT,file=FILE,yaml = TRUE)
    }

  }

}


.mapLayers <- function(UID,hs,USCS=ValidUSCS){

  DT <- data.table(UID,hs)
  Hs=sum(hs)

  DT <-  DT[, list(hs = round(sum(hs)/Hs*100)), by = UID]
  DT <- dcast(DT, formula = "1 ~ UID", value.var = "hs", fill = 0)
  DT[, (USCS) := lapply(USCS, function(u) ifelse(u %in% names(DT),DT[[u]],0))]
  DT <- DT[, .SD, .SDcols = USCS]
  return(DT)
}


.checkUSCS <- function(.x,UID,exact_match=FALSE){
  X <- str_split(UID,pattern = "[[.]]") |> unlist()
 if(exact_match){
   VALUE <- unname(all(X %in% .x)|> unname())
 } else {
   VALUE <- unname(all(.x %in% X)|> unname())
 }
  return(VALUE)
}

# > Z <- lapply(SiteTable$UID,function(U){.checkUSCS (.x=c("MH","CH","ML","CL"),U,exact_match=TRUE)})

.buildModel <- function(.data=SiteTable, .y,.newdata=NULL,level=c(0.16,0.5,0.84),.regression="qrf",...) {
  # Capture the variable arguments as a vector
  stopifnot(.y %in% names(.data))
  if(is.null(.newdata)){
    # .newdata==NULL: build model. Return model
    VARS <- as.character(list(...))
  }

  if(is.data.table(.newdata)){
    # .newdata!=NULL: build model, predict new data, return response
    VARS <- names(.newdata)
  }
  # Subset the data to include only valid columns
# Check and keep only those columns that are present in the data.table

  COLS <- VARS[VARS %in% names(.data)]
  X <- .data[, ..COLS, with = FALSE]
  # Remove rows with Zero values
  idx <- apply(X, 1, function(x) all(x > 0 & !is.na(x)))
  X <- X[idx, ]
  if(nrow(X) == 0) {
    stop("No valid data found")
  }

  Y <- .data[idx, ][[.y]]
  .model <- switch(.regression,
         "qrf"=quantregForest::quantregForest(x=X,y=Y,nthread=8,keep.inbag = FALSE),
         "rf"=randomForest::randomForest(x=X,y=Y,importance=FALSE,proximity=FALSE)
         )

  if(is.null(.newdata)){
    # .newdata==NULL: build model. Return model
    return(.model)
  }

  if(is.data.table(.newdata)){
    # .newdata!=NULL: build model, predict new data, return response
    VALUE <- predict(.model,newdata=.newdata, what = level)
    return(VALUE)
  }






  # Return the filtered data.table
  return()
}
