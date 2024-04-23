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

  DT <-  DT[, .(hs = round(sum(hs)/Hs*100)), by = UID]
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

