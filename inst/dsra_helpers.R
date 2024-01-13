
.runUSCS <- function(FILE,USCS_TARGET,Hs_SET,POP_SET=0){
  DT <- data.table()
  for(POP_TARGET in POP_SET){
    for(Hs_TARGET in Hs_SET){
      for(ITER in NITER){
        DSRA <- buildDSRA(Hs=Hs_TARGET,h=1.00,Hw=0,USCS=USCS_TARGET,Group=NULL,POP=POP_TARGET)
        if(!is.null(DSRA)){
          AUX <- DSRA$SiteProperties
          # print(AUX)
          DT <- rbindlist(list(DT,AUX),use.names = TRUE)
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


.runGROUP <- function(FILE,Group_SET,Hs_SET,POP_SET=0){
  for(Group_TARGET in Group_SET){
    DT <- data.table()
    for(POP_TARGET in POP_SET){
      for(Hs_TARGET in Hs_SET){

        for(ITER in NITER){
          DSRA <- buildDSRA(Hs=Hs_TARGET,h=1.00,Hw=0,USCS = NULL,Group=Group_TARGET,POP=POP_TARGET)
          if(!is.null(DSRA)){
            AUX <- DSRA$SiteProperties
            # print(AUX)
            DT <- rbindlist(list(DT,AUX),use.names = TRUE)
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

