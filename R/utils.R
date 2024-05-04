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


.find <- function(V, X) {
  # Ensure V is sorted
  V <- sort(unique(V))
  X <- sort(unique(X))
  # Initialize the result vector
  results <- numeric(length(X))

  # Process each element in X
  for (i in seq_along(X)) {
    x <- X[i]

    # Check if x is exactly in V
    if (x %in% V) {
      results[i] <- x
    } else {
      # Find the closest indices before and after x
      idx_before <- max(which(V < x), na.rm = TRUE)
      idx_after <- min(which(V > x), na.rm = TRUE)

      # Handle edge cases when x is outside the range of V
      if (length(idx_before) == 0) { # x is less than all elements in V
        idx_before <- idx_after
      }
      if (length(idx_after) == 0) { # x is more than all elements in V
        idx_after <- idx_before
      }

      # Fetch the closest values
      # Choose the closer one or both if x is equally distant from V[idx_before] and V[idx_after]
      if (abs(V[idx_before] - x) < abs(V[idx_after] - x)) {
        results[i] <- V[idx_before]
      } else if (abs(V[idx_before] - x) > abs(V[idx_after] - x)) {
        results[i] <- V[idx_after]
      } else {
        # If both are equally close, return the average or any other logic you want
        results[i] <- mean(c(V[idx_before], V[idx_after]))
      }
    }
  }

  return(results)
}
