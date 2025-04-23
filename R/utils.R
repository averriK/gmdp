#' @title Extract Quantile or Mean from OpenQuake Header
#'
#' @description
#' Parses a header line (e.g. from an OpenQuake hazard CSV) to detect either
#' `kind='quantile-0.xxx'` or `kind='mean'`.
#'
#' @param line Character string (usually the first line read from a CSV).
#'
#' @return A numeric quantile (e.g. 0.16, 0.5, 0.84) or the character "mean".
#'   Returns \code{NA} if nothing is found.
#'
#' @keywords internal
#' @export
.extractQuantileFromHeader <- function(line) {
  if (grepl("kind='mean'", line)) {
    return("mean")
  }
  mt <- regexpr("kind='quantile-([0-9\\.]+)'", line)
  if (mt > 0) {
    val  <- regmatches(line, mt)
    qval <- sub("kind='quantile-", "", val)
    qval <- sub("'", "", qval)
    return(suppressWarnings(as.numeric(qval)))
  }
  return(NA)
}


#' @title Extract Investigation Time from OpenQuake Header
#'
#' @description
#' Parses a header line (e.g. from an OpenQuake hazard CSV) for `investigation_time=XX.xx`.
#'
#' @param line Character string (e.g. the first line of the CSV).
#'
#' @return A numeric value of the investigation time if found, otherwise NA_real_.
#'
#' @keywords internal
#' @export
.extractInvestigationTime <- function(line) {
  mt <- regexpr("investigation_time=([0-9\\.]+)", line)
  if (mt > 0) {
    val <- regmatches(line, mt)
    num <- sub("investigation_time=", "", val)
    return(as.numeric(num))
  }
  return(NA_real_)
}


#' @title Extract Tn from OpenQuake Header
#'
#' @description
#' Detects `imt='PGA'`, `imt='PGV'`, or `imt='SA(...)'` in a header line
#' to determine the spectral period, with 0 for PGA and -1 for PGV.
#'
#' @param line Character string from the CSV header.
#'
#' @return Numeric Tn in seconds, 0 (PGA) if `imt='PGA'`, -1 if `imt='PGV'`, or NA if not found.
#'
#' @keywords internal
#' @export
.extractTnFromHeader <- function(line) {
  if (grepl("imt='PGA'", line)) {
    return(0)
  }
  if (grepl("imt='PGV'", line)) {
    return(-1)
  }
  mt <- regexpr("imt='SA\\(([0-9\\.]+)\\)'", line)
  if (mt > 0) {
    val <- regmatches(line, mt)
    # e.g. "imt='SA(0.12)'"
    num <- sub("imt='SA\\(", "", val)
    num <- sub("\\)'", "", num)
    return(as.numeric(num))
  }
  return(NA_real_)
}

#' @title Parse UHS Column Name (Internal)
#'
#' @description
#' A helper that splits a column name like `0.095160~PGA` or `0.048770~SA(0.2)`
#' into numeric rate and Tn.
#'
#' @param tag Character, e.g. `"0.095160~SA(0.2)"`.
#' @return A two-element list with `(rate, Tn)`. Tn=0 if it detects "PGA."
#'
#' @keywords internal
#' @export
.parseUHScolumn <- function(tag) {
  parts <- strsplit(tag, "~")[[1]]
  if (length(parts) < 2) return(list(NA_real_, NA_real_))

  rate_str <- parts[1]
  spec_str <- parts[2]

  # parse rate
  rate_val <- suppressWarnings(as.numeric(rate_str))
  if (grepl("PGA", spec_str, ignore.case=TRUE)) {
    return(list(rate_val, 0))
  }
  mt <- regexpr("SA\\(([0-9\\.]+)\\)", spec_str)
  if (mt > 0) {
    val <- regmatches(spec_str, mt)
    tn_str <- sub("SA\\(", "", val)
    tn_str <- sub("\\)", "", tn_str)
    return(list(rate_val, as.numeric(tn_str)))
  }
  return(list(rate_val, NA_real_))
}
#' @noRd
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


#' @title Build Param-Based Hazard
#' @description Creates new hazard rows from fitted coefficients (a,b,c).
#' @noRd
buildParamHaz <- function(fitDT, AEPTable) {
  # Example: build param-based hazard expansions for TR=100..10000
  ITo <- unique(AEPTable$ITo)[1]
  TRseq <- seq(100,10000,25)
  fitDT[
    ,
    {
      Sa_calc  <- exp(a + b*log(TRseq) + c*(1/TRseq))
      AEP_approx<- 1 / TRseq
      POE_approx<- 1 - exp(-ITo*(1/TRseq))
      data.table(
        TR=TRseq, Sa=Sa_calc, AEP=AEP_approx,
        POE=POE_approx, ITo=ITo
      )
    },
    by=.(lat, lon, depth, p, Tn, a, b, c, sdLnA, R2, MSE, RMSE, fit)
  ]
}


#' @noRd
buildParamUHS <- function(fitDT, AEPTable) {
  # Example: build param-based UHS for some typical TR
  ITo <- unique(AEPTable$ITo)[1]
  TRo <- c(seq(100,10000,25), 475,975,2475,5000,10000)
  TRo <- unique(TRo)
  fitDT[
    ,
    {
      Sa_calc  <- exp(a + b*log(TRo) + c*(1/TRo))
      AEP_approx<- 1 / TRo
      POE_approx<- 1 - exp(-ITo*(1/TRo))
      data.table(
        TR=TRo, Sa=Sa_calc, AEP=AEP_approx,
        POE=POE_approx, ITo=ITo
      )
    },
    by=.(lat, lon, depth, p, Tn)
  ]
}

#' @noRd
applySiteAmp <- function(dt, vs30vec, vref, quantAF) {
  # dt is either AEPTable or UHSTable with columns like:
  #   (lat, lon, depth, p, Tn, Sa, TR, etc.)

  # 1) Determine the grouping columns (lat, lon, depth, p, and TR if it exists)
  grouping <- c("lat","lon","depth","p")
  if ("TR" %in% names(dt)) {
    grouping <- c(grouping, "TR")
  }

  # We'll collect final AF results in resAF
  resAF <- data.table()

  for (Vs in vs30vec) {
    # (A) Identify Tn=0 rows in dt, and pick exactly one row per grouping
    #     e.g. if multiple Tn=0 rows exist, we pick the one with the largest Sa.
    #     This avoids duplicates => no cartesian blow-up.
    pgaKey <- dt[Tn == 0,
                 .(PGA = Sa[which.max(Sa)]),  # or Sa[1L] if you prefer the first
                 by = grouping
    ]
    # If no Tn=0 exist, pgaKey is empty => the merge will just yield NA for PGA below

    # (B) Merge dt with pgaKey on these grouping columns
    tmp <- merge(
      dt,
      pgaKey,
      by       = grouping,
      all.x    = TRUE
      # no allow.cartesian=TRUE => we do not want cartesian expansions
    )

    # (C) Now fit site amplification for each group (including Tn, because
    #     dsra::fitModel.AF.TR is done by= grouping that includes Tn).
    #     But note that in the data, Tn is outside "grouping" above,
    #     so we just add it here:
    full_group <- c(grouping, "Tn")  # Tn is not in "grouping" for pgaKey

    AFdt <- tmp[
      ,
      dsra::fitModel.AF.TR(
        .x   = .SD,
        pga  = PGA,         # from the merged column
        q    = quantAF,
        Tn   = Tn,
        vs30 = Vs,
        vref = vref
      ),
      by = full_group
    ]

    # Accumulate results
    resAF <- rbind(resAF, AFdt, fill=TRUE)
  }

  return(resAF)
}



#' @noRd
mergeAF <- function(dt, AFdt) {
  # Merge the AF factor back to multiply Sa
  keepC <- c("Vref","Vs30","lat","lon","depth","p","Tn","AF","sdLnAF","PGA")
  AFuniq <- unique(AFdt[, ..keepC])
  joinC  <- intersect(names(dt), names(AFuniq))
  dt2 <- AFuniq[dt, on=joinC][
    ,
    `:=`(
      Sa      = AF * Sa,
      siteAmp = TRUE
    )
  ]
  dt2
}
