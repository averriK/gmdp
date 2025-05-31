#' Internal helper for buildGMDP()
#' @keywords internal
#' @noRd



buildUHSTable <- function(AEPTable,
                          vs30 = NULL,
                          vref = 760,
                          TRo  = c(475,2475,4975,9975),
                          NS   = 100) {

  message("> Re-mesh hazard data on a uniform TR grid ...")
  COLS      <- setdiff(names(AEPTable), c("Sa", "POE", "AEP", "TR"))
  UHSTable  <- AEPTable[Tn != -1, remeshGroup(.SD, TRo), by = COLS]

  ## ── Optional site-response scaling ────────────────────────────────────
  if (!is.null(vs30) && vref %in% c(760, 3000)) {
    message("> Fit UHS site response for Vs30 = ", paste(vs30, collapse = ", "))

    SaFTable <- newmark::fitSaF(
      uhs  = UHSTable[, .(TR, Sa, Tn, p)],
      vs30 = vs30,
      NS   = NS,
      vref = vref
    )

    if ("Vs30" %in% names(UHSTable))
      UHSTable[, Vs30 := NULL]
    by_cols <- c("TR", "Tn", "p")
    data.table::setkeyv(UHSTable, by_cols)
    data.table::setkeyv(SaFTable,  by_cols)

    UHSTable <- SaFTable[UHSTable, allow.cartesian = TRUE][
      , `:=`(
        SaF  = ifelse(Vs30 == vref, Sa, SaF),
        AF   = ifelse(Vs30 == vref, 1, SaF / Sa),
        Sa   = SaF,
        Vref = vref
      )
    ][]
  } else if (is.null(vs30)) {
    ## Rock reference shortcut
    UHSTable[, `:=`(Vref = vref, Vs30 = vref, AF = 1, SaF = Sa)]
  }

  UHSTable[]
}
