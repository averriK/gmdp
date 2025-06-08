#' Build Ground-Motion Design Parameters (GMDP)
#'
#' A thin wrapper that assembles an AEP table, a uniform-hazard‐spectrum
#' table, and—where available—a magnitude–distance disaggregation table.
#' The heavy lifting is delegated to three internal helpers
#' (`buildAEPTable()`, `buildUHSTable()`, `buildMwTable()`).
#'
#' @param IDo    Character. Identifier for the GMDP. Default `"gmdp"`.
#' @param path   Character. Directory with hazard data files.
#' @param engine Character. `"openquake"` ( default ) or `"user"`.
#' @param vs30   Numeric vector. Target Vs30 values; `NULL` skips site response.
#' @param vref   Numeric. Reference Vs30 (m/s). Default 760.
#' @param TRo    Numeric vector. Target return periods (years).
#' @param NS     Integer ≥ 1. Monte-Carlo samples per model. Default 100.
#'
#' @return A named list with `AEPTable`, `UHSTable`, and `RMwTable`
#'   (`NULL` if disaggregation is unavailable).
#'
#' @examples
#' \dontrun{
#' buildGMDP(path = "path/to/oq/output",
#'           vs30 = c(300, 760),
#'           TRo  = seq(475, 2475, by = 100))
#' }
#'
#' @export


buildGMDP <- function(path,
                      IDo    = "gmdp",
                      engine = "openquake",
                      vs30   = NULL,
                      vref   = 760,
                      TRo    = 2475,
                      NS     = 100) {

  ## 1 ────────────────────────────────────────────────────────────────────
  ## Annual-exceedance probability table
  ## ---------------------------------------------------------------------
  OUT   <- importAEPTable(path = path, engine = engine, vref = vref)
  AEPTable  <- OUT$AEPTable
  ITo       <- unique(AEPTable$ITo)[1]

  ## 2 ────────────────────────────────────────────────────────────────────
  ## Magnitude-distance disaggregation (optional)
  ## ---------------------------------------------------------------------

  if (engine != "openquake") {
    message("> Disaggregation not requested for engine = '", engine, "'.")
    return(NULL)
  }

  message("> Building Disaggregation Hazard Table...")
  OUT <- tryCatch(
    importModel.oqRMw(path = temp_dir, ITo = ITo, vref = vref),
    error = function(e) {
      message(">> Skipping disagg (", e$message, ")")
      NULL
    }
  )

  if (!is.null(OUT))
    RMwTable <- OUT[, `:=`(SID = Vs30toSID(vref), Vs30 = vref, SM = engine, IT = ITo)]
  else
    message("> Disaggregation data not available.")
  ## 3 ────────────────────────────────────────────────────────────────────
  ## Uniform-hazard spectra, optional site-response scaling
  ## ---------------------------------------------------------------------

  message("> Re-mesh hazard data on a uniform TR grid ...")
  COLS      <- setdiff(names(AEPTable), c("Sa", "POE", "AEP", "TR"))
  UHSTable  <- AEPTable[Tn != -1, remeshGroup(.SD, TRo), by = COLS]

  ## ── Optional site-response scaling ────────────────────────────────────
  UHSTable[, `:=`(Vref = vref, Vs30 = vref, AF = 1, SaF = Sa)]

  ## 4 ────────────────────────────────────────────────────────────────────
  ## Final bookkeeping
  ## ---------------------------------------------------------------------
  UHSTable[, ID := IDo]
  # AEPTable[, ID := IDo]
  # RMwTable[, ID := IDo]
  list(
    AEPTable = AEPTable,
    UHSTable = UHSTable,
    RMwTable = RMwTable
  )
}




# nolint end -----------------------------------------------------------------
