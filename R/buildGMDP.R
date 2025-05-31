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
  AEP_res   <- buildAEPTable(path = path, engine = engine, vref = vref)
  AEPTable  <- AEP_res$AEPTable
  temp_dir  <- AEP_res$temp_dir                   # only meaningful for OQ
  ITo       <- unique(AEPTable$ITo)[1]

  ## 2 ────────────────────────────────────────────────────────────────────
  ## Magnitude-distance disaggregation (optional)
  ## ---------------------------------------------------------------------
  RMwTable  <- buildMwTable(temp_dir = temp_dir,
                            engine   = engine,
                            ITo      = ITo,
                            vref     = vref)

  ## 3 ────────────────────────────────────────────────────────────────────
  ## Uniform-hazard spectra, optional site-response scaling
  ## ---------------------------------------------------------------------
  UHSTable  <- buildUHSTable(AEPTable = AEPTable,
                             vs30     = vs30,
                             vref     = vref,
                             TRo      = TRo,
                             NS       = NS)

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
