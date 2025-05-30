#' Build Ground-Motion Design Parameters (GMDP)
#'
#' @description
#' Orchestrates the full GMDP workflow by delegating to three focused
#' helpers:
#' \itemize{
#'   \item \code{\link{buildAEPTable}} – reads hazard curves and assembles an AEP table
#'   \item \code{\link{buildUHSTable}} – remeshes to uniform-hazard spectra and (optionally) performs site-response scaling
#'   \item \code{\link{buildMwTable}}  – imports magnitude–distance disaggregation (when available)
#' }
#' The function returns the same list structure as earlier versions, so
#' existing analysis scripts require no changes.
#'
#' @param IDo    Character. Identifier for the GMDP.  Default \code{"gmdp"}.
#' @param path   Character. Directory containing hazard data files.
#' @param engine Character. Source of hazard data, either \code{"openquake"}
#'               or \code{"user"}.  Default \code{"openquake"}.
#' @param vs30   Numeric vector. Target Vs30 values for site-response
#'               analysis. If \code{NULL}, no site response is performed.
#' @param vref   Numeric. Reference Vs30 value (m/s).  Default \code{760}.
#' @param TRo    Numeric vector. Target return periods for uniform-hazard
#'               spectra.  Default \code{seq(400, 10000, by = 25)}.
#' @param NS     Integer ≥ 1. Monte-Carlo samples per model.  Default \code{100}.
#'
#' @return A named \code{list} with elements
#'   \describe{
#'     \item{\strong{AEPTable}}{Annual-exceedance-probability table.}
#'     \item{\strong{UHSTable}}{Uniform-hazard-spectrum table.}
#'     \item{\strong{RMwTable}}{Magnitude-distance disaggregation table (or \code{NULL}).}
#'   }
#'
#' @examples
#' \dontrun{
#' result <- buildGMDP(
#'   path   = "path/to/oq/output",
#'   vs30   = c(300, 760),
#'   TRo    = seq(475, 2475, by = 100)
#' )
#' }
#'
#' @seealso
#'   \code{\link{buildAEPTable}}, \code{\link{buildUHSTable}},
#'   \code{\link{buildMwTable}}
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom utils unzip
#' @export

buildGMDP <- function(path,
                      IDo    = "gmdp",
                      engine = "openquake",
                      vs30   = NULL,
                      vref   = 760,
                      TRo    = seq(400, 10000, by = 25),
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
  AEPTable[, ID := IDo]
  RMwTable[, ID := IDo]
  list(
    AEPTable = AEPTable,
    UHSTable = UHSTable,
    RMwTable = RMwTable
  )
}




# nolint end -----------------------------------------------------------------
