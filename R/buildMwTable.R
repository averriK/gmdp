
#' Build a magnitude–distance (M–w) disaggregation table
#'
#' @description
#' For OpenQuake runs that include disaggregation output, this helper
#' imports the Mw–R histogram for a given intensity-measure level
#' (\code{ITo}) and tags it with basic metadata (\code{Vs30}, \code{SID},
#' etc.).
#' If the chosen hazard engine is not \code{"openquake"}, or if the
#' disaggregation files are missing, the function quietly returns
#' \code{NULL}.
#'
#' @param temp_dir \code{character}. Temporary directory produced by
#'                 \code{buildAEPTable()} that holds the extracted OQ
#'                 CSV/JSON files.
#' @param engine   \code{character}. Hazard engine identifier; only
#'                 \code{"openquake"} triggers any work.
#' @param ITo      \code{numeric}. Intensity-measure level for which the
#'                 disaggregation is requested.
#' @param vref     \code{numeric}. Reference Vs30 (m/s) used to derive the
#'                 site identifier (\code{SID}).
#'
#' @return A \code{data.table} named \strong{RMwTable}, or \code{NULL} when
#'         disaggregation is not available.
#'
#' @seealso
#'   \code{\link{buildAEPTable}}, \code{\link{buildUHSTable}},
#'   \code{\link{buildGMDP}}
#'
#' @keywords internal
#' @noRd
buildMwTable <- function(temp_dir, engine, ITo, vref) { ... }

buildMwTable <- function(temp_dir, engine, ITo, vref) {

  if (engine != "openquake") {
    message("> Disaggregation not requested for engine = '", engine, "'.")
    return(NULL)
  }

  message("> Building Disaggregation Hazard Table...")
  out <- tryCatch(
    importModel.oqRMw(path = temp_dir, ITo = ITo, vref = vref),
    error = function(e) {
      message(">> Skipping disagg (", e$message, ")")
      NULL
    }
  )

  if (!is.null(out))
    out[, `:=`(SID = Vs30toSID(vref), Vs30 = vref, SM = engine, IT = ITo)]
  else
    message("> Disaggregation data not available.")

  out
}
