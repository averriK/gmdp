#' Build an Annual-Exceedance-Probability (AEP) table from OpenQuake or user data
#'
#' @description
#' Reads one or more OpenQuake ZIP archives — or a user-supplied
#' “AEP.xlsx” file — and merges the embedded hazard curves into a
#' single **AEPTable**.
#' When \code{engine = "openquake"} the ZIP files are extracted to a
#' disposable temporary directory that is returned so downstream
#' routines (e.g., disaggregation) can re-use the same files without
#' reopening the archives.
#'
#' @param path   \code{character}. Directory that contains the ZIP archives
#'               (OpenQuake) or the “AEP.xlsx” file (user engine).
#' @param engine \code{character}. Either \code{"openquake"} (default) or
#'               \code{"user"}.
#' @param vref   \code{numeric}. Reference site class velocity (m/s);
#'               passed through to \code{importModel.oqAEP()}.
#'
#' @return A named \code{list} with two elements
#'   \describe{
#'     \item{\strong{AEPTable}}{A \code{data.table} with the merged hazard curves.}
#'     \item{\strong{temp_dir}}{The temporary extraction folder used for
#'             OpenQuake input; \code{NULL} when \code{engine = "user"}.}
#'   }
#'
#' @seealso
#'   \code{\link{buildUHSTable}}, \code{\link{buildMwTable}},
#'   \code{\link{buildGMDP}}
#'
#' @keywords internal
#' @noRd


buildAEPTable <- function(path, engine = "openquake", vref = 760) {

  message("> Build AEP Table...")
  AEPTable <- NULL
  temp_dir <- NULL                     # will remain NULL for 'user' engine

  if (engine == "openquake") {
    message("> Unzip OQ data ...")

    temp_dir <- tempdir()
    if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

    FILES <- list.files(path, pattern = "\\.zip$", full.names = TRUE)
    if (!length(FILES))
      stop("No .zip files found in ", path)

    for (z in FILES) {
      message("> Import AEP data from: ", z)
      tryCatch({
        unlink(temp_dir, recursive = TRUE)
        dir.create(temp_dir, showWarnings = FALSE)
        utils::unzip(z, junkpaths = TRUE, exdir = temp_dir)

        AUX <- importModel.oqAEP(path = temp_dir, vref = vref)

        AEPTable <- if (is.null(AEPTable)) AUX
        else data.table::rbindlist(list(AEPTable, AUX), use.names = TRUE, fill = TRUE)
      },
      error = function(e) {
        message(">> Skipping file: ", z, " (", e$message, ")")
      })
    }
  } else if (engine == "user") {
    message("> Import user-supplied AEP.xlsx ...")
    AEPTable <- tryCatch(
      importModel.userAEP(path, filename = "AEP.xlsx"),
      error = function(e) stop("Error in user AEP data: ", e$message)
    )
  } else {
    stop("Unknown engine: ", engine)
  }

  if (is.null(AEPTable) || !nrow(AEPTable))
    stop("No valid AEP data found in path: ", path)

  list(AEPTable = AEPTable, temp_dir = temp_dir)
}
