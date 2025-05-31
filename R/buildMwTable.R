#' Internal helper for buildGMDP()
#' @keywords internal
#' @noRd


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
