# nolint start
# file: buildGMDP.R
#' Build Ground Motion Design Parameters
#'
#' @description
#' This function builds ground motion design parameters from hazard curves obtained from OpenQuake or user-provided data.
#'
#' @param IDo Character. Identifier for the GMDP. Usually "gmdp" or ""gem".
#' @param path Character. Path to the directory containing hazard data files.
#' @param engine Character. Source of hazard data, either "openquake" or "user". Default is "openquake".
#' @param vs30 Numeric vector. Target Vs30 values for site response analysis. If NULL, no site response is performed.
#' @param vref Numeric. Reference Vs30 value. Default is 760 m/s.
#' @param TRo Numeric vector. Target return periods for uniform hazard spectra. Default is seq(400,10000,by=25).
#' @param NS    integer ≥ 1 – Monte-Carlo samples per model (default 30).
#'
#' @return A list containing:
#' \itemize{
#'   \item AEPTable: Annual Exceedance Probability table
#'   \item UHSTable: Uniform Hazard Spectrum table
#'   \item AFmodel: Amplification Factor model
#'   \item RMwTable: Magnitude-Distance disaggregation table (if available)
#' }
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom utils unzip
#'
#' @examples
#' \dontrun{
#' # Example with OpenQuake data
#' result <- buildGMDP(
#'     path = "path/to/oq/output",
#'     vs30 = c(300, 760),
#'     TRo = seq(475, 2475, by = 100)
#' )
#' }
#'
#' @export
buildGMDP <- function(path,
                      IDo = "gmdp",
                      engine = "openquake",
                      vs30 = NULL,
                      vref = 760,
                      TRo = seq(400, 10000, by = 25),
                      NS=100) {
  message("> Build AEP Table...")
  AEPTable <- NULL

  if (engine == "openquake") {
    message("> Unzip OQ data ...")

    # Prepare a temp directory. We'll reuse it for each .zip (one at a time).
    TEMP <- tempdir()
    if (dir.exists(TEMP)) {
      unlink(TEMP, recursive = TRUE)
      TEMP <- tempdir()
    }

    FILES <- list.files(path, pattern = "\\.zip$", full.names = TRUE)
    if (!length(FILES)) {
      stop("No .zip files found in ", path)
    }

    for (.files in FILES) {
      message("> Import AEP data from openquake from: ", .files)
      tryCatch(
        {
          unlink(TEMP, recursive = TRUE)
          dir.create(TEMP, showWarnings = FALSE)

          utils::unzip(.files, junkpaths = TRUE, exdir = TEMP)

          # Attempt to parse hazard data inside TEMP
          AUX <- importModel.oqAEP(path = TEMP, vref = vref)

          if (is.null(AEPTable)) {
            AEPTable <- AUX
          } else {
            AEPTable <- data.table::rbindlist(list(AEPTable, AUX), use.names = TRUE, fill = TRUE)
          }
        },
        error = function(e) {
          message(">> Skipping file: ", .files)
          message("   Reason: ", e$message)
        }
      )
    }
  } else if (engine == "user") {
    message("> Unzip USER data ...")
    message("> Import AEP data from user ...")
    AEPTable <- tryCatch(
      {
        importModel.userAEP(path, filename = "AEP.xlsx")
      },
      error = function(e) {
        stop("Error in user-supplied AEP data: ", e$message)
      }
    )
  } else {
    stop("Unknown engine: ", engine)
  }

  # If we have nothing after processing all files, stop
  if (is.null(AEPTable) || !nrow(AEPTable)) {
    stop("No valid AEP data found after processing all files in path: ", path)
  }

  ITo <- unique(AEPTable$ITo)[1]

  # (2) Disagg
  RMwTable <- NULL
  message("> Building Disaggregation Hazard Table...")
  if (engine == "openquake") {
    message("> Import Disaggregation data from openquake...")
    tryCatch(
      {RMwTable <- importModel.oqRMw(path = TEMP, ITo = ITo, vref = vref)},
      error = function(e) {
        message(">> Skipping disagg data due to error: ", e$message)
      }
    )
  }
  if (!is.null(RMwTable)) {
    RMwTable[, `:=`(SID = Vs30toSID(vref), Vs30 = vref, SM = engine, IT = ITo)]
  } else {
    message("> Disaggregation data not available.")
  }

  # (3) Remesh
  message("> Re-mesh hazard data on a uniform TR grid (no param-fitting)...")
  COLS <- setdiff(colnames(AEPTable), c("Sa", "POE", "AEP", "TR"))
  UHSTable <- AEPTable[Tn != -1, remeshGroup(.SD, TRo), by = COLS]


  ## ------------------------------------------------------------------------
  ##  Site-response scaling with fitSaF()  (replaces previous block)
  ## ------------------------------------------------------------------------
  if (!is.null(vs30) && vref %in% c(760, 3000)) {

    message(sprintf(
      "> Fit UHS site response for Vs30 values: %s ...",
      paste(vs30, collapse = ", ")
    ))

    ## -- 1.  Compute SaF for every (TR, Vs30, Tn, p) ----------------------
    SaFTable <- newmark::fitSaF(
      uhs  = UHSTable[, .(TR, Sa, Tn, p)],   # minimal columns
      vs30 = vs30,                           # scalar OR vector
      NS   = NS,
      vref = vref
    )
    ## SaFTable cols: TR, Vs30, Vref, Tn, p, SaF

    ## -- 2.  Prepare UHSTable for a clean join ---------------------------
    ##        (drop the old Vs30 col — it is always vref here)
    UHSTable[, Vs30 := NULL]

    ## -- 3.  Join and update motions -------------------------------------
    by_cols <- c("TR", "Tn", "p")              # join keys
    data.table::setkeyv(UHSTable, by_cols)
    data.table::setkeyv(SaFTable,  by_cols)

    UHSTable <- SaFTable[UHSTable, allow.cartesian = TRUE][
      , `:=`(
        SaF = ifelse(Vs30 == vref, Sa, SaF),
        AF  = ifelse(Vs30 == vref, 1, SaF / Sa),
        Sa  = SaF,
        Vref = vref
      )
    ][]
  }

  ## ------------------------------------------------------------------------
  ##  Rock-reference shortcut  (unchanged logic)
  ## ------------------------------------------------------------------------
  if (is.null(vs30)) {
    UHSTable[, `:=`(Vref = vref, Vs30 = vref, AF = 1, SaF = Sa)]
  }


  message("> Update UHSTable ...")
  UHSTable[, ID := IDo]
  message("> Update AEPTable ...")
  AEPTable[, `:=`(ID = IDo, Vref = vref, Vs30 = vref)]

  return(list(
    AEPTable = AEPTable,
    UHSTable = UHSTable,
    RMwTable = RMwTable
  ))
}
