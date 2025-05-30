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
#' @param q_AF Character. Quantile for amplification factors. Default is "mean".
#'
#' @return A list containing:
#' \itemize{
#'   \item AEPTable: Annual Exceedance Probability table
#'   \item UHSTable: Uniform Hazard Spectrum table
#'   \item AFmodel: Amplification Factor model
#'   \item RMwTable: Magnitude-Distance disaggregation table (if available)
#' }
#'
#' @importFrom data.table data.table setdiff colnames unique rbindlist
#' @importFrom utils unzip
#' @importFrom stats setNames
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
                      q_AF = "mean") {
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
            {
                RMwTable <- importModel.oqRMw(path = TEMP, ITo = ITo, vref = vref)
            },
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

    # (4) Merge Tn=0 => PGA
    message("> Merge Tn=0 => PGA into UHSTable ...")
    COLS <- setdiff(colnames(UHSTable), c("Sa", "Tn", "AEP", "POE"))
    UHSTable[, PGA := Sa[Tn == 0], by = COLS]

    AFmodel <- data.table::data.table()

    if (!is.null(vs30) && vref %in% c(760, 3000)) {
        for (Vs in vs30) {
            message(sprintf("> Fit UHS Site Response for Vs30 %.1f...", Vs))
            COLS <- setdiff(colnames(UHSTable), c("Sa", "PGA", "AEP", "POE"))
            AUX <- UHSTable[, fitModel.AF.TR(.SD, pga = PGA, q = q_AF, Tn = Tn, vs30 = Vs, vref = vref), by = COLS]
            AFmodel <- data.table::rbindlist(list(AFmodel, AUX), use.names = TRUE)

            message(sprintf("> Fit AEP Site Response for Vs30 %.1f...", Vs))
        }
        COLS <- colnames(UHSTable)[colnames(UHSTable) %in% colnames(AFmodel)]
        UHSTable <- AFmodel[UHSTable, on = COLS][, `:=`(Sa = AF * Sa, PGA = AF * PGA)] |> unique()
    }

    if (is.null(vs30)) {
        UHSTable[, `:=`(Vref = vref, Vs30 = vref, AF = 1, sdLnAF = 0)]
    }

    message("> Update UHSTable ...")

    UHSTable[, ID := IDo]
    message("> Update AEPTable ...")
    AEPTable[, `:=`(ID = IDo, Vref = vref, Vs30 = vref)]

    return(list(
        AEPTable = AEPTable,
        UHSTable = UHSTable,
        AFmodel  = AFmodel,
        RMwTable = RMwTable
    ))
}
