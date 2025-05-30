
#' Build a Uniform-Hazard-Spectrum (UHS) table (with optional site-response scaling)
#'
#' @description
#' (1) Remeshes the input \code{AEPTable} onto a user-defined grid of
#' target return periods (\code{TRo}) to produce rock-reference UHS
#' ordinates;
#' (2) optionally applies stochastic site-response factors via
#' \code{newmark::fitSaF()} for one or several \code{Vs30} values.
#'
#' @param AEPTable \code{data.table}. Output of \code{buildAEPTable()}.\cr
#'                 Must include columns \code{TR}, \code{Sa}, \code{Tn},
#'                 and \code{p}.
#' @param vs30     \code{numeric} vector or \code{NULL}. Target Vs30s for
#'                 which to compute amplification factors.  If
#'                 \code{NULL}, the rock motions are simply duplicated
#'                 with \code{AF = 1}.
#' @param vref     \code{numeric}. Reference Vs30 (m/s) used in the hazard
#'                 calculation; must be 760 or 3000 for site response to
#'                 be applied.
#' @param TRo      \code{numeric} vector. Desired return-period grid
#'                 (years) for the remeshing step.
#' @param NS       \code{integer} (≥ 1). Number of Monte-Carlo samples
#'                 per Vs30 in the stochastic site-response model.
#'
#' @return A \code{data.table} named \strong{UHSTable} with columns
#'   \itemize{
#'     \item \code{TR}, \code{Tn}, \code{p}, \code{Sa}  – UHS ordinates
#'     \item \code{Vs30}, \code{Vref}                    – site velocities
#'     \item \code{AF},  \code{SaF}                     – amplification factors
#'   }
#'
#' @seealso
#'   \code{\link{newmark::fitSaF}}, \code{\link{buildAEPTable}},
#'   \code{\link{buildMwTable}}, \code{\link{buildGMDP}}
#'
#' @keywords internal
#' @noRd
buildUHSTable <- function(AEPTable,
                          vs30 = NULL,
                          vref = 760,
                          TRo  = seq(400, 10000, by = 25),
                          NS   = 100) { ... }

buildUHSTable <- function(AEPTable,
                          vs30 = NULL,
                          vref = 760,
                          TRo  = seq(400, 10000, by = 25),
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
