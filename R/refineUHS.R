#' Refine Uniform Hazard Spectrum (UHS) Data
#'
#' @description
#' Interpolates or smooths UHS data using either spline or linear approximation methods.
#' Can operate in linear or logarithmic space.
#'
#' @param .x A data.table containing the UHS data
#' @param x_ID Character. Column name for the x-axis values (default: "Tn")
#' @param y_ID Character. Column name for the y-axis values (default: "Sa")
#' @param method Character. Interpolation method: "spline" or "approx" (default: "spline")
#' @param nout Integer. Number of output points (default: 100)
#' @param logspace Logical. Whether to perform interpolation in log space (default: FALSE)
#' @param spline_method Character. Method for spline interpolation (default: "monoH.FC")
#'
#' @return A data.table with refined UHS data containing the specified number of points
#'
#' @importFrom stats splinefun approx
#' @importFrom data.table data.table setnames
#'
#'
#' @export
refineUHS <- function(
    .x,
    x_ID = "Tn",
    y_ID = "Sa",
    method = c("spline", "approx"),
    nout = 100,
    logspace = FALSE,
    spline_method = "monoH.FC" #
    ) {
    method <- match.arg(method)
    x_vals <- .x[[x_ID]]
    y_vals <- .x[[y_ID]]

    if (logspace) {
        if (any(x_vals <= 0) || any(y_vals <= 0)) {
            stop("logspace=TRUE but found non-positive Tn or Sa.")
        }
        x_vals <- log(x_vals)
        y_vals <- log(y_vals)
    }
    x_seq <- seq(min(x_vals), max(x_vals), length.out = nout)
    if (method == "spline") {
        sp_fun <- stats::splinefun(x_vals, y_vals, method = spline_method)
        y_seq <- sp_fun(x_seq)
    }
    if (method == "approx") {
        approx_out <- stats::approx(x_vals, y_vals, xout = x_seq)
        x_seq <- approx_out$x
        y_seq <- approx_out$y
    }
    if (logspace) {
        x_seq <- exp(x_seq)
        y_seq <- exp(y_seq)
    }
    outDT <- data.table::data.table(x_seq, y_seq)
    data.table::setnames(outDT, c("x_seq", "y_seq"), c(x_ID, y_ID))
    return(outDT)
}
