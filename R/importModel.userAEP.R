#' @title Import AEP values from a user-provided Excel workbook
#'
#' @description
#' Reads an Excel file (\code{AEP.xlsx} by default) with multiple sheets named like "p=..."
#' (e.g., "p=0.16", "p=mean"). Each sheet must have a column \code{Tn} and other columns
#' representing spectral accelerations or ground-motion levels. The function melts those
#' wide columns into two columns: \code{Sa}, \code{AEP}, and then computes \code{POE} and
#' \code{TR} from a default \code{IT=50} year assumption.
#'
#' @param path Path to the directory containing the file.
#' @param filename Name of the Excel file (default "AEP.xlsx").
#' @return A data.table with columns:
#' \itemize{
#'   \item \strong{Tn}: spectral period
#'   \item \strong{Sa}: ground-motion level (converted to numeric)
#'   \item \strong{AEP}: annual exceedance probability
#'   \item \strong{p}: the quantile or "mean" extracted from the sheet name
#'   \item \strong{POE, TR}: probability of exceedance, and return period
#'   \item \strong{IT}: investigation time (assumed 50 years)
#' }
#' @export importModel.userAEP
#' @import data.table
#' @importFrom stringr str_remove
#' @importFrom readxl excel_sheets read_xlsx
#'
#' @examples
#' \dontrun{
#' userAEP <- importModel.userAEP(path = "/some/folder", filename="AEP.xlsx")
#' }
importModel.userAEP <- function(path = NULL, filename = "AEP.xlsx") {
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`: must be a directory.")
  }

  file_xlsx <- file.path(path, filename)
  if (!file.exists(file_xlsx)) {
    stop("File not found: ", file_xlsx)
  }

  sheets_all <- readxl::excel_sheets(file_xlsx)
  # We expect sheet names like "p=0.16", "p=mean", etc.
  sheets_p <- grep(pattern = "^p=", sheets_all, value = TRUE)
  if (length(sheets_p) == 0) {
    stop("No sheets named 'p=...' found in: ", file_xlsx)
  }

  AT <- data.table()
  for (SHEET in sheets_p) {
    dt_sheet <- data.table::as.data.table(
      readxl::read_xlsx(file_xlsx, sheet = SHEET)
    )
    if (!("Tn" %in% names(dt_sheet))) {
      stop("Missing column 'Tn' in sheet '", SHEET, "'.")
    }

    # We'll assume all other columns besides Tn are "ground motion" columns
    id_var <- "Tn"
    measure_vars <- setdiff(names(dt_sheet), id_var)
    if (length(measure_vars) == 0) {
      warning("No measure columns in sheet '", SHEET, "'. Skipping.")
      next
    }

    # Reshape from wide to long
    aux <- melt(
      dt_sheet,
      id.vars = id_var,
      measure.vars = measure_vars,
      variable.name = "Sa",
      value.name    = "AEP"
    )

    # parse the "p" from the sheet name, e.g. "p=0.16" -> 0.16
    po <- stringr::str_remove(SHEET, "p=")
    if (po != "mean") {
      po_num <- suppressWarnings(as.numeric(po))
      if (!is.na(po_num)) {
        po <- po_num
      }
      # else it might remain a character if it's not numeric
    }

    # Filter out non-positive AEP
    aux <- aux[AEP > 0]
    # Convert Sa factor or string to numeric
    if (!is.numeric(aux$Sa)) {
      # attempt to parse numeric from the factor/character
      aux[, Sa := suppressWarnings(as.numeric(as.character(Sa)))]
    }

    aux[, p := po]
    aux[, IT := 50]   # default
    aux[, POE := 1 - exp(-IT * AEP)] # exact relationship
    aux[, TR := 1 / AEP]

    AT <- rbind(AT, aux, use.names = TRUE, fill = TRUE)
  }

  # reorder columns
  col_order <- c("Tn","Sa","AEP","p","POE","TR","IT")
  col_order <- intersect(col_order, names(AT))
  setcolorder(AT, col_order)

  return(AT[])
}
