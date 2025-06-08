#' Import User-Provided Annual Exceedance Probability Data
#'
#' @description
#' Imports annual exceedance probability data from a user-provided Excel file.
#'
#' @param path Character. Path to the directory containing the Excel file.
#' @param filename Character. Name of the Excel file. Default is "AEP.xlsx".
#'
#' @return A data.table containing the annual exceedance probability data.
#'
#' @importFrom data.table data.table as.data.table melt setcolorder
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom stringr str_remove
#'
#' @noRd
importModel.userAEP <- function(path = NULL, filename = "AEP.xlsx") {
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`: must be a directory.")
  }

  file_xlsx <- file.path(path, filename)
  if (!file.exists(file_xlsx)) {
    stop("File not found: ", file_xlsx)
  }

  sheets_all <- readxl::excel_sheets(file_xlsx)
  sheets_p <- grep(pattern = "^p=", sheets_all, value = TRUE)
  if (length(sheets_p) == 0) {
    stop("No sheets named 'p=...' found in: ", file_xlsx)
  }

  AT <- data.table()
  for (SHEET in sheets_p) {
    dt_sheet <- data.table::as.data.table(readxl::read_xlsx(file_xlsx, sheet = SHEET))
    if (!("Tn" %in% names(dt_sheet))) {
      stop("Missing column 'Tn' in sheet '", SHEET, "'.")
    }

    id_var <- "Tn"
    measure_vars <- setdiff(names(dt_sheet), id_var)
    if (length(measure_vars) == 0) {
      warning("No measure columns in sheet '", SHEET, "'. Skipping.")
      next
    }

    aux <- melt(
      dt_sheet,
      id.vars = id_var,
      measure.vars = measure_vars,
      variable.name = "Sa",
      value.name = "AEP",
      variable.factor = FALSE
    )
    aux[, Sa := as.character(Sa)]

    po <- stringr::str_remove(SHEET, "p=")
    if (po != "mean") {
      po_num <- suppressWarnings(as.numeric(po))
      if (!is.na(po_num)) {
        po <- po_num
      }
    }

    aux <- aux[AEP > 0]
    if (!is.numeric(aux$Sa)) {
      aux[, Sa := suppressWarnings(as.numeric(as.character(Sa)))]
    }

    aux[, p := po]
    aux[, IT := 50]
    aux[, POE := 1 - exp(-IT * AEP)]
    aux[, TR := 1 / AEP]

    AT <- rbind(AT, aux, use.names = TRUE, fill = TRUE)
  }

  col_order <- c("Tn", "Sa", "AEP", "p", "POE", "TR", "IT")
  col_order <- intersect(col_order, names(AT))
  setcolorder(AT, col_order)

  return(AT[])
}
