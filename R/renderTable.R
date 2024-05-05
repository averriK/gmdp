
#' Title
#'
#' @param fontname String. Font name.
#' @param fontsize.header String Font size in px.
#' @param fontsize.body String Font size in px.
#' @param border.color String. Border color.
#' @param .x Data frame.
#'
#' @return gt object
#' @export renderTable
#'
#' @import gt
#' @import data.table
#'
#' @examples
#' ShearModelParameters |> renderTable()
#'

renderTable <- function(.x,fontname="Helvetica",fontsize.header="11px",fontsize.body="10px",border.color = "blue"){

  TABLE <- .x |> gt() |>
    tab_style(
      style = list(
        cell_text(size = fontsize.body),
        css = "pointer-events: none;"  # Disable hover effects
      ),
      locations = cells_body(rows = everything(), columns = everything())
    ) |>
    tab_style(
      style = list(
        cell_text(size = fontsize.header),
        cell_text(weight = "bold"),
        cell_borders(sides = "top", color = border.color, weight = "2px"),
        cell_borders(sides = "bottom", color = border.color, weight = "2px")
      ),
      locations = cells_column_labels()
    )


  return(TABLE)



}
