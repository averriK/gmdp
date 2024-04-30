
#' Title
#'
#' @param fontname String. Font name.
#' @param size Numeric. Font size.
#' @param border.color String. Border color.
#' @param x Data frame.
#'
#' @return flextable
#' @export renderTable
#'
#' @importFrom flextable flextable
#' @import data.table
#'
#' @examples
#'
#'

renderTable <- function(x,fontname="Helvetica",size=10,border.color = "gray"){

  flextable::use_df_printer()
  flextable::set_flextable_defaults( font.family=fontname,border.color = border.color)


  stopifnot(is.data.frame(x))
  x |> flextable::flextable() |>
    flextable::theme_vanilla() |>
    flextable::fontsize(size=size,part = "all") |>
    flextable::align(align="center",part="all") |>
    flextable::align(align="left",j=2) |>
    flextable::autofit(add_w = 0.1, add_h = 0.1, unit= "cm", hspans = "none",part="all") |>
    flextable::font(fontname = fontname,part="all") |>
    flextable::bold(part = "header")



}
