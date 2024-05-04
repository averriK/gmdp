
#' Title
#'
#' @param fontname String. Font name.
#' @param size Numeric. Font size.
#' @param border.color String. Border color.
#' @param x Data frame.
#' @param package String. Package name.
#'
#' @return table
#' @export renderTable
#'
#' @import flextable
#' @import gt
#' @import data.table
#'
#' @examples
#'
#'

renderTable <- function(x,fontname="Helvetica",size=10,border.color = "gray",package="gt"){

  # flextable::use_df_printer()
  if(package=="gt"){
    flextable::set_flextable_defaults( font.family=fontname,border.color = border.color)
    TABLE <- x |> flextable::flextable() |>
      flextable::theme_vanilla() |>
      flextable::fontsize(size=size,part = "all") |>
      flextable::align(align="center",part="all") |>
      flextable::align(align="left",j=2) |>
      flextable::autofit(add_w = 0.1, add_h = 0.1, unit= "cm", hspans = "none",part="all") |>
      flextable::font(fontname = fontname,part="all") |>
      flextable::bold(part = "header")
  }
  if(package=="gt"){
    TABLE <- x |> gt()
  }

  return(TABLE)



}
