#' Title
#'
#' @param HEIGHT Integer. Height of the plot in pixels
#' @param LEGEND Boolean. Show legend
#' @param TIP String. Tooltip
#' @param CURVE data.table. Data for the curve
#' @param BAR data.table. Data for the bar
#' @param POINT data.table. Data for the point
#' @param COLUMN data.table. Data for the column
#' @param XT String. X axis title
#' @param YT String. Y axis title
#' @param COLORS Vector. Colors for the plot
#' @param TYPE String. Type of the plot
#' @param XLOG Boolean. Logarithmic scale for X axis
#' @param YLOG Boolean. Logarithmic scale for Y axis
#' @param XREV Boolean. Reverse X axis
#' @param YREV Boolean. Reverse Y axis
#' @param XMAX Double. Maximum value for X axis
#' @param YMAX Double. Maximum value for Y axis
#' @param XMIN Double. Minimum value for X axis
#' @param YMIN Double. Minimum value for Y axis
#' @param LAYOUT String. Layout of the legend
#' @param ALIGN String. Alignment of the legend
#' @param VALIGN String. Vertical alignment of the legend
#' @param LINE String. Line style
#' @param THEME String. Theme
#' @param SYMBOL String. Symbol
#'
#' @return Highchart. Highchart object
#' @export
#'
#' @examples
#' @importFrom highcharter highchart
#' @importFrom highcharter hc_add_series
#' @importFrom highcharter hc_theme_hcrt
#' @importFrom highcharter hc_add_theme
#' @importFrom highcharter hc_chart
#' @importFrom highcharter hc_colors
#' @importFrom highcharter hc_legend
#' @importFrom highcharter hc_size
#' @importFrom highcharter hc_xAxis
#' @importFrom highcharter hc_yAxis
#' @importFrom highcharter hc_tooltip
#' @importFrom highcharter hcaes
#' @importFrom grDevices hcl.colors
#'
#'
buildHCPlot <- function(
    HEIGHT=600,
    LEGEND=TRUE,
    TIP = "ID:{point.series.name}<br> X={point.x}<br> Y={point.y}",
    CURVE=NULL,  BAR=NULL,  POINT=NULL,COLUMN=NULL,
    XT="X []",YT="Y []",
    COLORS=hcl.colors(n= 5,palette = "ag_Sunset",alpha=0.6),
    TYPE="line",
    XLOG=TRUE,YLOG=FALSE,
    XREV=FALSE,YREV=FALSE,
    XMAX=NULL, YMAX=NULL,
    XMIN=NULL, YMIN=NULL,
    LAYOUT="horizontal",ALIGN="right",VALIGN="top",
    LINE="Solid",
    #c('Solid','ShortDash','ShortDot','ShortDashDot','ShortDashDotDot','Dot','Dash','LongDash','DashDot','LongDashDot','LongDashDotDot')
    THEME=hc_theme_hcrt(),
    SYMBOL="circle"){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  stopifnot(!is.null(POINT)|!is.null(CURVE)|!is.null(BAR)|!is.null(COLUMN))

  . <- .SD <- .N <- .I <- NULL


  HC <- highcharter::highchart()

  if(!is.null(CURVE)){
    HC <- HC |> hc_add_series(
      CURVE[,.(ID,X,Y)],# main curve
      type=TYPE,
      dashStyle = LINE,
      hcaes(x=X,y=Y,group=ID))


  }
  if(!is.null(BAR)){
    HC <- HC |> hc_add_series(
      BAR[,.(ID,X,Y)],# main curve
      type="bar",
      hcaes(x=X,y=Y,group=ID))

  }
  if(!is.null(COLUMN)){
    HC <- HC |> hc_add_series(
      COLUMN[,.(ID,X,Y)],# main curve
      type="column",
      hcaes(x=X,y=Y,group=ID))

  }
  if(!is.null(POINT)){
    HC <- HC |>  hc_add_series(
      POINT[,.(ID,X,Y)],# main curve
      type="scatter",
      marker=list(symbol=SYMBOL),
      hcaes(x=X,y=Y,group=ID))

  }


  HC <- HC |>
    hc_yAxis(
      title= list(text=YT),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = FALSE,
      showLastLabel = TRUE) |>

    hc_xAxis(
      title= list(text=XT),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = TRUE,
      showLastLabel = TRUE) |>

    hc_add_theme(hc_thm = THEME) |>

    hc_colors(
      # colors <- c("blue","brown","red")
      # colors = hcl.colors(12,palette = PALETTE)
      colors = COLORS
    ) |>

    hc_tooltip(
      sort = FALSE,
      split=FALSE,
      crosshairs = TRUE,
      pointFormat = TIP) |>

    hc_size( height = HEIGHT) |>
    hc_legend(enabled = LEGEND)

  if(LEGEND==TRUE){

    HC <- HC |>
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = LAYOUT,
        x = 100,
        y = 100
      )  |>
      hc_chart(
        style=list(fontFamily = "Helvetica"))
  }

  if(!is.null(XMAX)){
    HC <- HC |> hc_xAxis(max = XMAX)
  }
  if(!is.null(YMAX)){
    HC <- HC |> hc_yAxis(max = YMAX)
  }

  if(!is.null(XMIN)){
    HC <- HC |> hc_xAxis(min = XMIN)
  }
  if(!is.null(YMIN)){
    HC <- HC |> hc_yAxis(min = YMIN)
  }
  if(YLOG==TRUE) {
    HC <- HC |> hc_yAxis(type = "logarithmic")
  }
  if(XLOG==TRUE) {
    HC <- HC |> hc_xAxis(reversed=XREV,type = "logarithmic")
  } else {
    HC <- HC |> hc_xAxis(reversed=XREV)
  }

  if(YLOG==TRUE) {
    HC <- HC |> hc_yAxis(reversed=YREV,type = "logarithmic")
  } else {
    HC <- HC |> hc_yAxis(reversed=YREV)
  }
  return(HC)
}
