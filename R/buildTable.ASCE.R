#' Title
#'
#' @param units c("m","m/s","ft","ft/s")
#' @param engine c("flextable","kableExtra","gtable")
#' @param size Font Size
#'
#' @return data.table
#' @export
#'
#' @examples
#' buildTable.ASCE()
#' buildTable.ASCE(units="m/s")
#' buildTable.ASCE(units="m/s",engine="flextable")
#' buildTable.ASCE(units="m/s",size=8)
buildTable.ASCE <- function(units="m",engine="flextable",size=12){
  if(tolower(units) %in%  c("m","m/s")){
    DT <- data.table::data.table(
      SC=c("A","B","BC","C","CD","D","DE","E","F"),
      Description=c(
        "Hard rock",#A,>1500 m/s
        "Medium hard rock",#B,910 to 1500 m/s
        "Soft rock",#BC 640 to 910 m/s
        "Very dense soil or hard clay",#C 440 to 640 m/s
        "Dense sand of very stiff clay", #CD 300 to 440 m/s
        "Medium dense sand or stiff clay",#D 210 to 300 m/s
        "Loose sand or medium stiff clay",#DE 150 to 210 m/s
        "Very loose sand or soft clay",#E
        "Soils requiring site response analysis (ASCE/SEI 7-22 21.1)"),#F

      "Vs30"=c(
        ">1500 [m/s]",
        ">910-1500 [m/s]",
        ">640-910 [m/s]",
        ">440-640 [m/s]",
        ">300-440 [m/s]",
        ">210-300 [m/s]",
        ">150-210 [m/s]",
        "<=150 [m/s]",
        "*See ASCE 7 section 20.2.1"))
  }

  if(tolower(units) %in% c("ft","ft/s")){

    DT <- data.table::data.table(
      SC=c("A","B","BC","C","CD","D","DE","E","F"),
      Description=c(
        "Hard rock",#A,>5000
        "Medium hard rock",#B,910 to 1500 m/s
        "Soft rock",#BC 640 to 910 m/s
        "Very dense soil or hard clay",#C 440 to 640 m/s
        "Dense sand of very stiff clay", #CD 300 to 440 m/s
        "Medium dense sand or stiff clay",#D 210 to 300 m/s
        "Loose sand or medium stiff clay",#DE 150 to 210 m/s
        "Very loose sand or soft clay",#E
        "Soils requiring site response analysis (ASCE/SEI 7-22 21.1)"),#F

      "Vs30"=c(
        ">5000 [ft/s]",
        ">3000-5000 [ft/s]",
        ">2100-3000 [ft/s]",
        ">1450-2100 [ft/s]",
        ">1000-2100 [ft/s]",
        ">700-1000 [ft/s]",
        ">500-7000 [ft/s]",
        "<=500 [ft/s]","*See ASCE 7 section 20.2.1"))
  }
  return(DT)
}
