file.remove("NAMESPACE") |> suppressWarnings()
source("data-raw/sysdata.R")
usethis::use_proprietary_license(copyright_holder = "Alejandro Verri Kozlowski")
devtools::document()
devtools::check()
remove.packages("gmdp")
# devtools::install()
## PUSH MAIN
remotes::install_github("averriK/gmdp")

