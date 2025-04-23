file.remove("NAMESPACE") |> suppressWarnings()
usethis::use_proprietary_license(copyright_holder = "Alejandro Verri Kozlowski")
devtools::check(document = TRUE,cran = TRUE)
remove.packages("gmdp") |> suppressWarnings()
devtools::install()

## PUSH MAIN
remotes::install_github("averriK/gmdp")

