file.remove("NAMESPACE") |> suppressWarnings()
# usethis::use_proprietary_license(copyright_holder = "Alejandro Verri Kozlowski")
devtools::check(document = TRUE,cran = TRUE,force_suggests=TRUE,vignettes = TRUE)
devtools::install(build_vignettes = TRUE, dependencies = TRUE,build = TRUE,upgrade = TRUE,force = TRUE)


## PUSH MAIN
# remotes::install_github("averriK/gmdp")

