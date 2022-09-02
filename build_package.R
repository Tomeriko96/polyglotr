## Set project name
package_name <- basename(rstudioapi::getActiveProject())
## =============================================== ==============================
## Install and load the packages
build_packages <- c("devtools",
                    "usethis",
                    "renv")

## install the packages that are not installed yet
lapply(build_packages[which(!build_packages %in% installed.packages())],
       install.packages)


## load the packages
invisible(lapply(build_packages,
                 library,
                 character.only = TRUE))


## =============================================== ==============================
## Build the package


## Check if the package is correct and can be built.
devtools::document()
devtools::check(manual = T)
devtools::build_manual(path = ".")


## Increment the version number and check in that change in Git
usethis::use_version()

## Build the package, and release it to the correct folder
devtools::build(path = "../../../Downloads/")

## Make use of the development version again
usethis::use_dev_version()
