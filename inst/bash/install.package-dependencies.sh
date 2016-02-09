#! /usr/bin/env Rscript
options(repos = structure(c(CRAN = "http://cran-mirror.cs.uu.nl/")))
packages <- c("Amelia", "jsonlite", "vars", "testthat", "roxygen2", "urca", "Rcpp")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
    install.packages(new.packages)
  } else {
  print('No new packages installed')
}
update.packages(lib.loc = Sys.getenv("R_LIBS_USER"), ask = FALSE)
