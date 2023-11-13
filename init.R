# init.R

my_packages = c("shiny", "bslib", "curl", "ggplot2", "dplyr", "readr", "showtext")
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

install.packages("bslib_0.5.1.tar.gz", repos=NULL, type="source")

invisible(sapply(my_packages, install_if_missing))
