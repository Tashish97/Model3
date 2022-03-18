# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny","shinythemes","DT","tidyverse","knitr","corrplot","plotly")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
install.packages("rmarkdown",version="1.8")
invisible(sapply(my_packages, install_if_missing))

