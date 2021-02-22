# Tidyverse is installed in the base image
# Other packages should be installed here

install.packages("assertthat")
install.packages("optparse")
install.packages("doParallel")
install.packages("plotly")
install.packages("shinyjs")
install.packages("tsibble")
install.packages("viridis")

devtools::install_github("cmu-delphi/covidcast",ref = "evalcast-killcards",subdir = "R-packages/evalcast")
