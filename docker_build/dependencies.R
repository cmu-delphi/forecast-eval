# install.packages(c(), repo="http://cran.rstudio.com/")

# blogdown::install_hugo()

devtools::install_github("cmu-delphi/covidcast", ref = "main",
                         upgrade = 'never', 
                         subdir = "R-packages/covidcast")
