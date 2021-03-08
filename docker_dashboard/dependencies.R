# Tidyverse is installed in the base image	
# Other packages should be installed here	
	
install.packages("plotly", dependencies=TRUE, repos='http://cran.rstudio.com/')	
install.packages("shinyjs", dependencies=TRUE, repos='http://cran.rstudio.com/')	
install.packages("tsibble", dependencies=TRUE, repos='http://cran.rstudio.com/')	
install.packages("viridis", dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("aws.s3", dependencies=TRUE, repos='http://cran.rstudio.com/')
