# Install necessary packages
# data shaping packages: tidyverse, fst, stringi, tictoc, stringdist, text2vec,
# readxl, fuzzyjoin, data.table, sf, googleway, igraph, openxlsx, lubridate,
# e1071, rpart, randomForest

packages <- c('tidyverse', 'fst', 'stringi', 'tictoc', 'stringdist', 'text2vec',
	'readxl', 'fuzzyjoin', 'data.table', 'sf', 'googleway', 'igraph',
	'openxlsx', 'lubridate', 'e1071', 'rpart', 'randomForest', 'Formula', 'grf')

check_install <- function(pkg){
	if(!(pkg %in% installed.packages())){
		install.packages(pkg, repos="http://cran.rstudio.com/")
		return(paste(pkg, " is now installed"))
	} else {
		return(paste(pkg, " is already installed"))
	}
}

lapply(packages, check_install)
