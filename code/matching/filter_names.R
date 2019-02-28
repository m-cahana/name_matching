# Modified by Michael Cahana in mid Feb. 2018
# Filters out name matches determined echoed in address matches
# To be called by master.csv

#===========
# inputs: 
#===========
# name_matches tibble
# address_matches tibble
# output_file string

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

#===========
# functions
#===========

filter_names <- function(name_matches, address_matches, output_file) {
	name_matches <- 
		name_matches %>% 
		left_join(address_matches, by=c('name','match')) %>% 
	    mutate(keep = if_else(!is.na(address), 1, as.double(NA)))

	write_csv(name_matches, output_file)
}