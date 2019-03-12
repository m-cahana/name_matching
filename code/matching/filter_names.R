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

filter_names <- function(name_matches, address_matches, count, output_file) {
	name_matches <- 
		name_matches %>% 
		left_join(address_matches, by=c('name','match')) %>% 
	    mutate(keep = if_else(!is.na(address), 1, as.double(NA))) %>% 
	    left_join(count, by='name') %>% 
	    left_join(count, by=c('match' = 'name')) %>% 
	    mutate(n.x = ifelse(duplicated(name), 0, n.x)) %>% 
	    mutate(n.y = ifelse(duplicated(match), 0, n.y)) %>% 
	    mutate(n.y = ifelse(match %in% .$name, 0, n.y)) %>% 
	    mutate(n = n.x + n.y) %>% 
	    arrange(desc(n)) %>% 
	    mutate(pct_coverage = cumsum(n)/sum(n))

	write_csv(name_matches, output_file)
}