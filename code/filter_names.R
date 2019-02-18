# Modified by Michael Cahana in mid Feb. 2018
# Filters out name matches determined echoed in address matches
# To be called by master.csv

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

#===========
# find double matches
#===========

double_matches <- 
	name_matches %>% 
	inner_join(address_matches, by=c('name','match')) %>% 
	select(-c(method.x, method.y, score)) %>% 
    mutate(keep = 1)

#===========
# remove doubles from name matches
#===========

name_matches <- 
	name_matches %>% 
	left_join(double_matches, by=c('name', 'match'))

write_csv(name_matches, output_file)