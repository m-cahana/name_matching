# Created by Michael Cahana in mid Feb. 2018
# Verifies certain name matches given an address match

#===========
# inputs: 
#===========
# landtrac_tx

#===========
# needed libraries
#===========
library(tidyverse)
library(sf)

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

source(file.path(root, 'code', 'functions', 'pre_screen_names.R'))

#===========
# data read in
#===========

leases <- 
	readRDS(file.path(rdir, 'leases', 'landtrac_tx.Rds')) %>% 
	st_set_geometry(NULL) %>% 
	as_tibble()

#===========
# verify name matches with addresses 
#===========

name_matches <- read_csv(file.path(ddir, 'matches', 'names', 
	'leases_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 'addresses', 
	'leases_address_matches.csv'))
output_file <- file.path(vdir, 'leases_matches.csv')
lease_count <- 
	leases %>% 
	rename(name = grnte_al) %>% 
	count(name) 

pre_screen_names(name_matches, address_matches, lease_count, output_file)