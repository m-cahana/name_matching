# Created by Michael Cahana in mid Feb. 2019
# Verifies certain name matches given an address match

#===========
# inputs: 
#===========
# landtrac_tx.Rds
# leases_pa_raw.Rds
# all_leases.Rds

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

tx_leases <- 
	readRDS(file.path(rdir, 'leases', 'landtrac_tx.Rds')) %>% 
	st_set_geometry(NULL) %>% 
	as_tibble()
pa_leases <- 
	readRDS(file.path(rdir, 'leases', 'leases_pa_raw.Rds')) %>% 
	st_set_geometry(NULL) %>% 
	as_tibble()
all_leases <- 
	readRDS(file.path(rdir, 'leases', 'all_leases.Rds'))

#===========
# verify name matches with addresses 
#===========

# texas 
name_matches <- read_csv(file.path(ddir, 'matches', 'names', 
	'leases_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 'addresses', 
	'leases_address_matches.csv'))
output_file <- file.path(vdir, 'tx_leases_matches.csv')
lease_count <- 
	tx_leases %>% 
	rename(name = grnte_al) %>% 
	mutate(name = str_replace_all(name, '\xc9', 'E')) %>% 
	count(name) 

pre_screen_names(name_matches, address_matches, lease_count, output_file)

# pennsylvania 
name_matches <- read_csv(file.path(ddir, 'matches', 'names', 
	'pa_leases_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 'addresses', 
	'pa_leases_address_matches.csv'))
output_file <- file.path(vdir, 'pa_leases_matches.csv')
lease_count <- 
	pa_leases %>% 
	rename(name = grnte_al) %>% 
	mutate(name = str_replace_all(name, '\xc9', 'E')) %>% 
	count(name) 

pre_screen_names(name_matches, address_matches, lease_count, output_file)

# all 
name_matches <- read_csv(file.path(ddir, 'matches', 'names', 
	'leases_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 'addresses', 
	'leases_address_matches.csv'))
output_file <- file.path(vdir, 'leases_matches.csv')
lease_count <- 
	all_leases %>% 
	rename(name = grnte_al) %>% 
	mutate(name = str_replace_all(name, '\xc9', 'E')) %>% 
	count(name) 

pre_screen_names(name_matches, address_matches, lease_count, output_file)