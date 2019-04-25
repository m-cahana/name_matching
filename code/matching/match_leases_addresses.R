# Created by Michael Cahana in mid Feb. 2019
# Determines address matches for leases

#===========
# inputs: 
#===========
# landtrac_tx.Rds
# coded_addresses.csv
# leases_pa_raw.Rds
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

source(file.path(root, 'code', 'functions', 'match_addresses.R'))

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
already_coded_addresses <- read_csv(file.path(ddir, 'address_backups', 
	'coded_addresses.csv')) 

#===========
# match addresses 
#===========

already_coded_addresses <-  pull(already_coded_addresses, address)

# texas
df <- 
	tx_leases %>% 
	rename(address = grnte_ad, name = grnte_al) %>% 
	select(name, address) 

output_file <- file.path(ddir, 'matches', 'addresses', 
	'leases_address_matches.csv')

match_addresses(df, already_coded_addresses, output_file)

# pennsylvania
df <- 
	pa_leases %>% 
	rename(address = grnte_ad, name = grnte_al) %>% 
	select(name, address) 

output_file <- file.path(ddir, 'matches', 'addresses', 
	'pa_leases_address_matches.csv')

match_addresses(df, already_coded_addresses, output_file)
