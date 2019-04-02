# Created by Michael Cahana in mid Feb. 2019
# Determines address matches for leases

#===========
# inputs: 
#===========
# landtrac_tx
# coded_addresses.csv

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

leases <- 
	readRDS(file.path(rdir, 'leases', 'landtrac_tx.Rds')) %>% 
	st_set_geometry(NULL) %>% 
	as_tibble()
already_coded_addresses <- read_csv(file.path(ddir, 'address_backups', 
	'coded_addresses.csv')) 

#===========
# match addresses 
#===========

already_coded_addresses <-  pull(already_coded_addresses, address)

df <- 
	leases %>% 
	rename(address = grnte_ad, name = grnte_al) %>% 
	select(name, address) 

output_file <- file.path(ddir, 'matches', 'addresses', 
	'leases_address_matches.csv')

match_addresses(df, already_coded_addresses, output_file)