# Created by Michael Cahana in mid Feb. 2019
# Verifies certain name matches given an address match

#===========
# inputs:
#===========
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

leases_di <-
	readRDS(file.path(rdir, 'leases', 'leases_di.Rds')) %>%
	count(grnte_al) %>%
	rename(name = grnte_al)

leases_jb <-
  readRDS(file.path(rdir, 'leases', 'leases_jb.Rds')) %>%
	mutate(Lessee = str_to_upper(Lessee)) %>%
	count(Lessee) %>%
	rename(name = Lessee)

#===========
# verify name matches with addresses
#===========

# all
name_matches <- read_csv(file.path(ddir, 'matches', 'names',
	'leases_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 'addresses',
	'leases_address_matches.csv'))
output_file <- file.path(vdir, 'leases_matches.csv')
lease_count <-
	rbind(leases_di, leases_jb) %>%
	mutate(name = str_replace_all(name, '\xc9', 'E'))

pre_screen_names(name_matches, address_matches, lease_count, output_file)
