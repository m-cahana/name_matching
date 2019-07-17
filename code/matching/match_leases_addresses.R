# Created by Michael Cahana in mid Feb. 2019
# Determines address matches for leases

#===========
# inputs:
#===========
# coded_addresses.csv
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

source(file.path(root, 'code', 'functions', 'match_addresses.R'))

#===========
# data read in
#===========

leases_di <-
	readRDS(file.path(rdir, 'leases', 'leases_di.Rds')) %>%
	select(address = grnte_ad, name = grnte_al)

leases_jb <-
  readRDS(file.path(rdir, 'leases', 'leases_jb.Rds')) %>%
	filter(LesseeAddr != "") %>%
	mutate(address = str_to_upper(paste(LesseeAddr, LesseeCStZ)),
		name = str_to_upper(Lessee)) %>%
	select(name, address)

already_coded_addresses <- read_csv(file.path(ddir, 'address_backups',
	'coded_addresses.csv'))

#===========
# match addresses
#===========

already_coded_addresses <-  pull(already_coded_addresses, address)

# all
df <-
	rbind(leases_di, leases_jb) %>%
	distinct()

output_file <- file.path(ddir, 'matches', 'addresses',
	'leases_address_matches.csv')

match_addresses(df, already_coded_addresses, output_file)
