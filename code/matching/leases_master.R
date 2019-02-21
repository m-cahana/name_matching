# Created by Michael Cahana in mid Feb. 2018
# Determines name matches given name and address inputs

#===========
# inputs: 
#===========
# landtrac_tx
# coded_addresses.csv


#===========
# needed libraries
#===========
library(tidyverse)
library(fst)
library(stringi)
library(readxl)
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

source(file.path(root, 'code', 'matching', 'match_names.R'))
source(file.path(root, 'code', 'matching', 'match_addresses.R'))
source(file.path(root, 'code', 'matching', 'filter_names.R'))

#===========
# data read in
#===========

leases <- readRDS(file.path(rdir, 'leases', 'landtrac_tx.Rds'))
already_coded_addresses <- read_csv(file.path(ddir, 'coded_addresses.csv'))

#===========
# match names within modeled
#===========

df <- 
    leases %>% 
    st_set_geometry(NULL) %>% 
    count(grnte_al) %>% 
    rename(name = grnte_al)

output_file <- file.path(ddir, 'matches', 'leases_name_matches.csv')

match_names(df, output_file)

#===========
# match addresses within modeled
#===========

already_coded_addresses <-  pull(already_coded_addresses, address)

df <- 
	leases %>% 
	st_set_geometry(NULL) %>% 
	rename(address = grnte_ad, name = grnte_al) %>% 
	select(name, address) 

output_file <- file.path(ddir, 'matches', 'leases_address_matches.csv')

match_addresses(df, already_coded_addresses, output_file)

#===========
# filter down name match list
#===========

name_matches <- read_csv(file.path(ddir, 'matches', 'leases_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 
	'leases_address_matches.csv'))
output_file <- file.path(vdir, 'leases_name_matches.csv')

filter_names(name_matches, address_matches, output_file)