# Created by Michael Cahana in mid Feb. 2019
# Determines name matches for leases

#===========
# inputs: 
#===========
# landtrac_tx.Rds
# leases_pa.Rds
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

source(file.path(root, 'code', 'functions', 'match_names.R'))

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
# match names 
#===========

# texas
df <- 
    tx_leases %>% 
    count(grnte_al) %>% 
    rename(name = grnte_al) 

output_file <- file.path(ddir, 'matches', 'names', 'tx_leases_name_matches.csv')

match_names(df, output_file)

# pennslvania 
df <- 
    pa_leases %>% 
    count(grnte_al) %>% 
    rename(name = grnte_al) 

output_file <- file.path(ddir, 'matches', 'names', 'pa_leases_name_matches.csv')

match_names(df, output_file)

# all
df <- 
    all_leases %>% 
    count(grnte_al) %>% 
    rename(name = grnte_al) 

output_file <- file.path(ddir, 'matches', 'names', 'leases_name_matches.csv')

match_names(df, output_file)


