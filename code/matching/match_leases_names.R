# Created by Michael Cahana in mid Feb. 2018
# Determines name matches for leases

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

source(file.path(root, 'code', 'matching', 'functions', 'match_names.R'))

#===========
# data read in
#===========

leases <- 
	readRDS(file.path(rdir, 'leases', 'landtrac_tx.Rds')) %>% 
	st_set_geometry(NULL) %>% 
	as_tibble()

#===========
# match names 
#===========

df <- 
    leases %>% 
    count(grnte_al) %>% 
    rename(name = grnte_al) 

output_file <- file.path(ddir, 'matches', 'names', 'leases_name_matches.csv')

match_names(df, output_file)